use crate::error::Result;
use serde::ser;
use std::io::Write;

use crate::error::Error;

pub struct ParityCompatibleSerializer<W>
where
    W: Write,
{
    writer: W,
}

pub struct InnerSerializer<'a, W>
where
    W: Write,
{
    child: ParityCompatibleSerializer<Vec<u8>>,
    parent: &'a mut ParityCompatibleSerializer<W>,
}

impl<'a, W> InnerSerializer<'a, W>
where
    W: Write,
{
    fn new(parent: &'a mut ParityCompatibleSerializer<W>) -> InnerSerializer<W> {
        let writer = <Vec<u8>>::new();
        let child = ParityCompatibleSerializer { writer };

        InnerSerializer { child, parent }
    }

    fn end(self) -> Result<()> {
        encode_variable_length(&mut self.parent.writer, self.child.writer.as_slice(), 0xc0)?;
        Ok(())
    }
}

macro_rules! not_supported {
    ($fn_name: ident, $type: ty, $err: path) => {
        fn $fn_name(self, _v: $type) -> Result<()> {
            Err($err)
        }
    };
}

// Generates the serialization function for unsigned integers.
macro_rules! serialize_u {
    ($fn_name: ident, $type: ty) => {
        fn $fn_name(self, v: $type) -> Result<()> {
            match v {
                // Parity requires the value 0 of the types u8, u16, u32, and u64 to be represented
                // as `0x80`. Representing 0 with `0x00` is forbidden, bug? (Maybe it happened
                // because prunning of leading zeros is enforced)
                0x00 => {
                    self.writer.write(&[0x80])?;
                },
                0x01..=0x7f => {
                    // debug_assert!(
                    //     v <= 255,
                    //     "v must fit in a u8, otherwise it can not be safely cast."
                    // );

                    self.writer.write(&[v as u8])?;
                }
                _ => {
                    // At most 16 bytes are necessary (for an u128), which falls well below the 55
                    // encoding.
                    let leading_empty_bytes = v.leading_zeros() as usize / 8;
                    let encoded_number = v.to_be_bytes();
                    let encoded_length = encoded_number.len();

                    debug_assert!(
                        encoded_length <= 55,
                        "v must fit in 55 bytes after encoded, otherwise it can not be encoded here."
                    );
                    debug_assert!(
                        encoded_length <= 255,
                        "v must fit in a u8, otherwise it can not be safely cast."
                    );

                    self.writer.write(&[0x80 + encoded_length as u8])?;
                    self.writer.write(&encoded_number[leading_empty_bytes..])?;
                }
            };
            Ok(())
        }
    };
}

fn encode_variable_length<W>(writer: &mut W, bytes: &[u8], base: u8) -> Result<()>
where
    W: Write,
{
    let bytes_len = bytes.len();

    if bytes_len <= 55 {
        debug_assert!(
            bytes_len <= 255,
            "length must fit in a u8, otherwise it can not be safely cast."
        );

        writer.write(&[base + bytes_len as u8])?;
        writer.write(bytes)?;
    } else {
        let length_leading_zeros = bytes_len.leading_zeros() as usize / 8;
        let length_encoded = &bytes_len.to_be_bytes()[length_leading_zeros..];
        let length_of_length = length_encoded.len();

        if length_of_length <= 7 {
            debug_assert!(
                length_of_length <= 255,
                "length must fit in a u8, otherwise it can not be safely cast."
            );

            writer.write(&[0xb7 + length_of_length as u8])?;
            writer.write(&length_encoded)?;
            writer.write(bytes)?;
        } else {
            return Err(Error::SerializeBytesIsTooBig);
        }
    }

    Ok(())
}

impl<'a, W> ser::SerializeStructVariant for InnerSerializer<'a, W>
where
    W: Write,
{
    type Ok = ();
    type Error = Error;

    #[inline]
    fn serialize_field<T: ?Sized>(&mut self, _key: &'static str, value: &T) -> Result<()>
    where
        T: serde::ser::Serialize,
    {
        value.serialize(&mut self.child)
    }

    #[inline]
    fn end(self) -> Result<()> {
        self.end()
    }
}

impl<'a, W> ser::SerializeStruct for InnerSerializer<'a, W>
where
    W: Write,
{
    type Ok = ();
    type Error = Error;

    #[inline]
    fn serialize_field<T: ?Sized>(&mut self, _key: &'static str, value: &T) -> Result<()>
    where
        T: serde::ser::Serialize,
    {
        value.serialize(&mut self.child)
    }

    #[inline]
    fn end(self) -> Result<()> {
        self.end()
    }
}

impl<'a, W> ser::SerializeMap for InnerSerializer<'a, W>
where
    W: Write,
{
    type Ok = ();
    type Error = Error;

    #[inline]
    fn serialize_key<K: ?Sized>(&mut self, value: &K) -> Result<()>
    where
        K: serde::ser::Serialize,
    {
        value.serialize(&mut self.child)
    }

    #[inline]
    fn serialize_value<V: ?Sized>(&mut self, value: &V) -> Result<()>
    where
        V: serde::ser::Serialize,
    {
        value.serialize(&mut self.child)
    }

    #[inline]
    fn end(self) -> Result<()> {
        self.end()
    }
}

impl<'a, W> ser::SerializeTupleVariant for InnerSerializer<'a, W>
where
    W: Write,
{
    type Ok = ();
    type Error = Error;

    #[inline]
    fn serialize_field<T: ?Sized>(&mut self, value: &T) -> Result<()>
    where
        T: serde::ser::Serialize,
    {
        value.serialize(&mut self.child)
    }

    #[inline]
    fn end(self) -> Result<()> {
        self.end()
    }
}

impl<'a, W> ser::SerializeTuple for InnerSerializer<'a, W>
where
    W: Write,
{
    type Ok = ();
    type Error = Error;

    #[inline]
    fn serialize_element<T: ?Sized>(&mut self, value: &T) -> Result<()>
    where
        T: serde::ser::Serialize,
    {
        value.serialize(&mut self.child)
    }

    #[inline]
    fn end(self) -> Result<()> {
        self.end()
    }
}

impl<'a, W> ser::SerializeTupleStruct for InnerSerializer<'a, W>
where
    W: Write,
{
    type Ok = ();
    type Error = Error;

    #[inline]
    fn serialize_field<T: ?Sized>(&mut self, value: &T) -> Result<()>
    where
        T: serde::ser::Serialize,
    {
        value.serialize(&mut self.child)
    }

    #[inline]
    fn end(self) -> Result<()> {
        self.end()
    }
}

impl<'a, W> ser::SerializeSeq for InnerSerializer<'a, W>
where
    W: Write,
{
    type Ok = ();
    type Error = Error;

    #[inline]
    fn serialize_element<T: ?Sized>(&mut self, value: &T) -> Result<()>
    where
        T: serde::ser::Serialize,
    {
        value.serialize(&mut self.child)
    }

    #[inline]
    fn end(self) -> Result<()> {
        self.end()
    }
}

impl<'a, W> ser::Serializer for &'a mut ParityCompatibleSerializer<W>
where
    W: Write,
{
    type Ok = ();
    type Error = Error;
    type SerializeSeq = InnerSerializer<'a, W>;
    type SerializeTuple = InnerSerializer<'a, W>;
    type SerializeTupleStruct = InnerSerializer<'a, W>;
    type SerializeTupleVariant = InnerSerializer<'a, W>;
    type SerializeMap = InnerSerializer<'a, W>;
    type SerializeStruct = InnerSerializer<'a, W>;
    type SerializeStructVariant = InnerSerializer<'a, W>;

    // Parity does not have a default impl for signed integers, so here it is assumed the Ethereum
    // protocols don't use it (if this assumption is wrong the code has to be revisted for
    // compatibility).
    not_supported!(serialize_i8, i8, Error::SerializeSignedIntegerNotSupported);
    not_supported!(
        serialize_i16,
        i16,
        Error::SerializeSignedIntegerNotSupported
    );
    not_supported!(
        serialize_i32,
        i32,
        Error::SerializeSignedIntegerNotSupported
    );
    not_supported!(
        serialize_i64,
        i64,
        Error::SerializeSignedIntegerNotSupported
    );

    // Parity does not support floats out-of-the-box.
    not_supported!(serialize_f32, f32, Error::SerializeFloatNotSupported);
    not_supported!(serialize_f64, f64, Error::SerializeFloatNotSupported);

    fn serialize_unit(self) -> Result<()> {
        Ok(())
    }

    fn serialize_unit_struct(self, _: &'static str) -> Result<()> {
        Ok(())
    }

    fn serialize_bool(self, v: bool) -> Result<()> {
        self.writer.write(&[v as u8])?;
        Ok(())
    }

    serialize_u!(serialize_u8, u8);
    serialize_u!(serialize_u16, u16);
    serialize_u!(serialize_u32, u32);
    serialize_u!(serialize_u64, u64);

    fn serialize_char(self, c: char) -> Result<()> {
        let mut buffer = [0x00; 4];

        // result may be lower than 4 bytes
        let encoded_char = c.encode_utf8(&mut buffer);
        let data = encoded_char.as_bytes();

        self.serialize_bytes(data)?;
        Ok(())
    }

    fn serialize_str(self, v: &str) -> Result<()> {
        self.serialize_bytes(v.as_bytes())?;
        Ok(())
    }

    fn serialize_bytes(self, bytes: &[u8]) -> Result<()> {
        let bytes_len = bytes.len();

        if bytes_len == 1 && bytes[0] < 0x80 {
            self.writer.write(bytes)?;
            Ok(())
        } else {
            encode_variable_length(&mut self.writer, bytes, 0x80)
        }
    }

    fn serialize_none(self) -> Result<()> {
        self.writer.write(&[0xc0])?;
        Ok(())
    }

    fn serialize_some<T: ?Sized>(self, v: &T) -> Result<()>
    where
        T: serde::Serialize,
    {
        self.writer.write(&[0xc1])?;
        v.serialize(self)
    }

    fn serialize_unit_variant(
        self,
        _name: &'static str,
        variant_index: u32,
        _variant: &'static str,
    ) -> Result<()> {
        self.serialize_u32(variant_index)
    }

    fn serialize_seq(self, _len: Option<usize>) -> Result<Self::SerializeSeq> {
        Ok(InnerSerializer::new(self))
    }

    fn serialize_tuple(self, _len: usize) -> Result<Self::SerializeTuple> {
        Ok(InnerSerializer::new(self))
    }

    fn serialize_tuple_struct(
        self,
        _name: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeTupleStruct> {
        Ok(InnerSerializer::new(self))
    }

    fn serialize_tuple_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        _variant: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeTupleVariant> {
        Ok(InnerSerializer::new(self))
    }

    fn serialize_map(self, _len: Option<usize>) -> Result<Self::SerializeMap> {
        Ok(InnerSerializer::new(self))
    }

    fn serialize_struct(self, _name: &'static str, _len: usize) -> Result<Self::SerializeStruct> {
        Ok(InnerSerializer::new(self))
    }

    fn serialize_struct_variant(
        self,
        _name: &'static str,
        variant_index: u32,
        _variant: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeStructVariant> {
        self.serialize_u32(variant_index)?;
        Ok(InnerSerializer::new(self))
    }

    fn serialize_newtype_struct<T: ?Sized>(self, _name: &'static str, value: &T) -> Result<()>
    where
        T: serde::ser::Serialize,
    {
        value.serialize(self)
    }

    fn serialize_newtype_variant<T: ?Sized>(
        self,
        _name: &'static str,
        variant_index: u32,
        _variant: &'static str,
        value: &T,
    ) -> Result<()>
    where
        T: serde::ser::Serialize,
    {
        self.serialize_u32(variant_index)?;
        value.serialize(self)
    }
}

pub fn to_bytes_parity_compatible<T: ?Sized, W>(value: &T, writer: W) -> Result<()>
where
    T: ser::Serialize,
    W: Write,
{
    let mut serializer = ParityCompatibleSerializer { writer };
    value.serialize(&mut serializer)?;
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use rlp;

    macro_rules! test_min_max_for_u {
        ($name: ident, $type: ty) => {
            #[test]
            fn $name() {
                let values = [<$type>::MIN, <$type>::MAX];

                for v in &values {
                    let parity_result = rlp::encode(v);
                    let mut writer = <Vec<u8>>::new();
                    assert!(to_bytes_parity_compatible(v, &mut writer).is_ok());
                    assert_eq!(parity_result, writer);
                }
            }
        };
    }

    macro_rules! test_range {
        ($name: ident, $type: ty,$start: literal, $end: literal) => {
            #[test]
            fn $name() {
                for value in $start..=$end {
                    let parity_result = rlp::encode(&value);
                    let mut writer = <Vec<u8>>::new();
                    assert!(to_bytes_parity_compatible(&value, &mut writer).is_ok());

                    assert_eq!(
                        parity_result, writer,
                        "mismatch while encoding the value {:?}",
                        value
                    );
                }
            }
        };
    }

    test_min_max_for_u!(test_encoding_u16, u16);
    test_min_max_for_u!(test_encoding_u32, u32);
    test_min_max_for_u!(test_encoding_u64, u64);

    test_range!(test_encoding_single_byte_u8, u8, 0x00u8, 0xffu8);
    test_range!(test_encoding_single_byte_u16, u16, 0x00u8, 0xffu8);
    test_range!(test_encoding_single_byte_u32, u32, 0x00u8, 0xffu8);
    test_range!(test_encoding_single_byte_u64, u64, 0x00u8, 0xffu8);
    test_range!(test_encoding_single_byte_bool, bool, 0x00u8, 0xffu8);

    // // Vec<u8> is incompatible, wrapping it in a DirectBytes is necessary to get the correct
    // // behavior!
    // #[test]
    // fn test_decoding_vec_of_u8() {
    //     for byte in 0x00..=0xff {
    //         let bytes = &[byte];

    //         let parity_result: Result<Vec<u8>, _> = rlp::decode(bytes);
    //         let result: DecodingResult<DirectBytes> = from_bytes_parity_compatible(bytes);
    //         let results = (parity_result, result);

    //         match results {
    //             (Ok(v1), Ok(v2)) => assert_eq!(v1, v2.0),
    //             (Err(_v1), Err(_v2)) => {}
    //             (Ok(v1), Err(v2)) => {
    //                 assert!(false, "parity result {:?}, library error {:?}", v1, v2)
    //             }
    //             (Err(v1), Ok(v2)) => {
    //                 assert!(false, "parity error {:?}, library result {:?}", v1, v2)
    //             }
    //         }
    //     }

    //     let values = [
    //         "",
    //         "a",
    //         "cats",
    //         "dogs",
    //         "This is a string with more than 55 bytes to test decoding of long lists",
    //     ];

    //     for v in &values {
    //         let encoded = rlp::encode(v);
    //         let result: String = from_bytes_parity_compatible(encoded.as_slice()).unwrap();
    //         assert_eq!(&result, v);
    //     }
    // }

    #[test]
    fn test_encoding_string() {
        let values = [
            "".to_string(),
            "a".to_string(),
            "cats".to_string(),
            "dogs".to_string(),
            "This is a string with more than 55 bytes to test decoding of long lists".to_string(),
        ];

        for v in &values {
            let parity_encoded = rlp::encode(v);
            let mut writer = <Vec<u8>>::new();

            assert!(to_bytes_parity_compatible(v, &mut writer).is_ok());

            assert_eq!(parity_encoded, writer);
        }
    }
}
