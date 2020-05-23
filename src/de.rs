use core::fmt;
use core::mem;
use serde::de;
use std::convert::TryFrom;
use std::str;

use crate::error::{Error, Result as DecodingResult};

pub fn from_bytes_parity_compatible<'a, T>(bytes: &'a [u8]) -> DecodingResult<T>
where
    T: serde::de::Deserialize<'a>,
{
    let mut deserializer = ParityCompatibleDeserializer::from_bytes(bytes);
    let t = T::deserialize(&mut deserializer)?;

    if deserializer.bytes.is_empty() {
        Ok(t)
    } else {
        Err(Error::TrailingCharacters)
    }
}

// This function is a copy of core::str::utf8_char_width
static UTF8_CHAR_WIDTH: [u8; 256] = [
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, // 0x1F
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, // 0x3F
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, // 0x5F
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, // 0x7F
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, // 0x9F
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, // 0xBF
    0, 0, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
    2, // 0xDF
    3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, // 0xEF
    4, 4, 4, 4, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, // 0xFF
];

macro_rules! not_supported {
    ($fn_name: ident, $err: path) => {
        fn $fn_name<V>(self, _visitor: V) -> DecodingResult<V::Value>
        where
            V: de::Visitor<'de>,
        {
            Err($err)
        }
    };
}

// Generates the deserialize function for usigned integers.
macro_rules! deserialize_u {
    ($fn_name: ident, $visitor_fn: ident, $type: ty) => {
        fn $fn_name<V>(self, visitor: V) -> DecodingResult<V::Value>
        where
            V: de::Visitor<'de>,
        {
            let buffer_size = self.bytes.len();

            if buffer_size == 0 {
                return Err(Error::UnexpectedEof);
            }

            match self.bytes[0] {
                // Parity requires the value 0 of the types u8, u16, u32, and u64 to be represented
                // as `0x80`. Representing 0 with `0x00` is forbidden, bug? (Maybe it happened
                // because prunning of leading zeros is enforced)
                0x00 => return Err(Error::InvalidIndirection),
                v @ 0x01..=0x7f => {
                    self.bytes = &self.bytes[1..];
                    visitor.$visitor_fn(v as $type)
                }
                // For the types u8, u16, u32, and u64 the empty string is used to represent 0.
                0x80 => {
                    self.bytes = &self.bytes[1..];
                    visitor.$visitor_fn(0 as $type)
                }
                0x81 => {
                    if buffer_size < 2 {
                        return Err(Error::UnexpectedEof);
                    }

                    let value = self.bytes[1];

                    // Parity rejects the un-optimized encodings [0x8100,0x817f] which saves one
                    // byte.
                    if value < 0x80 {
                        return Err(Error::InvalidIndirection);
                    }

                    self.bytes = &self.bytes[2..];
                    visitor.$visitor_fn(value as $type)
                }
                v @ 0x82..=0xb7 => {
                    let length = v as usize - 0x80;
                    const TYPE_SIZE_IN_BYTES: usize = mem::size_of::<$type>();

                    if length <= TYPE_SIZE_IN_BYTES {
                        // +1 to account for the first byte describing the encoded length.
                        let necessary_size = length + 1;

                        if buffer_size < necessary_size {
                            return Err(Error::UnexpectedEof);
                        }

                        // Parity requires prunning of leading zeros
                        if self.bytes[1] == 0x00 {
                            return Err(Error::InvalidIndirection);
                        }

                        let mut value = 0 as $type;

                        // Skipping 0 since that is the type+length
                        //
                        // `<$type>::from_be_bytes` can not be used here because the slice has
                        // variable length.
                        //
                        // This works fine since the data is encoded in network-order
                        //
                        // Unwrapping is safe since we are iterating over at most the number of
                        // bytes for the given itenger type.
                        for pos in 1..necessary_size {
                            value = value.checked_shl(8).unwrap() | self.bytes[pos] as $type;
                        }

                        self.bytes = &self.bytes[necessary_size..];

                        visitor.$visitor_fn(value)
                    } else {
                        Err(Error::Message(format!(
                            "Numbers cannot be represented with stings of {} bytes.",
                            length
                        )))
                    }
                }
                0xb8..=0xbf => Err(Error::Message(
                    "Numbers cannot be represented with more than 55 bytes".to_string(),
                )),
                // catch-all for lists
                0xc0..=0xff => Err(Error::Message(
                    "Numbers cannot be represented with lists".to_owned(),
                )),
            }
        }
    };
}

// Generates the deserialize function for stings
macro_rules! deserialize_str {
    ($fn_name: ident, $visitor_fn: ident) => {
        fn $fn_name<V>(self, visitor: V) -> DecodingResult<V::Value>
        where
            V: de::Visitor<'de>,
        {
            let buffer_size = self.bytes.len();
            if buffer_size == 0 {
                return Err(Error::UnexpectedEof);
            }

            match self.bytes[0] {
                v @ 0x00..=0x7f => {
                    self.bytes = &self.bytes[1..];

                    let c = v as char; // check deserialize_char
                    visitor.visit_str(&c.to_string())
                }
                0x80 => {
                    self.bytes = &self.bytes[1..];
                    visitor.visit_str("")
                }
                0x81 => invalid_one_byte_char::<V>(self.bytes),
                v @ 0x82..=0xb7 => {
                    let slice_size = v as usize - 0x80;
                    let necessary_size = slice_size + 1;

                    if buffer_size < necessary_size {
                        return Err(Error::UnexpectedEof);
                    }

                    match str::from_utf8(&self.bytes[1..necessary_size]) {
                        Ok(res) => {
                            self.bytes = &self.bytes[necessary_size..];
                            visitor.visit_str(res)
                        }
                        Err(_) => Err(Error::InvalidUTF8),
                    }
                }
                0xb8..=0xbf => decode_with_indirect_length::<V, _>(self, 0xb7, |inner_bytes| {
                    match str::from_utf8(inner_bytes) {
                        Ok(res) => visitor.visit_str(res),
                        Err(_) => Err(Error::InvalidUTF8),
                    }
                }),
                // catch-all for lists
                0xc0..=0xff => Err(Error::Message(
                    "Strings cannot be represented with lists.".to_string(),
                )),
            }
        }
    };
}

macro_rules! deserialize_b {
    ($fn_name: ident, $handler: expr) => {
        fn $fn_name<V>(self, visitor: V) -> DecodingResult<V::Value>
        where
            V: de::Visitor<'de>,
        {
            let buffer_size = self.bytes.len();
            if buffer_size == 0 {
                return Err(Error::UnexpectedEof);
            }

            match self.bytes[0] {
                v @ 0x00..=0x7f => {
                    self.bytes = &self.bytes[1..];
                    $handler(visitor, &[v])
                }
                v @ 0x80..=0xb7 => {
                    let bytes_size = v as usize - 0x80;

                    let necessary_size = bytes_size + 1;
                    if buffer_size < necessary_size {
                        return Err(Error::UnexpectedEof);
                    }

                    let (bytes, left_over) = self.bytes.split_at(necessary_size);
                    self.bytes = left_over;
                    $handler(visitor, &bytes[1..])
                }
                0xb8..=0xbf => decode_with_indirect_length::<V, _>(self, 0xb7, |bytes| {
                    visitor.visit_bytes(bytes)
                }),
                // catch-all for lists
                0xc0..=0xff => Err(Error::Message(
                    "Binary data cannot be represented with lists.".to_string(),
                )),
            }
        }
    };
}

fn utf8_char_width(b: u8) -> usize {
    UTF8_CHAR_WIDTH[b as usize] as usize
}

// Only the values in the range 0x00..0x7f represents valid UTF8 characters with a single byte, and
// parity happens to enforce these values to be encoded directly, which makes the string of a
// single byte always invalid.
fn invalid_one_byte_char<'de, V>(bytes: &[u8]) -> DecodingResult<V::Value>
where
    V: de::Visitor<'de>,
{
    if bytes.len() < 2 {
        return Err(Error::UnexpectedEof);
    }

    // These values must be encoded directly using 0x00..0x7f, and not 0x8100..0817f.
    if bytes[1] < 0x80 {
        return Err(Error::InvalidIndirection);
    }

    return Err(Error::InvalidUTF8);
}

fn decode_with_indirect_length<'de, V, F>(
    decoder: &mut ParityCompatibleDeserializer,
    base_value: usize,
    handler: F,
) -> DecodingResult<V::Value>
where
    V: de::Visitor<'de>,
    F: FnOnce(&[u8]) -> DecodingResult<V::Value>,
{
    let buffer_size = decoder.bytes.len();
    let prefix_size = decoder.bytes[0] as usize - base_value;
    let necessary_prefix_size = prefix_size + 1;

    if buffer_size < necessary_prefix_size {
        return Err(Error::UnexpectedEof);
    }

    // The length of the bytes may be up to 7 bytes. Which fits neatly in a u64.
    let mut str_lenght: u64 = 0;
    for pos in 1..necessary_prefix_size {
        str_lenght = (str_lenght << 8) | decoder.bytes[pos] as u64;
    }

    let str_size = match usize::try_from(str_lenght) {
        Ok(v) => v,
        Err(_) => {
            return Err(Error::Message(
                "Encoded value cannot be represented natively".to_string(),
            ))
        }
    };

    let necessary_size = str_size + prefix_size + 1;

    if buffer_size < necessary_size {
        return Err(Error::UnexpectedEof);
    }

    let (data, left_over) = decoder.bytes.split_at(necessary_size);
    let result = handler(&data[necessary_prefix_size..]);

    if let Ok(_) = result {
        decoder.bytes = left_over;
    }

    result
}

pub struct ParityCompatibleDeserializer<'a> {
    bytes: &'a [u8],
}

/// A wrapper around `Vec<u8>` which allows for direct serialization.
#[derive(Clone, Default, Debug)]
pub struct DirectBytes(Vec<u8>);

struct DirectBytesVisitor;

impl DirectBytes {
    pub fn from<T: Into<Vec<u8>>>(bytes: T) -> Self {
        DirectBytes(bytes.into())
    }
}

impl<'a> ParityCompatibleDeserializer<'a> {
    fn from_bytes(bytes: &[u8]) -> ParityCompatibleDeserializer {
        ParityCompatibleDeserializer { bytes }
    }
}

impl<'de> serde::de::Visitor<'de> for DirectBytesVisitor {
    type Value = DirectBytes;

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str("byte array")
    }

    fn visit_byte_buf<E>(self, v: Vec<u8>) -> Result<DirectBytes, E>
    where
        E: serde::de::Error,
    {
        Ok(DirectBytes::from(v))
    }
}

impl<'de> serde::de::Deserialize<'de> for DirectBytes {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::de::Deserializer<'de>,
    {
        deserializer.deserialize_byte_buf(DirectBytesVisitor)
    }
}

impl<'de, 'a, 'b: 'a> serde::de::SeqAccess<'de> for ParityCompatibleDeserializer<'a> {
    type Error = Error;

    fn next_element_seed<T>(&mut self, seed: T) -> DecodingResult<Option<T::Value>>
    where
        T: serde::de::DeserializeSeed<'de>,
    {
        if self.bytes.len() > 0 {
            let value = serde::de::DeserializeSeed::deserialize(seed, self)?;
            Ok(Some(value))
        } else {
            Ok(None)
        }
    }

    fn size_hint(&self) -> Option<usize> {
        None
    }
}

impl<'de, 'a, 'b: 'a> serde::de::MapAccess<'de> for ParityCompatibleDeserializer<'a> {
    type Error = Error;

    fn next_key_seed<T>(&mut self, seed: T) -> DecodingResult<Option<T::Value>>
    where
        T: serde::de::DeserializeSeed<'de>,
    {
        if self.bytes.len() > 0 {
            let value = serde::de::DeserializeSeed::deserialize(seed, self)?;
            Ok(Some(value))
        } else {
            Ok(None)
        }
    }

    fn next_value_seed<T>(&mut self, seed: T) -> DecodingResult<T::Value>
    where
        T: serde::de::DeserializeSeed<'de>,
    {
        if self.bytes.len() == 0 {
            return Err(Error::UnexpectedEof);
        }

        serde::de::DeserializeSeed::deserialize(seed, self)
    }

    fn size_hint(&self) -> Option<usize> {
        None
    }
}

impl<'de, 'a> de::Deserializer<'de> for &mut ParityCompatibleDeserializer<'a> {
    type Error = Error;

    // RLP is not a self-describing format
    not_supported!(deserialize_any, Error::DeserializeAnyNotSupported);
    not_supported!(deserialize_ignored_any, Error::DeserializeAnyNotSupported);

    // Parity does not have a default impl for signed integers, so here it is assumed the Ethereum
    // protocols don't use it (if this assumption is wrong the code has to be revisted for
    // compatibility).
    not_supported!(deserialize_i8, Error::DeserializeSignedIntegerNotSupported);
    not_supported!(deserialize_i16, Error::DeserializeSignedIntegerNotSupported);
    not_supported!(deserialize_i32, Error::DeserializeSignedIntegerNotSupported);
    not_supported!(deserialize_i64, Error::DeserializeSignedIntegerNotSupported);

    // Parity does not support floats out-of-the-box.
    not_supported!(deserialize_f32, Error::DeserializeFloatNotSupported);
    not_supported!(deserialize_f64, Error::DeserializeFloatNotSupported);

    fn deserialize_enum<V>(
        self,
        _name: &'static str,
        _variants: &'static [&'static str],
        _visitor: V,
    ) -> DecodingResult<V::Value>
    where
        V: de::Visitor<'de>,
    {
        Err(Error::DeserializeEnumSupported)
    }

    not_supported!(
        deserialize_identifier,
        Error::DeserializeIdentifierSupported
    );

    fn deserialize_bool<V>(self, visitor: V) -> DecodingResult<V::Value>
    where
        V: de::Visitor<'de>,
    {
        match self.bytes.get(0) {
            Some(v @ 0x00..=0x7f) => {
                self.bytes = &self.bytes[1..];
                visitor.visit_bool(*v != 0x0)
            }
            // empty string can be used to represent false
            Some(0x80) => {
                self.bytes = &self.bytes[1..];
                visitor.visit_bool(false)
            }
            // strings of length <=55
            Some(v @ 0x81..=0xb7) => {
                let length = *v as usize - 0x80;

                Err(Error::Message(format!(
                    "Boolean values cannot be represented by a string of length {}",
                    length
                )))
            }
            Some(0xb8..=0xbf) => Err(Error::Message(
                "Boolean values cannot be represented with strings longer than 55 bytes."
                    .to_string(),
            )),
            // catch-all for lists
            Some(0xc0..=0xff) => Err(Error::Message(
                "Boolean values cannot be represented with lists.".to_string(),
            )),
            None => Err(Error::UnexpectedEof),
        }
    }

    deserialize_u!(deserialize_u8, visit_u8, u8);
    deserialize_u!(deserialize_u16, visit_u16, u16);
    deserialize_u!(deserialize_u32, visit_u32, u32);
    deserialize_u!(deserialize_u64, visit_u64, u64);

    fn deserialize_char<V>(self, visitor: V) -> DecodingResult<V::Value>
    where
        V: de::Visitor<'de>,
    {
        let buffer_size = self.bytes.len();
        if buffer_size == 0 {
            return Err(Error::UnexpectedEof);
        }

        match self.bytes[0] {
            v @ 0x00..=0x7f => {
                self.bytes = &self.bytes[1..];

                // This works because of a few quirks:
                //
                // 1. This deserializer assumes UTF-8, which is a superset of ascii
                // 2. `b as char` (u8-char-cast)  assumes windows-1252, which is superset of ascii
                //    (https://doc.rust-lang.org/std/primitive.char.html#impl-From%3Cu8%3E)
                // 3. The values 00..=7f are exactly the ascii values, which means that decoding
                //    any of these bytes with either encoder produces the same result.
                visitor.visit_char(v as char)
            }
            0x80 => Err(Error::Message(
                "Char needs at least one byte, zero found".to_string(),
            )),
            0x81 => invalid_one_byte_char::<V>(self.bytes),
            v @ 0x82..=0x84 => {
                let char_size = v as usize - 0x80;

                let necessary_size = char_size + 1;
                if buffer_size < necessary_size {
                    return Err(Error::UnexpectedEof);
                }

                let first_byte = self.bytes[1];

                // Makes sure the slice contains exactly one character, this covers a few cases:
                //
                // - The slice is bigger than the encoded character, e.g. `0x826869` which would
                // encode "hi". The extra bytes could also have invalid characters.
                // - The string is not long enough to encode a valid character, e.g. `0x82f09f`, in
                // the encoded string has length `2` but UTF8 chars starting with `0xf0` needs `4`
                // bytes.
                if utf8_char_width(first_byte) != char_size {
                    return Err(Error::InvalidUTF8);
                }

                // At this point the size of the buffer is known to encoded a single character, but
                // not if the encoding is correct.

                match str::from_utf8(&self.bytes[1..necessary_size]) {
                    Ok(res) => {
                        self.bytes = &self.bytes[necessary_size..];
                        visitor.visit_char(res.chars().next().unwrap())
                    }
                    Err(_) => Err(Error::InvalidUTF8),
                }
            }
            v @ 0x85..=0xb7 => {
                let length = v as usize - 0x80;
                Err(Error::Message(format!(
                    "An utf8 char can have at most 4 bytes of length, {} found",
                    length
                )))
            }
            0xb8..=0xbf => Err(Error::Message(
                "An utf8 char can have at most 4 bytes of length, more found".to_string(),
            )),
            // catch-all for lists
            0xc0..=0xff => Err(Error::Message(
                "An utf8 char cannot be represented with lists.".to_string(),
            )),
        }
    }

    deserialize_str!(deserialize_str, visit_str);
    deserialize_str!(deserialize_string, visit_string);

    deserialize_b!(deserialize_bytes, |visitor: V, bytes: &[u8]| {
        visitor.visit_bytes(bytes)
    });
    deserialize_b!(deserialize_byte_buf, |visitor: V, bytes: &[u8]| {
        visitor.visit_byte_buf(bytes.to_vec())
    });

    fn deserialize_option<V>(self, visitor: V) -> DecodingResult<V::Value>
    where
        V: de::Visitor<'de>,
    {
        let buffer_size = self.bytes.len();
        if buffer_size == 0 {
            return Err(Error::UnexpectedEof);
        }

        match self.bytes[0] {
            0x00..=0x7f => Err(Error::Message(
                "Option cannot be represented by a single byte, only lists.".to_string(),
            )),
            0x80..=0xbf => Err(Error::Message(
                "Option cannot be represented by a string, only lists.".to_string(),
            )),
            0xc0 => visitor.visit_none(),
            0xc1 => {
                self.bytes = &self.bytes[1..];
                visitor.visit_some(self)
            }
            v @ 0xc2..=0xf7 => {
                let length = v as usize - 0xc0;
                Err(Error::Message(format!(
                    "Option can either be an empty list or an list of 1 element, {} found",
                    length
                )))
            }
            0xf8..=0xff => Err(Error::Message(
                "Option cannot be represented by long lists.".to_string(),
            )),
        }
    }

    fn deserialize_unit<V>(self, visitor: V) -> DecodingResult<V::Value>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_unit()
    }

    fn deserialize_unit_struct<V>(self, _name: &'static str, visitor: V) -> DecodingResult<V::Value>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_unit()
    }

    fn deserialize_newtype_struct<V>(
        self,
        _name: &'static str,
        visitor: V,
    ) -> DecodingResult<V::Value>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_newtype_struct(self)
    }

    fn deserialize_seq<V>(self, visitor: V) -> DecodingResult<V::Value>
    where
        V: de::Visitor<'de>,
    {
        let buffer_size = self.bytes.len();
        if buffer_size == 0 {
            return Err(Error::UnexpectedEof);
        }

        match self.bytes[0] {
            0x00..=0x7f => Err(Error::Message(
                "Seq cannot be represented by a single byte, only lists.".to_string(),
            )),
            0x80..=0xbf => Err(Error::Message(
                "Seq cannot be represented by a string, only lists.".to_string(),
            )),
            v @ 0xc0..=0xf7 => {
                let length = v as usize - 0xc0;

                let (sequence_data, left_over) = self.bytes.split_at(length);

                self.bytes = left_over;
                visitor.visit_seq(ParityCompatibleDeserializer {
                    bytes: sequence_data,
                })
            }
            0xf8..=0xff => decode_with_indirect_length::<V, _>(self, 0xf7, |inner_bytes| {
                visitor.visit_seq(ParityCompatibleDeserializer { bytes: inner_bytes })
            }),
        }
    }

    fn deserialize_tuple<V>(self, _len: usize, visitor: V) -> DecodingResult<V::Value>
    where
        V: de::Visitor<'de>,
    {
        // Ignore the number of elements hint, this is not useful since RLP encodes the aggregate
        // byte size, and not the number of elements.
        self.deserialize_seq(visitor)
    }

    fn deserialize_tuple_struct<V>(
        self,
        _name: &'static str,
        _len: usize,
        visitor: V,
    ) -> DecodingResult<V::Value>
    where
        V: de::Visitor<'de>,
    {
        // Ignore the number of elements hint, this is not useful since RLP encodes the aggregate
        // byte size, and not the number of elements.
        self.deserialize_seq(visitor)
    }

    fn deserialize_map<V>(self, visitor: V) -> DecodingResult<V::Value>
    where
        V: de::Visitor<'de>,
    {
        let buffer_size = self.bytes.len();
        if buffer_size == 0 {
            return Err(Error::UnexpectedEof);
        }

        match self.bytes[0] {
            0x00..=0x7f => Err(Error::Message(
                "Map cannot be represented by a single byte, only lists.".to_string(),
            )),
            0x80..=0xbf => Err(Error::Message(
                "Map cannot be represented by a string, only lists.".to_string(),
            )),
            v @ 0xc0..=0xf7 => {
                let length = v as usize - 0xc0;

                let (sequence_data, left_over) = self.bytes.split_at(length);

                self.bytes = left_over;
                visitor.visit_seq(ParityCompatibleDeserializer {
                    bytes: sequence_data,
                })
            }
            0xf8..=0xff => decode_with_indirect_length::<V, _>(self, 0xf7, |inner_bytes| {
                visitor.visit_seq(ParityCompatibleDeserializer { bytes: inner_bytes })
            }),
        }
    }

    fn deserialize_struct<V>(
        self,
        _name: &'static str,
        _fields: &'static [&'static str],
        visitor: V,
    ) -> DecodingResult<V::Value>
    where
        V: de::Visitor<'de>,
    {
        self.deserialize_seq(visitor)
    }
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
                    let encoded = rlp::encode(v);
                    let result: $type = from_bytes_parity_compatible(encoded.as_slice()).unwrap();
                    assert_eq!(result, *v);
                }
            }
        };
    }

    macro_rules! test_range {
        ($name: ident, $type: ty,$start: literal, $end: literal) => {
            #[test]
            fn $name() {
                for byte in $start..=$end {
                    let bytes = &[byte];

                    let parity_result: Result<$type, _> = rlp::decode(bytes);
                    let result: DecodingResult<$type> = from_bytes_parity_compatible(bytes);
                    let results = (parity_result, result);

                    match results {
                        (Ok(v1), Ok(v2)) => assert_eq!(v1, v2),
                        (Err(_v1), Err(_v2)) => {}
                        (Ok(v1), Err(v2)) => {
                            assert!(false, "parity result {:?}, library error {:?}", v1, v2)
                        }
                        (Err(v1), Ok(v2)) => {
                            assert!(false, "parity error {:?}, library result {:?}", v1, v2)
                        }
                    }
                }
            }
        };
    }

    test_min_max_for_u!(test_decoding_u16, u16);
    test_min_max_for_u!(test_decoding_u32, u32);
    test_min_max_for_u!(test_decoding_u64, u64);

    test_range!(test_decoding_single_byte_u8, u8, 0x00u8, 0xffu8);
    test_range!(test_decoding_single_byte_u16, u16, 0x00u8, 0xffu8);
    test_range!(test_decoding_single_byte_u32, u32, 0x00u8, 0xffu8);
    test_range!(test_decoding_single_byte_u64, u64, 0x00u8, 0xffu8);
    test_range!(test_decoding_single_byte_bool, bool, 0x00u8, 0xffu8);

    // Vec<u8> is incompatible, wrapping it in a DirectBytes is necessary to get the correct
    // behavior!
    #[test]
    fn test_decoding_vec_of_u8() {
        for byte in 0x00..=0xff {
            let bytes = &[byte];

            let parity_result: Result<Vec<u8>, _> = rlp::decode(bytes);
            let result: DecodingResult<DirectBytes> = from_bytes_parity_compatible(bytes);
            let results = (parity_result, result);

            match results {
                (Ok(v1), Ok(v2)) => assert_eq!(v1, v2.0),
                (Err(_v1), Err(_v2)) => {}
                (Ok(v1), Err(v2)) => {
                    assert!(false, "parity result {:?}, library error {:?}", v1, v2)
                }
                (Err(v1), Ok(v2)) => {
                    assert!(false, "parity error {:?}, library result {:?}", v1, v2)
                }
            }
        }

        let values = [
            "",
            "a",
            "cats",
            "dogs",
            "This is a string with more than 55 bytes to test decoding of long lists",
        ];

        for v in &values {
            let encoded = rlp::encode(v);
            let result: String = from_bytes_parity_compatible(encoded.as_slice()).unwrap();
            assert_eq!(&result, v);
        }
    }

    #[test]
    fn test_string() {
        let values = [
            "".to_string(),
            "a".to_string(),
            "cats".to_string(),
            "dogs".to_string(),
            "This is a string with more than 55 bytes to test decoding of long lists".to_string(),
        ];

        for v in &values {
            let encoded = rlp::encode(v);
            let result: String = from_bytes_parity_compatible(encoded.as_slice()).unwrap();
            assert_eq!(&result, v);
        }
    }
}
