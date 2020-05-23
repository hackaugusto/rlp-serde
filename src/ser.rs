// use crate::error::Result;
// use serde::Serialize;
// pub struct ParityCompatibleSerializer<'a> {
//     bytes: &'a [u8],
// }
// pub fn to_bytes<T>(value: &T) -> Result<Vec<u8>>
// where
//     T: Serialize,
// {
//     let mut serializer = Serializer {};
//     value.serialize(&mut serializer)?;
//     Ok(serializer.output)
// }
