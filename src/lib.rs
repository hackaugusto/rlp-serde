mod de;
mod error;
mod ser;

pub use de::{from_bytes_parity_compatible, ParityCompatibleDeserializer};
pub use error::{Error, Result};
// pub use ser::{to_bytes, Serializer};
