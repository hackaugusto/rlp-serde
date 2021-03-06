use serde::{de, ser};
use std::fmt::{self, Display};
use std::io;

pub type Result<T> = ::std::result::Result<T, Error>;

#[derive(Debug)]
pub enum Error {
    TrailingCharacters,
    UnexpectedEof,
    IOError,

    DeserializeAnyNotSupported,
    DeserializeSignedIntegerNotSupported,
    DeserializeFloatNotSupported,
    DeserializeIdentifierSupported,
    DeserializeEnumSupported,

    InvalidUTF8,

    // Parity enforces the values 0x00..=0x7f for any unsigned type (e.g. u32, u64, etc.) and of
    // chars to be represented as a direct byte. IOW `0x00` should be used instead of `0x8100`.
    InvalidIndirection,

    SerializeSignedIntegerNotSupported,
    SerializeFloatNotSupported,
    SerializeBytesIsTooBig,

    Message(String),
}

impl From<io::Error> for Error {
    fn from(_: io::Error) -> Self {
        Error::IOError
    }
}

impl ser::Error for Error {
    fn custom<T: Display>(msg: T) -> Self {
        Error::Message(msg.to_string())
    }
}

impl de::Error for Error {
    fn custom<T: Display>(msg: T) -> Self {
        Error::Message(msg.to_string())
    }
}

impl Display for Error {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Error::TrailingCharacters => {
                formatter.write_str("Input data has trailling characters.")
            }
            Error::UnexpectedEof => formatter.write_str("Input data ended unexpectedly."),
            Error::IOError => formatter.write_str("Writer failed unexpectedly."),
            Error::DeserializeAnyNotSupported => {
                formatter.write_str("RLP is not self-describing. deserialize_any is not supported.")
            }
            Error::DeserializeSignedIntegerNotSupported => {
                formatter.write_str("Parity compatible decoder does not support signed integers.")
            }
            Error::DeserializeFloatNotSupported => {
                formatter.write_str("Parity compatible decoder does not support floats.")
            }
            Error::DeserializeIdentifierSupported => formatter
                .write_str("Parity compatible decoder does not support deserialize_identifier."),
            Error::DeserializeEnumSupported => {
                formatter.write_str("Parity compatible decoder does not support deserialize_enum.")
            }
            Error::InvalidUTF8 => formatter.write_str("Encoded string is not valid UTF8."),
            Error::InvalidIndirection => {
                formatter.write_str("Data is not encoded in an optimal way, rejected.")
            }
            Error::Message(msg) => formatter.write_str(msg),
            Error::SerializeSignedIntegerNotSupported => {
                formatter.write_str("Parity compatible encoder does not support signed integers.")
            }
            Error::SerializeFloatNotSupported => {
                formatter.write_str("Parity compatible encoder does not support floats.")
            }
            Error::SerializeBytesIsTooBig => {
                formatter.write_str("Bytes data is too big to be encoded.")
            }
        }
    }
}
impl std::error::Error for Error {}
