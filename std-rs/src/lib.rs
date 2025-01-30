#![allow(non_snake_case)]
use base64::{
    engine::{
        general_purpose::{NO_PAD, PAD},
        GeneralPurpose,
    },
    DecodeError, Engine as _,
};
use std::{fmt::Debug, marker::PhantomData};
//  Re - export
pub use derive_new::new;

#[derive(Debug)]
pub struct W<T>(pub T);

/// Get an environment variable by name, returning an error message if not found.
pub fn get_env(name: &'static str) -> Result<String, String> {
    std::env::var(name).map_err(|_| format!("{} not found in environment", name))
}

/// Get and parse an environment variable into the desired type `T`.
pub fn get_env_parse<T: std::str::FromStr>(name: &'static str) -> Result<T, String> {
    let msg = format!(
        "Failed to parse {} into {}",
        name,
        std::any::type_name::<T>()
    );
    get_env(name).and_then(|value| value.parse::<T>().map_err(|_| msg))
}

#[macro_export]
macro_rules! lazy_lock {
    ($definition:expr) => {
        std::sync::LazyLock::new(|| $definition)
    };
    (() => $block:block) => {
        std::sync::LazyLock::new(|| $block)
    };
}

/// Ensure a predicate is true; return an error otherwise.
#[macro_export]
macro_rules! ensure {
    ($pred:expr, $err:expr) => {
        if !$pred {
            return Err($err);
        }
    };
}

/// Always return an error; used for early exits.
#[macro_export]
macro_rules! err {
    ($err:expr) => {
        return Err($err)
    };
}

/// Clone an expression.
#[macro_export]
macro_rules! clone {
    ($expr:expr) => {
        $expr.clone()
    };
}

/// Get the duration since a specific `Instant`.
#[macro_export]
macro_rules! duration_since {
    ($earlier:expr) => {
        std::time::Instant::now().duration_since($earlier)
    };
}

/// Simplified string formatting.
#[macro_export]
macro_rules! f {
    ($($arg:tt)*) => {
        format!($($arg)*)
    };
}

/// Implement `Error` and `Display` for a type.
#[macro_export]
macro_rules! impl_error_display {
    ($ident:ident) => {
        impl std::error::Error for $ident {}
        impl std::fmt::Display for $ident {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "Error: {:?}", self)
            }
        }
    };
}

/// Return `Some` for an optional value, or `None` otherwise.
#[macro_export]
macro_rules! opt {
    ($( $value:expr )?) => {
        match ($(Some($value))?) {
            Some(_) => Some($value),
            _ => None,
        }
    };
}

/// Create an `Arc` for a value.
#[macro_export]
macro_rules! arc {
    ($value:expr) => {
        std::sync::Arc::new($value)
    };
}

/// Create a `Mutex` for a value.
#[macro_export]
macro_rules! mutex {
    ($value:expr) => {
        std::sync::Mutex::new($value)
    };
}

/// Create a static reference from a type and data using `LazyLock`.
#[macro_export]
macro_rules! to_static {
    ($ty:ty, $data:expr) => {
        static DATA: std::sync::LazyLock<$ty> = $crate::lazy_lock!($data);
        &*DATA
    };
}

/// A macro to create `String` instances in various formats.
///
/// # Usage
/// ```rust
/// use std_rs::string;
///
/// let empty = string!();
/// let simple = string!("Hello, world!");
/// let with_capacity = string!("Hello", 20);
///
/// let from_utf8 = string!(u8: vec![72, 101, 108, 108, 111]).unwrap();
/// let from_utf8_lossy = string!(u8l: &[255, 72, 101, 108, 108, 111]);
///
/// let from_utf16 = string!(u16: &[72, 101, 108, 108, 111]).unwrap();
/// let from_utf16_lossy = string!(u16l: &[72, 101, 108, 108, 111]);
///
/// let repeat_chars = string!(repeat: 'A', 5);
/// let from_char_iter = string!(iter: ['H', 'e', 'l', 'l', 'o']);
///
/// let with_capacity_only = string!(capacity: 30);
/// ```
///
/// # Supported Variants
/// - `string!()` → Creates an empty `String`.
/// - `string!(content)` → Converts input into a `String`.
/// - `string!(content, capacity)` → Creates a `String` with initial capacity and content.
/// - `string!(u8: content)` → Creates a `String` from `Vec<u8>`. Can fail.
/// - `string!(u8l: content)` → Creates a lossy `String` from `&[u8]`.
/// - `string!(u16: content)` → Creates a `String` from `&[u16]`. Can fail.
/// - `string!(u16l: content)` → Creates a lossy `String` from `&[u16]`.
/// - `string!(repeat: char, count)` → Creates a `String` by repeating a character `count` times.
/// - `string!(iter: iterable)` → Creates a `String` from an iterator of characters.
/// - `string!(capacity: size)` → Creates an empty `String` with the given capacity.
#[macro_export]
macro_rules! string {
    // Empty string
    () => {
        String::new()
    };

    // String from content
    ($content:expr) => {
        String::from($content)
    };

    // String from content with specified capacity
    ($content:expr, $cap:expr) => {{
        let mut string = String::with_capacity($cap);
        string.push_str($content);
        string
    }};

    // String from Vec<u8> (UTF-8), returns Result<String, Utf8Error>
    (u8: $content:expr) => {
        String::from_utf8($content)
    };

    // Lossy String from &[u8] (UTF-8)
    (u8l: $content:expr) => {
        String::from_utf8_lossy($content).to_string()
    };

    // String from &[u16] (UTF-16), returns Result<String, Utf16Error>
    (u16: $content:expr) => {
        String::from_utf16($content)
    };

    // Lossy String from &[u16] (UTF-16)
    (u16l: $content:expr) => {
        String::from_utf16_lossy($content)
    };

    // Repeat a character `count` times
    (repeat: $ch:expr, $count:expr) => {
        std::iter::repeat($ch).take($count).collect::<String>()
    };

    // Create String from iterator of chars
    (iter: $iterable:expr) => {
        $iterable.into_iter().collect::<String>()
    };

    // Create String with specific capacity
    (capacity: $size:expr) => {
        String::with_capacity($size)
    };
}

/// Represents the result of a hashing verification process.
pub enum HashingResult {
    /// Verification failed.
    Failed,

    /// Verification succeeded.
    Success,
}

/// A trait for hashing and verifying data.
///
/// This trait defines methods for computing hashes and verifying them.
pub trait Hasher {
    /// Useful for logging or marking a hashed password
    const NAME: &'static str;

    /// Computes a hash for the given content.
    ///
    /// # Notes
    ///
    /// Implementors should return an empty string if hashing fails. This approach is suggested
    /// to make it easy to check if the hash computation was successful. Errors are not expected,
    /// so this method does not return a `Result`.
    ///
    /// # Parameters
    ///
    /// * `content` - The input string to hash.
    ///
    /// # Returns
    ///
    /// A `String` representing the computed hash. Returns an empty string if hashing fails.
    fn hash(&self, content: &str) -> String;

    /// Verifies whether the provided content matches the given hash.
    ///
    /// # Parameters
    ///
    /// * `content` - The original input string to verify.
    /// * `other` - The hash to compare against.
    ///
    /// # Returns
    ///
    /// A `HashingResult` indicating whether the verification succeeded, failed or rehash needed!.
    fn verify(&self, content: &str, other: &str) -> HashingResult;
}

// Update comment of the code below and make it better
pub trait Encoding {
    const NAME: &'static str;
    type Success;
    type Error;

    fn encode(&self, input: impl AsRef<[u8]>) -> Result<Self::Success, Self::Error>;

    fn decode(&self, input: impl AsRef<[u8]>) -> Result<Self::Success, Self::Error>;
}

pub trait Encryption {
    type Success;
    type Error;
    type Claim;

    fn encrypt(&self, claim: Self::Claim) -> Result<Self::Success, Self::Error>;

    fn decrypt<T>(&self, content: Self::Success, claim: Self::Claim) -> Result<T, Self::Error>;
}

/// A struct representing a sensitive type with safe handling for display and serialization.
#[derive(new, Clone, Eq, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Deserialize))]
pub struct Sensitive<T> {
    content: T,
}

impl<T> std::ops::Deref for Sensitive<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.content
    }
}

impl<T> std::fmt::Display for Sensitive<T>
where
    T: std::fmt::Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if cfg!(debug_assertions) {
            // In debug mode, display the actual content for easier testing.
            write!(f, "{}", self.content)
        } else {
            // In release mode, redact the Sensitive to prevent leaks.
            write!(f, "<Content: REDACTED>")
        }
    }
}

impl<T> std::fmt::Debug for Sensitive<T>
where
    T: Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if cfg!(debug_assertions) {
            f.debug_struct("Sensitive")
                .field("content", &self.content)
                .finish()
        } else {
            f.debug_struct("Sensitive")
                .field("content", &"<Content: REDACTED>")
                .finish()
        }
    }
}

// Implement `Serialize` to safely handle Sensitive serialization.
#[cfg(feature = "serde")]
mod sensitive_serde {
    use serde::{Serialize, Serializer};

    use crate::Sensitive;
    impl<T> Serialize for Sensitive<T>
    where
        T: Serialize,
    {
        fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where
            S: Serializer,
        {
            if cfg!(debug_assertions) {
                // In debug mode, serialize the actual content.
                self.content.serialize(serializer)
            } else {
                // In release mode, redact the content to prevent leaks.
                serializer.serialize_str("<Content: REDACTED>")
            }
        }
    }
}

#[cfg(feature = "serde")]
#[allow(unused_imports)]
pub use sensitive_serde::*;

impl<T> From<T> for Sensitive<T> {
    fn from(value: T) -> Self {
        Self::new(value)
    }
}

/// ```no_rust
/// Base64
///
/// Encode and Decode bytes using base64 encoding
/// ```
#[derive(Clone)]
#[cfg_attr(test, derive(Debug))]
pub struct B64<T = UrlSafe>(PhantomData<T>);

impl<T> B64<T> {
    pub fn new() -> Self {
        B64(PhantomData)
    }
}

pub(crate) const STANDARD: GeneralPurpose = GeneralPurpose::new(&base64::alphabet::STANDARD, PAD);
pub(crate) const STANDARD_NO_PAD: GeneralPurpose =
    GeneralPurpose::new(&base64::alphabet::STANDARD, NO_PAD);
pub(crate) const URL_SAFE: GeneralPurpose = GeneralPurpose::new(&base64::alphabet::URL_SAFE, PAD);
pub(crate) const URL_SAFE_NO_PAD: GeneralPurpose =
    GeneralPurpose::new(&base64::alphabet::URL_SAFE, NO_PAD);

// These are used to enforced the standard we want
macro_rules! impl_encoding {
    ($ident:ident, $alg:expr, $name:expr) => {
        #[derive(Clone)]
        pub struct $ident;
        impl Encoding for B64<$ident> {
            const NAME: &'static str = $name;
            type Success = String;
            type Error = String;

            fn encode(&self, input: impl AsRef<[u8]>) -> Result<Self::Success, Self::Error> {
                Ok($alg.encode(input))
            }

            fn decode(&self, input: impl AsRef<[u8]>) -> Result<Self::Success, Self::Error> {
                $alg.decode(input)
                    .map(String::from_utf8)
                    .map_err(from_decode_error_to_string)?
                    .map_err(|_| f!("Failed to convert decoded bytes into a UTF-8 string"))
            }
        }
    };
}

fn from_decode_error_to_string(args: DecodeError) -> String {
    use base64::DecodeError::*;

    match args {
        InvalidByte(offset, bytes) => {
            f!("Invalid token byte at offset: {} bytes = {}", offset, bytes)
        }
        InvalidLength(length) => f!("The length of the token is invalid length: {}", length),
        InvalidLastSymbol(o, b) => f!("Failed encoding, invalid offset: {} bytes = {}", o, b),
        InvalidPadding => string!("This token failed encoding to due to invalid padding"),
    }
}

impl_encoding!(UrlSafe, URL_SAFE, "URLSAFE");
impl_encoding!(Standard, STANDARD, "STANDARD");
impl_encoding!(UrlSafeNopad, URL_SAFE_NO_PAD, "URLSAFE NOPAD");
impl_encoding!(StandardNopad, STANDARD_NO_PAD, "STANDARD NOPAD");

#[cfg(test)]
pub mod test {
    use super::*;

    use super::Sensitive;

    #[test]
    fn test_sensitive_display() {
        let sensitive = Sensitive::new(string!("secret"));

        if cfg!(debug_assertions) {
            assert_eq!(format!("{}", sensitive), "secret");
        } else {
            assert_eq!(format!("{}", sensitive), "<Content: REDACTED>");
        }
    }

    #[test]
    #[cfg(feature = "serde")]
    fn test_serializer_sensitive() {
        use serde_json::to_string;

        let sensitive = Sensitive::new(string!("secret"));

        if cfg!(debug_assertions) {
            assert_eq!(to_string(&sensitive).unwrap(), "\"secret\"");
        } else {
            assert_eq!(to_string(&sensitive).unwrap(), "\"<Content: REDACTED>\"");
        }
    }

    /// Encode and Decode The value Passed testing the engine passed
    ///
    /// # Errors
    ///
    /// This function will return an error if encoding and decoding failed.
    fn encode_and_decode_handler<T>(engine: T, value: impl AsRef<[u8]>)
    where
        T: Encoding<Success = String, Error = String>,
    {
        let enc_content = engine.encode(value).unwrap();

        println!("{} - {:?}", T::NAME, enc_content);
        println!("{} - {:?}", T::NAME, engine.decode(enc_content).unwrap());
    }

    #[test]
    fn test_standard_no_pad() {
        encode_and_decode_handler(B64::<StandardNopad>::new(), "ABCDGETAJHE")
    }

    #[test]
    fn test_url_safe_no_pad() {
        encode_and_decode_handler(B64::<UrlSafeNopad>::new(), "ABCDGETAJHE")
    }

    #[test]
    fn test_standard() {
        encode_and_decode_handler(B64::<Standard>::new(), "ABCDGETAJHE")
    }

    #[test]
    fn test_url_safe() {
        encode_and_decode_handler(B64::<UrlSafe>::new(), "ABCDGETAJHE")
    }
}
