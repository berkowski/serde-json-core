//! Serialize a Rust data structure into pretty-printed JSON data
use serde::ser;
use serde::ser::SerializeStruct as _;

use core::{fmt, str};

use self::map::SerializeMap;
use self::seq::SerializeSeq;
use self::struct_::{SerializeStruct, SerializeStructVariant};

#[cfg(feature = "heapless")]
use heapless::{String, Vec};

use crate::ser::{Error, Result, hex};

mod map;
mod seq;
mod struct_;

pub(crate) struct Serializer<'buffer, 'indent> {
    buf: &'buffer mut [u8],
    current_length: usize,
    current_indent: usize,
    indent: &'indent [u8],
}

impl<'buffer, 'indent> Serializer<'buffer, 'indent> {
    fn new(buf: &'buffer mut [u8], indent: &'indent [u8]) -> Self {
        Serializer {
            buf,
            current_length: 0,
            current_indent: 0,
            indent,
        }
    }

    fn push(&mut self, c: u8) -> Result<()> {
        if self.current_length < self.buf.len() {
            unsafe { self.push_unchecked(c) };
            Ok(())
        } else {
            Err(Error::BufferFull)
        }
    }

    unsafe fn push_unchecked(&mut self, c: u8) {
        self.buf[self.current_length] = c;
        self.current_length += 1;
    }

    fn extend_from_slice(&mut self, other: &[u8]) -> Result<()> {
        if self.current_length + other.len() > self.buf.len() {
            // won't fit in the buf; don't modify anything and return an error
            Err(Error::BufferFull)
        } else {
            for c in other {
                unsafe { self.push_unchecked(*c) };
            }
            Ok(())
        }
    }

    pub fn indent(&mut self) -> Result<()> {
        for _ in 0..self.current_indent {
            self.extend_from_slice(self.indent)?;
        }
        Ok(())
    }
}

impl<'serializer, 'buffer: 'serializer, 'indent: 'serializer> ser::Serializer for &'serializer mut Serializer<'buffer, 'indent> {
    type Ok = ();
    type Error = Error;
    type SerializeSeq = SerializeSeq<'serializer, 'buffer, 'indent>;
    type SerializeTuple = SerializeSeq<'serializer, 'buffer, 'indent>;
    type SerializeTupleStruct = crate::ser::Unreachable;
    type SerializeTupleVariant = crate::ser::Unreachable;
    type SerializeMap = SerializeMap<'serializer, 'buffer, 'indent>;
    type SerializeStruct = SerializeStruct<'serializer, 'buffer, 'indent>;
    type SerializeStructVariant = SerializeStructVariant<'serializer, 'buffer, 'indent>;

    fn serialize_bool(self, v: bool) -> Result<Self::Ok> {
        if v {
            self.extend_from_slice(b"true")
        } else {
            self.extend_from_slice(b"false")
        }
    }

    fn serialize_i8(self, v: i8) -> Result<Self::Ok> {
        // "-128"
        serialize_signed!(self, 4, v, i8, u8)
    }

    fn serialize_i16(self, v: i16) -> Result<Self::Ok> {
        // "-32768"
        serialize_signed!(self, 6, v, i16, u16)
    }

    fn serialize_i32(self, v: i32) -> Result<Self::Ok> {
        // "-2147483648"
        serialize_signed!(self, 11, v, i32, u32)
    }

    fn serialize_i64(self, v: i64) -> Result<Self::Ok> {
        // "-9223372036854775808"
        serialize_signed!(self, 20, v, i64, u64)
    }

    fn serialize_u8(self, v: u8) -> Result<Self::Ok> {
        // "255"
        serialize_unsigned!(self, 3, v)
    }

    fn serialize_u16(self, v: u16) -> Result<Self::Ok> {
        // "65535"
        serialize_unsigned!(self, 5, v)
    }

    fn serialize_u32(self, v: u32) -> Result<Self::Ok> {
        // "4294967295"
        serialize_unsigned!(self, 10, v)
    }

    fn serialize_u64(self, v: u64) -> Result<Self::Ok> {
        // "18446744073709551615"
        serialize_unsigned!(self, 20, v)
    }

    fn serialize_f32(self, v: f32) -> Result<Self::Ok> {
        if v.is_finite() {
            serialize_ryu!(self, v)
        } else {
            self.serialize_none()
        }
    }

    fn serialize_f64(self, v: f64) -> Result<Self::Ok> {
        if v.is_finite() {
            serialize_ryu!(self, v)
        } else {
            self.serialize_none()
        }
    }

    fn serialize_char(self, _v: char) -> Result<Self::Ok> {
        unreachable!()
    }

    fn serialize_str(self, v: &str) -> Result<Self::Ok> {
        self.push(b'"')?;

        // Do escaping according to "6. MUST represent all strings (including object member names) in
        // their minimal-length UTF-8 encoding": https://gibson042.github.io/canonicaljson-spec/
        //
        // We don't need to escape lone surrogates because surrogate pairs do not exist in valid UTF-8,
        // even if they can exist in JSON or JavaScript strings (UCS-2 based). As a result, lone surrogates
        // cannot exist in a Rust String. If they do, the bug is in the String constructor.
        // An excellent explanation is available at https://www.youtube.com/watch?v=HhIEDWmQS3w

        // Temporary storage for encoded a single char.
        // A char is up to 4 bytes long wehn encoded to UTF-8.
        let mut encoding_tmp = [0u8; 4];

        for c in v.chars() {
            match c {
                '\\' => {
                    self.push(b'\\')?;
                    self.push(b'\\')?;
                }
                '"' => {
                    self.push(b'\\')?;
                    self.push(b'"')?;
                }
                '\u{0008}' => {
                    self.push(b'\\')?;
                    self.push(b'b')?;
                }
                '\u{0009}' => {
                    self.push(b'\\')?;
                    self.push(b't')?;
                }
                '\u{000A}' => {
                    self.push(b'\\')?;
                    self.push(b'n')?;
                }
                '\u{000C}' => {
                    self.push(b'\\')?;
                    self.push(b'f')?;
                }
                '\u{000D}' => {
                    self.push(b'\\')?;
                    self.push(b'r')?;
                }
                '\u{0000}'..='\u{001F}' => {
                    self.push(b'\\')?;
                    self.push(b'u')?;
                    self.push(b'0')?;
                    self.push(b'0')?;
                    let (hex1, hex2) = hex(c as u8);
                    self.push(hex1)?;
                    self.push(hex2)?;
                }
                _ => {
                    let encoded = c.encode_utf8(&mut encoding_tmp as &mut [u8]);
                    self.extend_from_slice(encoded.as_bytes())?;
                }
            }
        }

        self.push(b'"')
    }

    fn serialize_bytes(self, v: &[u8]) -> Result<Self::Ok> {
        self.extend_from_slice(v)
    }

    fn serialize_none(self) -> Result<Self::Ok> {
        self.extend_from_slice(b"null")
    }

    fn serialize_some<T: ?Sized>(self, value: &T) -> Result<Self::Ok>
        where
            T: ser::Serialize,
    {
        value.serialize(self)
    }

    fn serialize_unit(self) -> Result<Self::Ok> {
        self.serialize_none()
    }

    fn serialize_unit_struct(self, _name: &'static str) -> Result<Self::Ok> {
        self.serialize_unit()
    }

    fn serialize_unit_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        variant: &'static str,
    ) -> Result<Self::Ok> {
        self.serialize_str(variant)
    }

    fn serialize_newtype_struct<T: ?Sized>(self, _name: &'static str, value: &T) -> Result<Self::Ok>
        where
            T: ser::Serialize,
    {
        value.serialize(self)
    }

    fn serialize_newtype_variant<T: ?Sized>(
        self,
        _name: &'static str,
        _variant_index: u32,
        variant: &'static str,
        value: &T,
    ) -> Result<Self::Ok>
        where
            T: ser::Serialize,
    {
        self.current_indent += 1;
        self.push(b'{')?;
        let mut s = SerializeStruct::new(self);
        s.serialize_field(variant, value)?;
        s.end()?;
        Ok(())
    }

    fn serialize_seq(self, _len: Option<usize>) -> Result<Self::SerializeSeq> {
        self.current_indent += 1;
        self.push(b'[')?;

        Ok(SerializeSeq::new(self))
    }

    fn serialize_tuple(self, _len: usize) -> Result<Self::SerializeTuple> {
        self.serialize_seq(Some(_len))
    }

    fn serialize_tuple_struct(
        self,
        _name: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeTupleStruct> {
        unreachable!()
    }

    fn serialize_tuple_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        _variant: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeTupleVariant> {
        unreachable!()
    }

    fn serialize_map(self, _len: Option<usize>) -> Result<Self::SerializeMap> {
        self.current_indent += 1;
        self.push(b'{')?;

        Ok(SerializeMap::new(self))
    }

    fn serialize_struct(self, _name: &'static str, _len: usize) -> Result<Self::SerializeStruct> {
        self.current_indent += 1;
        self.push(b'{')?;

        Ok(SerializeStruct::new(self))
    }

    fn serialize_struct_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        variant: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeStructVariant> {
        self.current_indent += 1;
        self.extend_from_slice(b"{\n")?;
        self.indent()?;
        self.push(b'"')?;
        self.extend_from_slice(variant.as_bytes())?;
        self.extend_from_slice(b"\":{")?;
        self.current_indent += 1;

        Ok(SerializeStructVariant::new(self))
    }

    fn collect_str<T: ?Sized>(self, _value: &T) -> Result<Self::Ok>
        where
            T: fmt::Display,
    {
        unreachable!()
    }
}

/// Serializes the given data structure as a pretty-printed string of JSON text
#[cfg(feature = "heapless")]
pub fn to_string_pretty<T, const N: usize>(value: &T, indent: &[u8]) -> Result<String<N>>
    where
        T: ser::Serialize + ?Sized,
{
    Ok(unsafe { str::from_utf8_unchecked(&to_vec_pretty::<T, N>(value, indent)?) }.into())
}

/// Serializes the given data structure as a pretty-printed JSON byte vector
#[cfg(feature = "heapless")]
pub fn to_vec_pretty<T, const N: usize>(value: &T, indent: &[u8]) -> Result<Vec<u8, N>>
    where
        T: ser::Serialize + ?Sized,
{
    let mut buf = Vec::<u8, N>::new();
    buf.resize_default(N)?;
    let len = to_slice_pretty(value, &mut buf, indent)?;
    buf.truncate(len);
    Ok(buf)
}

/// Serializes the given data structure as a pretty-printed JSON byte vector into the provided buffer
pub fn to_slice_pretty<T>(value: &T, buf: &mut [u8], indent: &[u8]) -> Result<usize>
    where
        T: ser::Serialize + ?Sized,
{
    let mut ser = Serializer::new(buf, indent);
    value.serialize(&mut ser)?;
    Ok(ser.current_length)
}

#[cfg(test)]
mod tests {
    use serde_derive::Serialize;

    const N: usize = 128;
    const INDENT: &[u8] = b"  ";

    #[test]
    fn array() {
        let buf = &mut [0u8; 128];
        let len = crate::to_slice_pretty(&[0, 1, 2], buf, INDENT).unwrap();
        let expected =r#"[
  0,
  1,
  2
]"#;
        assert_eq!(&buf[..len], expected.as_bytes());
        assert_eq!(&*crate::to_string_pretty::<_, N>(&[0, 1, 2], INDENT).unwrap(), expected);
    }

    #[test]
    fn enum_() {
        #[derive(Serialize)]
        enum Type {
            #[serde(rename = "boolean")]
            Boolean,
            #[serde(rename = "number")]
            Number,
        }

        assert_eq!(
            &*crate::to_string_pretty::<_, N>(&Type::Boolean, INDENT).unwrap(),
            r#""boolean""#
        );

        assert_eq!(
            &*crate::to_string_pretty::<_, N>(&Type::Number, INDENT).unwrap(),
            r#""number""#
        );
    }

    #[test]
    fn str() {
        assert_eq!(&*crate::to_string_pretty::<_, N>("hello", INDENT).unwrap(), r#""hello""#);
        assert_eq!(&*crate::to_string_pretty::<_, N>("", INDENT).unwrap(), r#""""#);

        // Characters unescaped if possible
        assert_eq!(&*crate::to_string_pretty::<_, N>("ä", INDENT).unwrap(), r#""ä""#);
        assert_eq!(&*crate::to_string_pretty::<_, N>("৬", INDENT).unwrap(), r#""৬""#);
        // assert_eq!(&*crate::to_string_pretty::<_, N>("\u{A0}").unwrap(), r#"" ""#); // non-breaking space
        assert_eq!(&*crate::to_string_pretty::<_, N>("ℝ", INDENT).unwrap(), r#""ℝ""#); // 3 byte character
        assert_eq!(&*crate::to_string_pretty::<_, N>("💣", INDENT).unwrap(), r#""💣""#); // 4 byte character

        // " and \ must be escaped
        assert_eq!(
            &*crate::to_string_pretty::<_, N>("foo\"bar", INDENT).unwrap(),
            r#""foo\"bar""#
        );
        assert_eq!(
            &*crate::to_string_pretty::<_, N>("foo\\bar", INDENT).unwrap(),
            r#""foo\\bar""#
        );

        // \b, \t, \n, \f, \r must be escaped in their two-character escaping
        assert_eq!(
            &*crate::to_string_pretty::<_, N>(" \u{0008} ", INDENT).unwrap(),
            r#"" \b ""#
        );
        assert_eq!(
            &*crate::to_string_pretty::<_, N>(" \u{0009} ", INDENT).unwrap(),
            r#"" \t ""#
        );
        assert_eq!(
            &*crate::to_string_pretty::<_, N>(" \u{000A} ", INDENT).unwrap(),
            r#"" \n ""#
        );
        assert_eq!(
            &*crate::to_string_pretty::<_, N>(" \u{000C} ", INDENT).unwrap(),
            r#"" \f ""#
        );
        assert_eq!(
            &*crate::to_string_pretty::<_, N>(" \u{000D} ", INDENT).unwrap(),
            r#"" \r ""#
        );

        // U+0000 through U+001F is escaped using six-character \u00xx uppercase hexadecimal escape sequences
        assert_eq!(
            &*crate::to_string_pretty::<_, N>(" \u{0000} ", INDENT).unwrap(),
            r#"" \u0000 ""#
        );
        assert_eq!(
            &*crate::to_string_pretty::<_, N>(" \u{0001} ", INDENT).unwrap(),
            r#"" \u0001 ""#
        );
        assert_eq!(
            &*crate::to_string_pretty::<_, N>(" \u{0007} ", INDENT).unwrap(),
            r#"" \u0007 ""#
        );
        assert_eq!(
            &*crate::to_string_pretty::<_, N>(" \u{000e} ", INDENT).unwrap(),
            r#"" \u000E ""#
        );
        assert_eq!(
            &*crate::to_string_pretty::<_, N>(" \u{001D} ", INDENT).unwrap(),
            r#"" \u001D ""#
        );
        assert_eq!(
            &*crate::to_string_pretty::<_, N>(" \u{001f} ", INDENT).unwrap(),
            r#"" \u001F ""#
        );
    }

    #[test]
    fn struct_bool() {
        #[derive(Serialize)]
        struct Led {
            led: bool,
        }

        assert_eq!(
            &*crate::to_string_pretty::<_, N>(&Led { led: true }, INDENT).unwrap(),
            r#"{
  "led":true
}"#
        );
    }

    #[test]
    fn struct_i8() {
        #[derive(Serialize)]
        struct Temperature {
            temperature: i8,
        }

        assert_eq!(
            &*crate::to_string_pretty::<_, N>(&Temperature { temperature: 127 }, INDENT).unwrap(),
            r#"{
  "temperature":127
}"#
        );

        assert_eq!(
            &*crate::to_string_pretty::<_, N>(&Temperature { temperature: 20 }, INDENT).unwrap(),
            r#"{
  "temperature":20
}"#
        );

        assert_eq!(
            &*crate::to_string_pretty::<_, N>(&Temperature { temperature: -17 }, INDENT).unwrap(),
            r#"{
  "temperature":-17
}"#
        );

        assert_eq!(
            &*crate::to_string_pretty::<_, N>(&Temperature { temperature: -128 }, INDENT).unwrap(),
            r#"{
  "temperature":-128
}"#
        );
    }

    #[test]
    fn struct_f32() {
        #[derive(Serialize)]
        struct Temperature {
            temperature: f32,
        }

        assert_eq!(
            &*crate::to_string_pretty::<_, N>(&Temperature { temperature: -20. }, INDENT).unwrap(),
            r#"{
  "temperature":-20.0
}"#
        );

        assert_eq!(
            &*crate::to_string_pretty::<_, N>(&Temperature {
                temperature: -20345.
            }, INDENT)
                .unwrap(),
            r#"{
  "temperature":-20345.0
}"#
        );

        assert_eq!(
            &*crate::to_string_pretty::<_, N>(&Temperature {
                temperature: -2.3456789012345e-23
            }, INDENT)
                .unwrap(),
            r#"{
  "temperature":-2.3456788e-23
}"#
        );

        assert_eq!(
            &*crate::to_string_pretty::<_, N>(&Temperature {
                temperature: f32::NAN
            }, INDENT)
                .unwrap(),
            r#"{
  "temperature":null
}"#
        );

        assert_eq!(
            &*crate::to_string_pretty::<_, N>(&Temperature {
                temperature: f32::NEG_INFINITY
            }, INDENT)
                .unwrap(),
            r#"{
  "temperature":null
}"#
        );
    }

    #[test]
    fn struct_option() {
        #[derive(Serialize)]
        struct Property<'a> {
            description: Option<&'a str>,
        }

        assert_eq!(
            crate::to_string_pretty::<_, N>(&Property {
                description: Some("An ambient temperature sensor"),
            }, INDENT)
                .unwrap(),
            r#"{
  "description":"An ambient temperature sensor"
}"#
        );

        // XXX Ideally this should produce "{}"
        assert_eq!(
            crate::to_string_pretty::<_, N>(&Property { description: None }, INDENT).unwrap(),
            r#"{
  "description":null
}"#
        );
    }

    #[test]
    fn struct_u8() {
        #[derive(Serialize)]
        struct Temperature {
            temperature: u8,
        }

        assert_eq!(
            &*crate::to_string_pretty::<_, N>(&Temperature { temperature: 20 }, INDENT).unwrap(),
            r#"{
  "temperature":20
}"#
        );
    }

    #[test]
    fn struct_() {
        #[derive(Serialize)]
        struct Empty {}

        assert_eq!(&*crate::to_string_pretty::<_, N>(&Empty {}, INDENT).unwrap(), r#"{}"#);

        #[derive(Serialize)]
        struct Tuple {
            a: bool,
            b: bool,
        }

        assert_eq!(
            &*crate::to_string_pretty::<_, N>(&Tuple { a: true, b: false }, INDENT).unwrap(),
            r#"{
  "a":true,
  "b":false
}"#
        );
    }

    #[test]
    fn test_unit() {
        let a = ();
        assert_eq!(&*crate::to_string_pretty::<_, N>(&a, INDENT).unwrap(), r#"null"#);
    }

    #[test]
    fn test_newtype_struct() {
        #[derive(Serialize)]
        struct A(pub u32);
        let a = A(54);
        assert_eq!(&*crate::to_string_pretty::<_, N>(&a, INDENT).unwrap(), r#"54"#);
    }

    #[test]
    fn test_newtype_variant() {
        #[derive(Serialize)]
        enum A {
            A(u32),
        }
        let a = A::A(54);

        assert_eq!(&*crate::to_string_pretty::<_, N>(&a, INDENT).unwrap(), r#"{
  "A":54
}"#);
    }

    #[test]
    fn test_struct_variant() {
        #[derive(Serialize)]
        enum A {
            A { x: u32, y: u16 },
        }
        let a = A::A { x: 54, y: 720 };

        assert_eq!(
            &*crate::to_string_pretty::<_, N>(&a, INDENT).unwrap(),
            r#"{
  "A":{
    "x":54,
    "y":720
  }
}"#
        );
    }

    #[test]
    fn test_serialize_bytes() {
        use core::fmt::Write;
        use heapless::String;

        pub struct SimpleDecimal(f32);

        impl serde::Serialize for SimpleDecimal {
            fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
                where
                    S: serde::Serializer,
            {
                let mut aux: String<{ N }> = String::new();
                write!(aux, "{:.2}", self.0).unwrap();
                serializer.serialize_bytes(&aux.as_bytes())
            }
        }

        let sd1 = SimpleDecimal(1.55555);
        assert_eq!(&*crate::to_string_pretty::<_, N>(&sd1, INDENT).unwrap(), r#"1.56"#);

        let sd2 = SimpleDecimal(0.000);
        assert_eq!(&*crate::to_string_pretty::<_, N>(&sd2, INDENT).unwrap(), r#"0.00"#);

        let sd3 = SimpleDecimal(22222.777777);
        assert_eq!(&*crate::to_string_pretty::<_, N>(&sd3, INDENT).unwrap(), r#"22222.78"#);
    }
}
