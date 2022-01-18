//! Serialize a Rust data structure into JSON data

use core::{fmt, str};

use serde::ser;
//use serde::ser::SerializeStruct as _;

#[cfg(feature = "heapless")]
use heapless::{String, Vec};

//use self::map::SerializeMap;
//use self::seq::SerializeSeq;
//use self::struct_::{SerializeStruct, SerializeStructVariant};

//mod map;
//mod seq;
//mod struct_;

/// Serialization result
pub type Result<T> = ::core::result::Result<T, Error>;

/// This type represents all possible errors that can occur when serializing JSON data
#[derive(Debug)]
#[non_exhaustive]
pub enum Error {
    /// Buffer is full
    BufferFull,
    /// Key is not a string
    KeyIsNotAString,
}

impl From<()> for Error {
    fn from(_: ()) -> Error {
        Error::BufferFull
    }
}

impl From<u8> for Error {
    fn from(_: u8) -> Error {
        Error::BufferFull
    }
}

#[cfg(feature = "std")]
impl ::std::error::Error for Error {
    fn description(&self) -> &str {
        ""
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Buffer is full")
    }
}

const BB: u8 = b'b'; // \x08
const TT: u8 = b't'; // \x09
const NN: u8 = b'n'; // \x0A
const FF: u8 = b'f'; // \x0C
const RR: u8 = b'r'; // \x0D
const QU: u8 = b'"'; // \x22
const BS: u8 = b'\\'; // \x5C
const UU: u8 = b'u'; // \x00...\x1F except the ones above
const __: u8 = 0;

// Lookup table of escape sequences. A value of b'x' at index i means that byte
// i is escaped as "\x" in JSON. A value of 0 means that byte i is not escaped.
static ESCAPE: [u8; 256] = [
    //   1   2   3   4   5   6   7   8   9   A   B   C   D   E   F
    UU, UU, UU, UU, UU, UU, UU, UU, BB, TT, NN, UU, FF, RR, UU, UU, // 0
    UU, UU, UU, UU, UU, UU, UU, UU, UU, UU, UU, UU, UU, UU, UU, UU, // 1
    __, __, QU, __, __, __, __, __, __, __, __, __, __, __, __, __, // 2
    __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // 3
    __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // 4
    __, __, __, __, __, __, __, __, __, __, __, __, BS, __, __, __, // 5
    __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // 6
    __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // 7
    __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // 8
    __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // 9
    __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // A
    __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // B
    __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // C
    __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // D
    __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // E
    __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // F
];

/// Represents a character escape code in a type-safe manner.
pub enum CharEscape {
    /// An escaped quote `"`
    Quote,
    /// An escaped reverse solidus `\`
    ReverseSolidus,
    /// An escaped solidus `/`
    Solidus,
    /// An escaped backspace character (usually escaped as `\b`)
    Backspace,
    /// An escaped form feed character (usually escaped as `\f`)
    FormFeed,
    /// An escaped line feed character (usually escaped as `\n`)
    LineFeed,
    /// An escaped carriage return character (usually escaped as `\r`)
    CarriageReturn,
    /// An escaped tab character (usually escaped as `\t`)
    Tab,
    /// An escaped ASCII plane control character (usually escaped as
    /// `\u00XX` where `XX` are two hex characters)
    AsciiControl(u8),
}

impl CharEscape {
    #[inline]
    fn from_escape_table(escape: u8, byte: u8) -> CharEscape {
        match escape {
            self::BB => CharEscape::Backspace,
            self::TT => CharEscape::Tab,
            self::NN => CharEscape::LineFeed,
            self::FF => CharEscape::FormFeed,
            self::RR => CharEscape::CarriageReturn,
            self::QU => CharEscape::Quote,
            self::BS => CharEscape::ReverseSolidus,
            self::UU => CharEscape::AsciiControl(byte),
            _ => unreachable!(),
        }
    }
}

// NOTE(serialize_*signed) This is basically the numtoa implementation minus the lookup tables,
// which take 200+ bytes of ROM / Flash
macro_rules! serialize_unsigned {
    ($writer:ident, $N:expr, $v:expr) => {{
        let mut buf: [u8; $N] = unsafe { super::uninitialized() };

        let mut v = $v;
        let mut i = $N - 1;
        loop {
            buf[i] = (v % 10) as u8 + b'0';
            v /= 10;

            if v == 0 {
                break;
            } else {
                i -= 1;
            }
        }

        $writer.extend_from_slice(&buf[i..])
    }};
}

macro_rules! serialize_signed {
    ($writer:ident, $N:expr, $v:expr, $ixx:ident, $uxx:ident) => {{
        let v = $v;
        let (signed, mut v) = if v == $ixx::min_value() {
            (true, $ixx::max_value() as $uxx + 1)
        } else if v < 0 {
            (true, -v as $uxx)
        } else {
            (false, v as $uxx)
        };

        let mut buf: [u8; $N] = unsafe { super::uninitialized() };
        let mut i = $N - 1;
        loop {
            buf[i] = (v % 10) as u8 + b'0';
            v /= 10;

            i -= 1;

            if v == 0 {
                break;
            }
        }

        if signed {
            buf[i] = b'-';
        } else {
            i += 1;
        }
        $writer.extend_from_slice(&buf[i..])
    }};
}

macro_rules! serialize_ryu {
    ($writer:ident, $v:expr) => {{
        let mut buffer = ryu::Buffer::new();
        let printed = buffer.format($v);
        $writer.extend_from_slice(printed.as_bytes())
    }};
}

pub struct Writer<'a> {
    buf: &'a mut [u8],
    index: usize,
}

impl<'a> Writer<'a> {
    pub fn with_slice(buf: &'a mut [u8]) -> Self {
        Self{buf, index: 0}
    }

    pub fn into_inner(self) -> &'a[u8] {
        &self.buf[..self.index]
    }

    pub fn push(&mut self, c: u8) -> Result<()> {
        if self.index < self.buf.len() {
            unsafe {self.push_unchecked(c)}
            Ok(())
        }
        else {
            Err(Error::BufferFull)
        }
    }

    unsafe fn push_unchecked(&mut self, c: u8) {
        self.buf[self.index] = c;
        self.index += 1;
    }

    fn extend_from_slice(&mut self, other: &[u8]) -> Result<()> {
        let required_size = other.len();
        if (self.buf.len() - self.index) < required_size {
            // won't fit in the buf; don't modify anything and return an error
            Err(Error::BufferFull)
        } else {
            for byte in other {
                unsafe{self.push_unchecked(*byte)}
            }
            Ok(())
        }
    }

}

/// This trait abstracts away serializing the JSON control characters, which allows the user to
/// optionally pretty print the JSON output.
pub trait Formatter {

    /// Writes a `null` value to the specified writer.
    #[inline]
    fn write_null(&mut self, writer: & mut Writer<'_>) -> Result<()>
    {
        writer.extend_from_slice(b"null")
    }

    /// Writes a `true` or `false` value to the specified writer.
    #[inline]
    fn write_bool(&mut self, writer: & mut Writer<'_>, value: bool) -> Result<()>
    {
        let s = if value {
            b"true" as &[u8]
        } else {
            b"false" as &[u8]
        };
        writer.extend_from_slice( s)
    }

    /// Writes an integer value like `-123` to the specified writer.
    #[inline]
    fn write_i8(&mut self, writer: & mut Writer<'_>, value: i8) -> Result<()>
    {
        serialize_signed!(writer, 4, value, i8, u8)
    }

    /// Writes an integer value like `-123` to the specified writer.
    #[inline]
    fn write_i16(&mut self, writer: & mut Writer<'_>, value: i16) -> Result<()>
    {
        serialize_signed!(writer, 6, value, i16, u16)
    }

    /// Writes an integer value like `-123` to the specified writer.
    #[inline]
    fn write_i32(&mut self, writer: & mut Writer<'_>, value: i32) -> Result<()>
    {
        serialize_signed!(writer, 11, value, i32, u32)
    }

    /// Writes an integer value like `-123` to the specified writer.
    #[inline]
    fn write_i64(&mut self, writer: & mut Writer<'_>, value: i64) -> Result<()>
    {
        serialize_signed!(writer, 20, value, i64, u64)
    }

    /// Writes an integer value like `123` to the specified writer.
    #[inline]
    fn write_u8(&mut self, writer: & mut Writer<'_>, value: u8) -> Result<()>
    {
        serialize_unsigned!(writer, 3, value)
    }

    /// Writes an integer value like `123` to the specified writer.
    #[inline]
    fn write_u16(&mut self, writer: & mut Writer<'_>, value: u16) -> Result<()>
    {
        serialize_unsigned!(writer, 5, value)
    }

    /// Writes an integer value like `123` to the specified writer.
    #[inline]
    fn write_u32(&mut self, writer: & mut Writer<'_>, value: u32) -> Result<()>
    {
        serialize_unsigned!(writer, 10, value)
    }

    /// Writes an integer value like `123` to the specified writer.
    #[inline]
    fn write_u64(&mut self, writer: & mut Writer<'_>, value: u64) -> Result<()>
    {
        serialize_unsigned!(writer, 20, value)
    }

    /// Writes a floating point value like `-31.26e+12` to the specified writer.
    #[inline]
    fn write_f32(&mut self, writer: & mut Writer<'_>, value: f32) -> Result<()>
    {
        serialize_ryu!(writer, value)
    }

    /// Writes a floating point value like `-31.26e+12` to the specified writer.
    #[inline]
    fn write_f64(&mut self, writer: & mut Writer<'_>, value: f64) -> Result<()>
    {
        serialize_ryu!(writer, value)
    }

    /// Writes a number that has already been rendered to a string.
    #[inline]
    fn write_number_str(&mut self, writer: & mut Writer<'_>, value: &str) -> Result<()>
    {
        writer.extend_from_slice(value.as_bytes())
    }

    /// Called before each series of `write_string_fragment` and
    /// `write_char_escape`.  Writes a `"` to the specified writer.
    #[inline]
    fn begin_string(&mut self, writer: & mut Writer<'_>) -> Result<()>
    {
        writer.extend_from_slice(b"\"")
    }

    /// Called after each series of `write_string_fragment` and
    /// `write_char_escape`.  Writes a `"` to the specified writer.
    #[inline]
    fn end_string(&mut self, writer: & mut Writer<'_>) -> Result<()>
    {
        writer.extend_from_slice(b"\"")
    }

    /// Writes a string fragment that doesn't need any escaping to the
    /// specified writer.
    #[inline]
    fn write_string_fragment(&mut self, writer: & mut Writer<'_>, fragment: &str) -> Result<()>
    {
        writer.extend_from_slice(fragment.as_bytes())
    }

    /// Writes a character escape code to the specified writer.
    #[inline]
    fn write_char_escape(&mut self, writer: & mut Writer<'_>, char_escape: CharEscape) -> Result<()>
    {
        use self::CharEscape::*;

        let s = match char_escape {
            Quote => b"\\\"",
            ReverseSolidus => b"\\\\",
            Solidus => b"\\/",
            Backspace => b"\\b",
            FormFeed => b"\\f",
            LineFeed => b"\\n",
            CarriageReturn => b"\\r",
            Tab => b"\\t",
            AsciiControl(byte) => {
                static HEX_DIGITS: [u8; 16] = *b"0123456789ABCDEF";
                let bytes = &[
                    b'\\',
                    b'u',
                    b'0',
                    b'0',
                    HEX_DIGITS[(byte >> 4) as usize],
                    HEX_DIGITS[(byte & 0xF) as usize],
                ];
                return writer.extend_from_slice(bytes);
            }
        };

        writer.extend_from_slice(s)
    }

    /// Called before every array.  Writes a `[` to the specified
    /// writer.
    #[inline]
    fn begin_array(&mut self, writer: & mut Writer<'_>) -> Result<()>
    {
        writer.extend_from_slice(b"[")
    }

    /// Called after every array.  Writes a `]` to the specified
    /// writer.
    #[inline]
    fn end_array(&mut self, writer: & mut Writer<'_>) -> Result<()>
    {
        writer.extend_from_slice(b"]")
    }

    /// Called before every array value.  Writes a `,` if needed to
    /// the specified writer.
    #[inline]
    fn begin_array_value(&mut self, writer: & mut Writer<'_>, first: bool) -> Result<()>
    {
        if first {
            Ok(())
        } else {
            writer.extend_from_slice(b",")
        }
    }

    /// Called after every array value.
    #[inline]
    fn end_array_value(&mut self, _writer: & mut Writer<'_>) -> Result<()>
    {
        Ok(())
    }

    /// Called before every object.  Writes a `{` to the specified
    /// writer.
    #[inline]
    fn begin_object(&mut self, writer: & mut Writer<'_>) -> Result<()>
    {
        writer.extend_from_slice(b"{")
    }

    /// Called after every object.  Writes a `}` to the specified
    /// writer.
    #[inline]
    fn end_object(&mut self, writer: & mut Writer<'_>) -> Result<()>
    {
        writer.extend_from_slice(b"}")
    }

    /// Called before every object key.
    #[inline]
    fn begin_object_key(&mut self, writer: & mut Writer<'_>, first: bool) -> Result<()>
    {
        if first {
            Ok(())
        } else {
            writer.extend_from_slice(b",")
        }
    }

    /// Called after every object key.  A `:` should be written to the
    /// specified writer by either this method or
    /// `begin_object_value`.
    #[inline]
    fn end_object_key(&mut self, _writer: & mut Writer<'_>) -> Result<()>
    {
        Ok(())
    }

    /// Called before every object value.  A `:` should be written to
    /// the specified writer by either this method or
    /// `end_object_key`.
    #[inline]
    fn begin_object_value(&mut self, writer: & mut Writer<'_>) -> Result<()>
    {
        writer.extend_from_slice( b":")
    }

    /// Called after every object value.
    #[inline]
    fn end_object_value(&mut self, _writer: & mut Writer<'_>) -> Result<()>
    {
        Ok(())
    }

    /// Writes a raw JSON fragment that doesn't need any escaping to the
    /// specified writer.
    #[inline]
    fn write_raw_fragment(&mut self, writer: & mut Writer<'_>, fragment: &str) -> Result<()>
    {
        writer.extend_from_slice(fragment.as_bytes())
    }
}

/// This structure compacts a JSON value with no extra whitespace.
#[derive(Clone, Debug)]
pub struct CompactFormatter;

impl Formatter for CompactFormatter {}

/// This structure pretty prints a JSON value to make it human readable.
#[derive(Clone, Debug)]
pub struct PrettyFormatter<'a> {
    current_indent: usize,
    has_value: bool,
    indent: &'a [u8],
}

fn indent(writer: & mut Writer<'_>, n: usize, s: &[u8]) -> Result<()> {
    for _ in 0..n {
        writer.extend_from_slice(s)?;
    }
    Ok(())
}

impl<'a> PrettyFormatter<'a> {
    /// Construct a pretty printer formatter that defaults to using two spaces for indentation.
    pub fn new() -> Self {
        PrettyFormatter::with_indent(b"  ")
    }

    /// Construct a pretty printer formatter that uses the `indent` string for indentation.
    pub fn with_indent(indent: &'a [u8]) -> Self {
        PrettyFormatter {
            current_indent: 0,
            has_value: false,
            indent,
        }
    }

}

impl<'a> Default for PrettyFormatter<'a> {
    fn default() -> Self {
        PrettyFormatter::new()
    }
}

impl<'a> Formatter for PrettyFormatter<'a> {
    #[inline]
    fn begin_array(&mut self, writer: & mut Writer<'_>) -> Result<()>
    {
        self.current_indent += 1;
        self.has_value = false;
        writer.extend_from_slice(b"[")
    }

    #[inline]
    fn end_array(&mut self, writer: & mut Writer<'_>) -> Result<()>
    {
        self.current_indent -= 1;

        if self.has_value {
            writer.extend_from_slice(b"\n")?;
            indent(writer, self.current_indent, self.indent)?;
        }

        writer.extend_from_slice( b"]")
    }

    #[inline]
    fn begin_array_value(&mut self, writer: &mut Writer<'_>, first: bool) -> Result<()>
    {
        match first {
            true => writer.extend_from_slice( b"\n"),
            false => writer.extend_from_slice(b",\n"),
        }?;

        indent(writer, self.current_indent, self.indent)
    }

    #[inline]
    fn end_array_value(&mut self, _writer: &mut Writer<'_>) -> Result<()>
    {
        self.has_value = true;
        Ok(())
    }

    #[inline]
    fn begin_object(&mut self, writer: &mut Writer<'_>) -> Result<()>
    {
        self.current_indent += 1;
        self.has_value = false;
        writer.extend_from_slice(b"{")
    }

    #[inline]
    fn end_object(&mut self, writer: &mut Writer<'_>) -> Result<()>
    {
        self.current_indent -= 1;

        if self.has_value {
            writer.extend_from_slice(b"\n")?;
            indent(writer, self.current_indent, self.indent)?;
        }

        writer.extend_from_slice(b"}")
    }

    #[inline]
    fn begin_object_key(&mut self, writer: & mut Writer<'_>, first: bool) -> Result<()>
    {
        match first {
            true => writer.extend_from_slice(b"\n"),
            false => writer.extend_from_slice(b",\n"),
        }?;

        indent(writer, self.current_indent, self.indent)
    }

    #[inline]
    fn begin_object_value(&mut self, writer: & mut Writer<'_>) -> Result<()>
    {
        writer.extend_from_slice( b": ")
    }

    #[inline]
    fn end_object_value(&mut self, _writer: & mut Writer<'_>) -> Result<()>
    {
        self.has_value = true;
        Ok(())
    }
}



pub struct Serializer<'a, F> {
    writer: Writer<'a>,
    formatter: F,
}

impl<'a, F> Serializer<'a, F>
where
F: Formatter
{
    pub fn with_formatter(buf: &'a mut[u8], formatter: F) -> Self {
        let writer = Writer::with_slice(buf);
        Serializer{writer, formatter}
    }

    pub fn into_inner(self) -> &'a [u8] {
        self.writer.into_inner()
    }
}

/// Upper-case hex for value in 0..16, encoded as ASCII bytes
#[allow(unused)]
fn hex_4bit(c: u8) -> u8 {
    if c <= 9 {
        0x30 + c
    } else {
        0x41 + (c - 10)
    }
}

/// Upper-case hex for value in 0..256, encoded as ASCII bytes
#[allow(unused)]
fn hex(c: u8) -> (u8, u8) {
    (hex_4bit(c >> 4), hex_4bit(c & 0x0F))
}

impl<'a, 'b: 'a, F> ser::Serializer for &'a mut Serializer<'b, F>
where F: Formatter
{
    type Ok = ();
    type Error = Error;
    type SerializeSeq = Compound<'a, 'b, F>;
    type SerializeTuple = Compound<'a, 'b, F>;
    type SerializeTupleStruct = Compound<'a, 'b, F>;
    type SerializeTupleVariant = Compound<'a, 'b, F>;
    type SerializeMap = Compound<'a, 'b, F>;
    type SerializeStruct = Compound<'a, 'b, F>;
    type SerializeStructVariant = Compound<'a, 'b, F>;

    fn serialize_bool(self, v: bool) -> Result<Self::Ok> {
        self.formatter.write_bool(&mut self.writer, v)
    }

    fn serialize_i8(self, v: i8) -> Result<Self::Ok> {
        self.formatter.write_i8(&mut self.writer, v)
    }

    fn serialize_i16(self, v: i16) -> Result<Self::Ok> {
        self.formatter.write_i16(&mut self.writer, v)
    }

    fn serialize_i32(self, v: i32) -> Result<Self::Ok> {
        self.formatter.write_i32(&mut self.writer, v)
    }

    fn serialize_i64(self, v: i64) -> Result<Self::Ok> {
        self.formatter.write_i64(&mut self.writer, v)
    }

    fn serialize_u8(self, v: u8) -> Result<Self::Ok> {
        self.formatter.write_u8(&mut self.writer, v)
    }

    fn serialize_u16(self, v: u16) -> Result<Self::Ok> {
        self.formatter.write_u16(&mut self.writer, v)
    }

    fn serialize_u32(self, v: u32) -> Result<Self::Ok> {
        self.formatter.write_u32(&mut self.writer, v)
    }

    fn serialize_u64(self, v: u64) -> Result<Self::Ok> {
        self.formatter.write_u64(&mut self.writer, v)
    }

    fn serialize_f32(self, v: f32) -> Result<Self::Ok> {
        self.formatter.write_f32(&mut self.writer, v)
    }

    fn serialize_f64(self, v: f64) -> Result<Self::Ok> {
        self.formatter.write_f64(&mut self.writer, v)
    }

    fn serialize_char(self, _v: char) -> Result<Self::Ok> {
        unreachable!()
    }

    fn serialize_str(self, v: &str) -> Result<Self::Ok> {
        self.formatter.begin_string(&mut self.writer)?;

            let bytes = v.as_bytes();

            let mut start = 0;

            for (i, &byte) in bytes.iter().enumerate() {
                let escape = ESCAPE[byte as usize];
                if escape == 0 {
                    continue;
                }

                if start < i {
                    self.formatter.write_string_fragment(&mut self.writer, &v[start..i])?;
                }

                let char_escape = CharEscape::from_escape_table(escape, byte);
                self.formatter.write_char_escape(&mut self.writer, char_escape)?;

                start = i + 1;
            }

            if start != bytes.len() {
                self.formatter.write_string_fragment(&mut self.writer, &v[start..])?;
            }



        self.formatter.end_string(&mut self.writer)
    }

    fn serialize_bytes(self, v: &[u8]) -> Result<Self::Ok> {
        use serde::ser::SerializeSeq;

        let mut seq = self.serialize_seq(Some(v.len()))?;
        for byte in v {
            seq.serialize_element(byte)?;
        }
        SerializeSeq::end(seq)
    }

    fn serialize_none(self) -> Result<Self::Ok> {
        self.formatter.write_null(&mut self.writer)
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
        self.formatter.begin_object(&mut self.writer)?;
        self.formatter.begin_object_key(&mut self.writer, true)?;
        self.serialize_str(variant)?;
        self.formatter.begin_object_value(&mut self.writer)?;
        value.serialize(&mut *self)?;
        self.formatter.end_object_value(&mut self.writer)?;
        self.formatter.end_object(&mut self.writer)
    }

    fn serialize_seq(self, len: Option<usize>) -> Result<Self::SerializeSeq> {
        self
            .formatter
            .begin_array(&mut self.writer)?;
        if len == Some(0) {
            self
                .formatter
                .end_array(&mut self.writer)?;
            Ok(Compound::Map {
                ser: self,
                state: State::Empty,
            })
        } else {
            Ok(Compound::Map {
                ser: self,
                state: State::First,
            })
        }
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
        variant: &'static str,
        len: usize,
    ) -> Result<Self::SerializeTupleVariant> {
        self
            .formatter
            .begin_object(&mut self.writer)?;
        self
            .formatter
            .begin_object_key(&mut self.writer, true)?;
        self.serialize_str(variant)?;
        self
            .formatter
            .end_object_key(&mut self.writer)?;
        self
            .formatter
            .begin_object_value(&mut self.writer)?;
        self.serialize_seq(Some(len))
    }

    fn serialize_map(self, len: Option<usize>) -> Result<Self::SerializeMap> {
        self
            .formatter
            .begin_object(&mut self.writer)?;
        if len == Some(0) {
            self
                .formatter
                .end_object(&mut self.writer)?;
            Ok(Compound::Map {
                ser: self,
                state: State::Empty,
            })
        } else {
            Ok(Compound::Map {
                ser: self,
                state: State::First,
            })
        }


    }

    fn serialize_struct(self, _name: &'static str, len: usize) -> Result<Self::SerializeStruct> {
        self.serialize_map(Some(len))
    }

    fn serialize_struct_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        variant: &'static str,
        len: usize,
    ) -> Result<Self::SerializeStructVariant> {
        self
            .formatter
            .begin_object(&mut self.writer)?;
        self
            .formatter
            .begin_object_key(&mut self.writer, true)?;
        self.serialize_str(variant)?;
        self
            .formatter
            .end_object_key(&mut self.writer)?;
        self
            .formatter
            .begin_object_value(&mut self.writer)?;
        self.serialize_map(Some(len))
    }

    fn collect_str<T: ?Sized>(self, _value: &T) -> Result<Self::Ok>
    where
        T: fmt::Display,
    {
        unreachable!()
    }
}

// Not public API. Should be pub(crate).
#[doc(hidden)]
#[derive(Eq, PartialEq)]
pub enum State {
    Empty,
    First,
    Rest,
}

// Not public API. Should be pub(crate).
#[doc(hidden)]
pub enum Compound<'a, 'b, F> {
    Map {
        ser: &'a mut Serializer<'b, F>,
        state: State,
    },
}

impl<'a, 'b, F> ser::SerializeSeq for Compound<'a, 'b, F>
    where
        F: Formatter,
{
    type Ok = ();
    type Error = Error;

    #[inline]
    fn serialize_element<T>(&mut self, value: &T) -> Result<()>
        where
            T: ?Sized + ser::Serialize,
    {
        match *self {
            Compound::Map {
                ref mut ser,
                ref mut state,
            } => {
                ser
                    .formatter
                    .begin_array_value(&mut ser.writer, *state == State::First)?;
                *state = State::Rest;
                value.serialize(&mut **ser)?;
                ser
                    .formatter
                    .end_array_value(&mut ser.writer)
            }
            #[cfg(feature = "arbitrary_precision")]
            Compound::Number { .. } => unreachable!(),
            #[cfg(feature = "raw_value")]
            Compound::RawValue { .. } => unreachable!(),
        }
    }

    #[inline]
    fn end(self) -> Result<()> {
        match self {
            Compound::Map { ser, state } => {
                match state {
                    State::Empty => {}
                    _ => ser.formatter.end_array(&mut ser.writer)?
                };
                Ok(())
            }
            #[cfg(feature = "arbitrary_precision")]
            Compound::Number { .. } => unreachable!(),
            #[cfg(feature = "raw_value")]
            Compound::RawValue { .. } => unreachable!(),
        }
    }
}

impl<'a, 'b, F> ser::SerializeTuple for Compound<'a,  'b,F>
where
        F: Formatter,
{
    type Ok = ();
    type Error = Error;

    #[inline]
    fn serialize_element<T>(&mut self, value: &T) -> Result<()>
        where
            T: ?Sized + ser::Serialize,
    {
        ser::SerializeSeq::serialize_element(self, value)
    }

    #[inline]
    fn end(self) -> Result<()> {
        ser::SerializeSeq::end(self)
    }
}

impl<'a,  'b,F> ser::SerializeTupleStruct for Compound<'a,  'b,F>
    where
        F: Formatter,
{
    type Ok = ();
    type Error = Error;

    #[inline]
    fn serialize_field<T>(&mut self, value: &T) -> Result<()>
        where
            T: ?Sized + ser::Serialize,
    {
        ser::SerializeSeq::serialize_element(self, value)
    }

    #[inline]
    fn end(self) -> Result<()> {
        ser::SerializeSeq::end(self)
    }
}

impl<'a, 'b, F> ser::SerializeTupleVariant for Compound<'a, 'b, F>
    where
        F: Formatter,
{
    type Ok = ();
    type Error = Error;

    #[inline]
    fn serialize_field<T>(&mut self, value: &T) -> Result<()>
        where
            T: ?Sized + ser::Serialize,
    {
        ser::SerializeSeq::serialize_element(self, value)
    }

    #[inline]
    fn end(self) -> Result<()> {
        match self {
            Compound::Map { ser, state } => {
                match state {
                    State::Empty => {}
                    _ => ser.formatter.end_array(&mut ser.writer)?
                };

                ser
                    .formatter
                    .end_object_value(&mut ser.writer)?;
                ser.formatter.end_object(&mut ser.writer)
            }
            #[cfg(feature = "arbitrary_precision")]
            Compound::Number { .. } => unreachable!(),
            #[cfg(feature = "raw_value")]
            Compound::RawValue { .. } => unreachable!(),
        }
    }
}

impl<'a,  'b,F> ser::SerializeMap for Compound<'a,  'b,F>
    where
        F: Formatter,
{
    type Ok = ();
    type Error = Error;

    #[inline]
    fn serialize_key<T>(&mut self, key: &T) -> Result<()>
        where
            T: ?Sized + ser::Serialize,
    {
        match *self {
            Compound::Map {
                ref mut ser,
                ref mut state,
            } => {
                ser
                    .formatter
                    .begin_object_key(&mut ser.writer, *state == State::First)?;
                *state = State::Rest;

                key.serialize(MapKeySerializer { ser: *ser })?;

                ser
                    .formatter
                    .end_object_key(&mut ser.writer)
            }
            #[cfg(feature = "arbitrary_precision")]
            Compound::Number { .. } => unreachable!(),
            #[cfg(feature = "raw_value")]
            Compound::RawValue { .. } => unreachable!(),
        }
    }

    #[inline]
    fn serialize_value<T>(&mut self, value: &T) -> Result<()>
        where
            T: ?Sized + ser::Serialize,
    {
        match *self {
            Compound::Map { ref mut ser, .. } => {
                ser
                    .formatter
                    .begin_object_value(&mut ser.writer)?;
                value.serialize(&mut **ser)?;
                ser
                    .formatter
                    .end_object_value(&mut ser.writer)
            }
            #[cfg(feature = "arbitrary_precision")]
            Compound::Number { .. } => unreachable!(),
            #[cfg(feature = "raw_value")]
            Compound::RawValue { .. } => unreachable!(),
        }
    }

    #[inline]
    fn end(self) -> Result<()> {
        match self {
            Compound::Map { ser, state } => {
                match state {
                    State::Empty => {}
                    _ => ser.formatter.end_object(&mut ser.writer)?
                };
                Ok(())
            }
            #[cfg(feature = "arbitrary_precision")]
            Compound::Number { .. } => unreachable!(),
            #[cfg(feature = "raw_value")]
            Compound::RawValue { .. } => unreachable!(),
        }
    }
}

impl<'a,  'b,F> ser::SerializeStruct for Compound<'a,  'b,F>
    where
        F: Formatter,
{
    type Ok = ();
    type Error = Error;

    #[inline]
    fn serialize_field<T>(&mut self, key: &'static str, value: &T) -> Result<()>
        where
            T: ?Sized + ser::Serialize,
    {
        match *self {
            Compound::Map { .. } => ser::SerializeMap::serialize_entry(self, key, value),
            #[cfg(feature = "arbitrary_precision")]
            Compound::Number { ref mut ser, .. } => {
                if key == crate::number::TOKEN {
                    tri!(value.serialize(NumberStrEmitter(ser)));
                    Ok(())
                } else {
                    Err(invalid_number())
                }
            }
            #[cfg(feature = "raw_value")]
            Compound::RawValue { ref mut ser, .. } => {
                if key == crate::raw::TOKEN {
                    tri!(value.serialize(RawValueStrEmitter(ser)));
                    Ok(())
                } else {
                    Err(invalid_raw_value())
                }
            }
        }
    }

    #[inline]
    fn end(self) -> Result<()> {
        match self {
            Compound::Map { .. } => ser::SerializeMap::end(self),
            #[cfg(feature = "arbitrary_precision")]
            Compound::Number { .. } => Ok(()),
            #[cfg(feature = "raw_value")]
            Compound::RawValue { .. } => Ok(()),
        }
    }
}

impl<'a,  'b,F> ser::SerializeStructVariant for Compound<'a,  'b,F>
    where
        F: Formatter,
{
    type Ok = ();
    type Error = Error;

    #[inline]
    fn serialize_field<T>(&mut self, key: &'static str, value: &T) -> Result<()>
        where
            T: ?Sized + ser::Serialize,
    {
        match *self {
            Compound::Map { .. } => ser::SerializeStruct::serialize_field(self, key, value),
            #[cfg(feature = "arbitrary_precision")]
            Compound::Number { .. } => unreachable!(),
            #[cfg(feature = "raw_value")]
            Compound::RawValue { .. } => unreachable!(),
        }
    }

    #[inline]
    fn end(self) -> Result<()> {
        match self {
            Compound::Map { ser, state } => {
                match state {
                    State::Empty => {}
                    _ => ser.formatter.end_object(&mut ser.writer)?
                };
                ser
                    .formatter
                    .end_object_value(&mut ser.writer)?;
                ser.formatter.end_object(&mut ser.writer)
            }
            #[cfg(feature = "arbitrary_precision")]
            Compound::Number { .. } => unreachable!(),
            #[cfg(feature = "raw_value")]
            Compound::RawValue { .. } => unreachable!(),
        }
    }
}

struct MapKeySerializer<'a, 'b, F> {
    ser: &'a mut Serializer<'b, F>,
}

#[cfg(feature = "arbitrary_precision")]
fn invalid_number() -> Error {
    Error::syntax(ErrorCode::InvalidNumber, 0, 0)
}

#[cfg(feature = "raw_value")]
fn invalid_raw_value() -> Error {
    Error::syntax(ErrorCode::ExpectedSomeValue, 0, 0)
}

fn key_must_be_a_string() -> Error {
    //Error::syntax(ErrorCode::KeyMustBeAString, 0, 0)
    Error::KeyIsNotAString
}

impl<'a, 'b, F> ser::Serializer for MapKeySerializer<'a, 'b, F>
    where
        F: Formatter,
{
    type Ok = ();
    type Error = Error;

    #[inline]
    fn serialize_str(self, value: &str) -> Result<()> {
        self.ser.serialize_str(value)
    }

    #[inline]
    fn serialize_unit_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        variant: &'static str,
    ) -> Result<()> {
        self.ser.serialize_str(variant)
    }

    #[inline]
    fn serialize_newtype_struct<T>(self, _name: &'static str, value: &T) -> Result<()>
        where
            T: ?Sized + ser::Serialize,
    {
        value.serialize(self)
    }

    type SerializeSeq = ser::Impossible<(), Error>;
    type SerializeTuple = ser::Impossible<(), Error>;
    type SerializeTupleStruct = ser::Impossible<(), Error>;
    type SerializeTupleVariant = ser::Impossible<(), Error>;
    type SerializeMap = ser::Impossible<(), Error>;
    type SerializeStruct = ser::Impossible<(), Error>;
    type SerializeStructVariant = ser::Impossible<(), Error>;

    fn serialize_bool(self, _value: bool) -> Result<()> {
        Err(key_must_be_a_string())
    }

    fn serialize_i8(self, value: i8) -> Result<()> {
        self
            .ser
            .formatter
            .begin_string(&mut self.ser.writer)?;
        self
            .ser
            .formatter
            .write_i8(&mut self.ser.writer, value)?;
        self
            .ser
            .formatter
            .end_string(&mut self.ser.writer)
    }

    fn serialize_i16(self, value: i16) -> Result<()> {
        self
            .ser
            .formatter
            .begin_string(&mut self.ser.writer)?;
        self
            .ser
            .formatter
            .write_i16(&mut self.ser.writer, value)?;
        self
            .ser
            .formatter
            .end_string(&mut self.ser.writer)
    }

    fn serialize_i32(self, value: i32) -> Result<()> {
        self
            .ser
            .formatter
            .begin_string(&mut self.ser.writer)?;
        self
            .ser
            .formatter
            .write_i32(&mut self.ser.writer, value)?;
        self
            .ser
            .formatter
            .end_string(&mut self.ser.writer)
    }

    fn serialize_i64(self, value: i64) -> Result<()> {
        self
            .ser
            .formatter
            .begin_string(&mut self.ser.writer)?;
        self
            .ser
            .formatter
            .write_i64(&mut self.ser.writer, value)?;
        self
            .ser
            .formatter
            .end_string(&mut self.ser.writer)
    }

    // serde_if_integer128! {
    //     fn serialize_i128(self, value: i128) -> Result<()> {
    //         tri!(self
    //             .ser
    //             .formatter
    //             .begin_string(&mut self.ser.writer)
    //             .map_err(Error::io));
    //         let mut buf = itoa::Buffer::new();
    //         tri!(self
    //             .ser
    //             .formatter
    //             .write_number_str(&mut self.ser.writer, buf.format(value))
    //             .map_err(Error::io));
    //         tri!(self
    //             .ser
    //             .formatter
    //             .end_string(&mut self.ser.writer)
    //             .map_err(Error::io));
    //         Ok(())
    //     }
    // }

    fn serialize_u8(self, value: u8) -> Result<()> {
        self
            .ser
            .formatter
            .begin_string(&mut self.ser.writer)?;
        self
            .ser
            .formatter
            .write_u8(&mut self.ser.writer, value)?;
        self
            .ser
            .formatter
            .end_string(&mut self.ser.writer)
    }

    fn serialize_u16(self, value: u16) -> Result<()> {
        self
            .ser
            .formatter
            .begin_string(&mut self.ser.writer)?;
        self
            .ser
            .formatter
            .write_u16(&mut self.ser.writer, value)?;
        self
            .ser
            .formatter
            .end_string(&mut self.ser.writer)
    }

    fn serialize_u32(self, value: u32) -> Result<()> {
        self
            .ser
            .formatter
            .begin_string(&mut self.ser.writer)?;
        self
            .ser
            .formatter
            .write_u32(&mut self.ser.writer, value)?;
        self
            .ser
            .formatter
            .end_string(&mut self.ser.writer)
    }

    fn serialize_u64(self, value: u64) -> Result<()> {
        self
            .ser
            .formatter
            .begin_string(&mut self.ser.writer)?;
        self
            .ser
            .formatter
            .write_u64(&mut self.ser.writer, value)?;
        self
            .ser
            .formatter
            .end_string(&mut self.ser.writer)
    }

    //serde_if_integer128! {
    //    fn serialize_u128(self, value: u128) -> Result<()> {
    //        tri!(self
    //            .ser
    //            .formatter
    //            .begin_string(&mut self.ser.writer)
    //            .map_err(Error::io));
    //        let mut buf = itoa::Buffer::new();
    //        tri!(self
    //            .ser
    //            .formatter
    //            .write_number_str(&mut self.ser.writer, buf.format(value))
    //            .map_err(Error::io));
    //        tri!(self
    //            .ser
    //            .formatter
    //            .end_string(&mut self.ser.writer)
    //            .map_err(Error::io));
    //        Ok(())
    //    }
    //}

    fn serialize_f32(self, _value: f32) -> Result<()> {
        Err(key_must_be_a_string())
    }

    fn serialize_f64(self, _value: f64) -> Result<()> {
        Err(key_must_be_a_string())
    }

    fn serialize_char(self, value: char) -> Result<()> {
        let mut buf = [0u8; 4];
        self.ser.serialize_str(value.encode_utf8(&mut buf))
    }

    fn serialize_bytes(self, _value: &[u8]) -> Result<()> {
        Err(key_must_be_a_string())
    }

    fn serialize_unit(self) -> Result<()> {
        Err(key_must_be_a_string())
    }

    fn serialize_unit_struct(self, _name: &'static str) -> Result<()> {
        Err(key_must_be_a_string())
    }

    fn serialize_newtype_variant<T>(
        self,
        _name: &'static str,
        _variant_index: u32,
        _variant: &'static str,
        _value: &T,
    ) -> Result<()>
        where
            T: ?Sized + ser::Serialize,
    {
        Err(key_must_be_a_string())
    }

    fn serialize_none(self) -> Result<()> {
        Err(key_must_be_a_string())
    }

    fn serialize_some<T>(self, _value: &T) -> Result<()>
        where
            T: ?Sized + ser::Serialize,
    {
        Err(key_must_be_a_string())
    }

    fn serialize_seq(self, _len: Option<usize>) -> Result<Self::SerializeSeq> {
        Err(key_must_be_a_string())
    }

    fn serialize_tuple(self, _len: usize) -> Result<Self::SerializeTuple> {
        Err(key_must_be_a_string())
    }

    fn serialize_tuple_struct(
        self,
        _name: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeTupleStruct> {
        Err(key_must_be_a_string())
    }

    fn serialize_tuple_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        _variant: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeTupleVariant> {
        Err(key_must_be_a_string())
    }

    fn serialize_map(self, _len: Option<usize>) -> Result<Self::SerializeMap> {
        Err(key_must_be_a_string())
    }

    fn serialize_struct(self, _name: &'static str, _len: usize) -> Result<Self::SerializeStruct> {
        Err(key_must_be_a_string())
    }

    fn serialize_struct_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        _variant: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeStructVariant> {
        Err(key_must_be_a_string())
    }

    fn collect_str<T>(self, value: &T) -> Result<()>
        where
            T: ?Sized + core::fmt::Display,
    {
        self.ser.collect_str(value)
    }
}

/// Serializes the given data structure as a string of JSON text
#[cfg(feature = "heapless")]
pub fn to_string<T, const N: usize>(value: &T) -> Result<String<N>>
where
    T: ser::Serialize + ?Sized,
{
    Ok(unsafe { str::from_utf8_unchecked(&to_vec::<T, N>(value)?) }.into())
}

/// Serializes the given data structure as a string of JSON text
#[cfg(feature = "heapless")]
pub fn to_string_pretty<T, const N: usize>(value: &T, indent: &[u8]) -> Result<String<N>>
    where
        T: ser::Serialize + ?Sized,
{
    Ok(unsafe { str::from_utf8_unchecked(&to_vec_pretty::<T, N>(value, indent)?) }.into())
}

/// Serializes the given data structure as a JSON byte vector
#[cfg(feature = "heapless")]
pub fn to_vec<T, const N: usize>(value: &T) -> Result<Vec<u8, N>>
where
    T: ser::Serialize + ?Sized,
{
    let mut buf = Vec::<u8, N>::new();
    buf.resize_default(N)?;
    let len = to_slice(value, &mut buf)?;
    buf.truncate(len);
    Ok(buf)
}

/// Serializes the given data structure as a JSON byte vector in a more human readable format
#[cfg(feature = "heapless")]
pub fn to_vec_pretty<T, const N: usize>(value: &T, indent: &[u8]) -> Result<Vec<u8, N>>
    where
        T: ser::Serialize + ?Sized,
{
    let mut buf = Vec::<u8, N>::new();
    buf.resize_default(N)?;
    let len = to_slice_pretty(value, indent, &mut buf)?;
    buf.truncate(len);
    Ok(buf)
}

/// Serializes the given data structure as a JSON byte vector into the provided buffer
pub fn to_slice<T>(value: &T, buf: &mut [u8]) -> Result<usize>
where
    T: ser::Serialize + ?Sized,
{
    let mut ser = Serializer::with_formatter(buf, CompactFormatter);
    value.serialize(&mut ser)?;
    let buf = ser.into_inner();
    Ok(buf.len())
}

/// Serializes the given data structure as a JSON byte vector in a more human-readable format into the provided buffer
pub fn to_slice_pretty<T>(value: &T, indent: &[u8], buf: &mut [u8]) -> Result<usize>
    where
        T: ser::Serialize + ?Sized,
{
    let mut ser = Serializer::with_formatter(buf, PrettyFormatter::with_indent(indent));
    value.serialize(&mut ser)?;
    let buf = ser.into_inner();
    Ok(buf.len())
}

impl ser::Error for Error {
    fn custom<T>(_msg: T) -> Self
    where
        T: fmt::Display,
    {
        unreachable!()
    }
}

pub(crate) enum Unreachable {}

impl ser::SerializeTupleStruct for Unreachable {
    type Ok = ();
    type Error = Error;

    fn serialize_field<T: ?Sized>(&mut self, _value: &T) -> Result<()> {
        unreachable!()
    }

    fn end(self) -> Result<Self::Ok> {
        unreachable!()
    }
}

impl ser::SerializeTupleVariant for Unreachable {
    type Ok = ();
    type Error = Error;

    fn serialize_field<T: ?Sized>(&mut self, _value: &T) -> Result<()> {
        unreachable!()
    }

    fn end(self) -> Result<Self::Ok> {
        unreachable!()
    }
}

impl ser::SerializeMap for Unreachable {
    type Ok = ();
    type Error = Error;

    fn serialize_key<T: ?Sized>(&mut self, _key: &T) -> Result<()>
    where
        T: ser::Serialize,
    {
        unreachable!()
    }

    fn serialize_value<T: ?Sized>(&mut self, _value: &T) -> Result<()>
    where
        T: ser::Serialize,
    {
        unreachable!()
    }

    fn end(self) -> Result<Self::Ok> {
        unreachable!()
    }
}

#[cfg(test)]
mod tests {
    use serde_derive::Serialize;

    const N: usize = 128;

    #[test]
    fn array() {
        let buf = &mut [0u8; 128];
        let len = crate::to_slice(&[0, 1, 2], buf).unwrap();
        assert_eq!(len, 7);
        assert_eq!(&buf[..len], b"[0,1,2]");
        assert_eq!(&*crate::to_string::<_, N>(&[0, 1, 2]).unwrap(), "[0,1,2]");
    }

    #[test]
    fn bool() {
        let buf = &mut [0u8; 128];
        let len = crate::to_slice(&true, buf).unwrap();
        assert_eq!(len, 4);
        assert_eq!(&buf[..len], b"true");

        assert_eq!(&*crate::to_string::<_, N>(&true).unwrap(), "true");
    }

    #[test]
    fn enum_() {
        #[derive(Serialize)]
        enum Type {
            #[serde(rename = "boolean")]
            Boolean,
            #[serde(rename = "number")]
            Number,
            #[serde(rename = "tuple_variant")]
            TupleVariant(u8, i8),
            #[serde(rename = "struct_variant")]
            StructVariant{field: u8}
        }


        assert_eq!(
            &*crate::to_string::<_, N>(&Type::Boolean).unwrap(),
            r#""boolean""#
        );

        assert_eq!(
            &*crate::to_string::<_, N>(&Type::Number).unwrap(),
            r#""number""#
        );

        assert_eq!(
            &*crate::to_string::<_, N>(&Type::TupleVariant(32, -10)).unwrap(),
            r#"{"tuple_variant":[32,-10]}"#
        );

        assert_eq!(
            &*crate::to_string::<_, N>(&Type::StructVariant{field: 82}).unwrap(),
            r#"{"struct_variant":{"field":82}}"#
        );
    }

    #[test]
    fn str() {
        assert_eq!(&*crate::to_string::<_, N>("hello").unwrap(), r#""hello""#);
        assert_eq!(&*crate::to_string::<_, N>("").unwrap(), r#""""#);

        // Characters unescaped if possible
        assert_eq!(&*crate::to_string::<_, N>("ä").unwrap(), r#""ä""#);
        assert_eq!(&*crate::to_string::<_, N>("৬").unwrap(), r#""৬""#);
        // assert_eq!(&*crate::to_string::<_, N>("\u{A0}").unwrap(), r#"" ""#); // non-breaking space
        assert_eq!(&*crate::to_string::<_, N>("ℝ").unwrap(), r#""ℝ""#); // 3 byte character
        assert_eq!(&*crate::to_string::<_, N>("💣").unwrap(), r#""💣""#); // 4 byte character

        // " and \ must be escaped
        assert_eq!(
            &*crate::to_string::<_, N>("foo\"bar").unwrap(),
            r#""foo\"bar""#
        );
        assert_eq!(
            &*crate::to_string::<_, N>("foo\\bar").unwrap(),
            r#""foo\\bar""#
        );

        // \b, \t, \n, \f, \r must be escaped in their two-character escaping
        assert_eq!(
            &*crate::to_string::<_, N>(" \u{0008} ").unwrap(),
            r#"" \b ""#
        );
        assert_eq!(
            &*crate::to_string::<_, N>(" \u{0009} ").unwrap(),
            r#"" \t ""#
        );
        assert_eq!(
            &*crate::to_string::<_, N>(" \u{000A} ").unwrap(),
            r#"" \n ""#
        );
        assert_eq!(
            &*crate::to_string::<_, N>(" \u{000C} ").unwrap(),
            r#"" \f ""#
        );
        assert_eq!(
            &*crate::to_string::<_, N>(" \u{000D} ").unwrap(),
            r#"" \r ""#
        );

        // U+0000 through U+001F is escaped using six-character \u00xx uppercase hexadecimal escape sequences
        assert_eq!(
            &*crate::to_string::<_, N>(" \u{0000} ").unwrap(),
            r#"" \u0000 ""#
        );
        assert_eq!(
            &*crate::to_string::<_, N>(" \u{0001} ").unwrap(),
            r#"" \u0001 ""#
        );
        assert_eq!(
            &*crate::to_string::<_, N>(" \u{0007} ").unwrap(),
            r#"" \u0007 ""#
        );
        assert_eq!(
            &*crate::to_string::<_, N>(" \u{000e} ").unwrap(),
            r#"" \u000E ""#
        );
        assert_eq!(
            &*crate::to_string::<_, N>(" \u{001D} ").unwrap(),
            r#"" \u001D ""#
        );
        assert_eq!(
            &*crate::to_string::<_, N>(" \u{001f} ").unwrap(),
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
            &*crate::to_string::<_, N>(&Led { led: true }).unwrap(),
            r#"{"led":true}"#
        );
    }

    #[test]
    fn struct_i8() {
        #[derive(Serialize)]
        struct Temperature {
            temperature: i8,
        }

        assert_eq!(
            &*crate::to_string::<_, N>(&Temperature { temperature: 127 }).unwrap(),
            r#"{"temperature":127}"#
        );

        assert_eq!(
            &*crate::to_string::<_, N>(&Temperature { temperature: 20 }).unwrap(),
            r#"{"temperature":20}"#
        );

        assert_eq!(
            &*crate::to_string::<_, N>(&Temperature { temperature: -17 }).unwrap(),
            r#"{"temperature":-17}"#
        );

        assert_eq!(
            &*crate::to_string::<_, N>(&Temperature { temperature: -128 }).unwrap(),
            r#"{"temperature":-128}"#
        );
    }

    #[test]
    fn struct_f32() {
        #[derive(Serialize)]
        struct Temperature {
            temperature: f32,
        }

        assert_eq!(
            &*crate::to_string::<_, N>(&Temperature { temperature: -20. }).unwrap(),
            r#"{"temperature":-20.0}"#
        );

        assert_eq!(
            &*crate::to_string::<_, N>(&Temperature {
                temperature: -20345.
            })
            .unwrap(),
            r#"{"temperature":-20345.0}"#
        );

        assert_eq!(
            &*crate::to_string::<_, N>(&Temperature {
                temperature: -2.3456789012345e-23
            })
            .unwrap(),
            r#"{"temperature":-2.3456788e-23}"#
        );
    }

    #[test]
    fn struct_option() {
        #[derive(Serialize)]
        struct Property<'a> {
            description: Option<&'a str>,
        }

        assert_eq!(
            crate::to_string::<_, N>(&Property {
                description: Some("An ambient temperature sensor"),
            })
            .unwrap(),
            r#"{"description":"An ambient temperature sensor"}"#
        );

        // XXX Ideally this should produce "{}"
        assert_eq!(
            crate::to_string::<_, N>(&Property { description: None }).unwrap(),
            r#"{"description":null}"#
        );
    }

    #[test]
    fn struct_u8() {
        #[derive(Serialize)]
        struct Temperature {
            temperature: u8,
        }

        assert_eq!(
            &*crate::to_string::<_, N>(&Temperature { temperature: 20 }).unwrap(),
            r#"{"temperature":20}"#
        );
    }

    #[test]
    fn struct_() {
        #[derive(Serialize)]
        struct Empty {}

        assert_eq!(&*crate::to_string::<_, N>(&Empty {}).unwrap(), r#"{}"#);

        #[derive(Serialize)]
        struct Tuple {
            a: bool,
            b: bool,
        }

        assert_eq!(
            &*crate::to_string::<_, N>(&Tuple { a: true, b: false }).unwrap(),
            r#"{"a":true,"b":false}"#
        );
    }

    #[test]
    fn test_unit() {
        let a = ();
        assert_eq!(&*crate::to_string::<_, N>(&a).unwrap(), r#"null"#);
    }

    #[test]
    fn test_newtype_struct() {
        #[derive(Serialize)]
        struct A(pub u32);
        let a = A(54);
        assert_eq!(&*crate::to_string::<_, N>(&a).unwrap(), r#"54"#);
    }

    #[test]
    fn test_newtype_variant() {
        #[derive(Serialize)]
        enum A {
            A(u32),
        }
        let a = A::A(54);

        assert_eq!(&*crate::to_string::<_, N>(&a).unwrap(), r#"{"A":54}"#);
        assert_eq!(&*crate::to_string_pretty::<_, N>(&a, b"  ").unwrap(),
r#"{
  "A": 54
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
            &*crate::to_string::<_, N>(&a).unwrap(),
            r#"{"A":{"x":54,"y":720}}"#
        );

        assert_eq!(
            &*crate::to_string_pretty::<_, N>(&a, b"  ").unwrap(),
r#"{
  "A": {
    "x": 54,
    "y": 720
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
        assert_eq!(&*crate::to_string::<_, N>(&sd1).unwrap(), r#"1.56"#);

        let sd2 = SimpleDecimal(0.000);
        assert_eq!(&*crate::to_string::<_, N>(&sd2).unwrap(), r#"0.00"#);

        let sd3 = SimpleDecimal(22222.777777);
        assert_eq!(&*crate::to_string::<_, N>(&sd3).unwrap(), r#"22222.78"#);
    }
}
