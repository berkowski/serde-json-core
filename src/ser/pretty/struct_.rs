use serde::ser;

use crate::ser::{Error, Result, pretty::Serializer};

pub struct SerializeStruct<'serializer, 'buffer, 'indent> {
    ser: &'serializer mut Serializer<'buffer, 'indent>,
    first: bool,
}

impl<'serializer, 'buffer: 'serializer, 'indent: 'serializer> SerializeStruct<'serializer, 'buffer, 'indent> {
    pub(crate) fn new(ser: &'serializer mut Serializer<'buffer, 'indent>) -> Self {
        SerializeStruct { ser, first: true }
    }
}

impl<'serializer, 'buffer: 'serializer, 'indent: 'serializer> ser::SerializeStruct for SerializeStruct<'serializer, 'buffer, 'indent> {
    type Ok = ();
    type Error = Error;

    fn serialize_field<T: ?Sized>(&mut self, key: &'static str, value: &T) -> Result<()>
    where
        T: ser::Serialize,
    {
        // XXX if `value` is `None` we not produce any output for this field
        if !self.first {
            self.ser.push(b',')?;
        }
        self.first = false;
        self.ser.push(b'\n')?;
        self.ser.indent()?;

        self.ser.push(b'"')?;
        self.ser.extend_from_slice(key.as_bytes())?;
        self.ser.extend_from_slice(b"\":")?;

        value.serialize(&mut *self.ser)?;

        Ok(())
    }

    fn end(self) -> Result<Self::Ok> {
        self.ser.current_indent -= 1;
        if !self.first {
            self.ser.push(b'\n')?;
            self.ser.indent()?;
        }
        self.ser.push(b'}')?;
        Ok(())
    }
}

pub struct SerializeStructVariant<'serializer, 'buffer, 'indent> {
    ser: &'serializer mut Serializer<'buffer, 'indent>,
    first: bool,
}

impl<'serializer, 'buffer: 'serializer, 'indent: 'serializer> SerializeStructVariant<'serializer, 'buffer, 'indent> {
    pub(crate) fn new(ser: &'serializer mut Serializer<'buffer, 'indent>) -> Self {
        SerializeStructVariant { ser, first: true }
    }
}

impl<'serializer, 'buffer: 'serializer, 'indent: 'serializer> ser::SerializeStructVariant for SerializeStructVariant<'serializer, 'buffer, 'indent> {
    type Ok = ();
    type Error = Error;

    fn serialize_field<T: ?Sized>(&mut self, key: &'static str, value: &T) -> Result<()>
    where
        T: ser::Serialize,
    {
        // XXX if `value` is `None` we not produce any output for this field
        if !self.first {
            self.ser.push(b',')?;
        }
        self.first = false;
        self.ser.push(b'\n')?;
        self.ser.indent()?;

        self.ser.push(b'"')?;
        self.ser.extend_from_slice(key.as_bytes())?;
        self.ser.extend_from_slice(b"\":")?;

        value.serialize(&mut *self.ser)?;

        Ok(())
    }

    fn end(self) -> Result<Self::Ok> {
        for _ in 0..2 {
            self.ser.current_indent -= 1;
            if !self.first {
                self.ser.push(b'\n')?;
                self.ser.indent()?;
            }
            self.ser.push(b'}')?;
        }
        Ok(())
    }
}
