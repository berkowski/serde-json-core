use serde::ser;

use crate::ser::{Error, Result, Serializer};

pub struct SerializeStruct<'a, 'b, F> {
    ser: &'a mut Serializer<'b, F>,
    first: bool,
}

impl<'a, 'b: 'a, F> SerializeStruct<'a, 'b, F> {
    pub(crate) fn new(ser: &'a mut Serializer<'b, F>) -> Self {
        SerializeStruct { ser, first: true }
    }
}

impl<'a, 'b: 'a, F> ser::SerializeStruct for SerializeStruct<'a, 'b, F> {
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

        self.ser.push(b'"')?;
        self.ser.extend_from_slice(key.as_bytes())?;
        self.ser.extend_from_slice(b"\":")?;

        value.serialize(&mut *self.ser)?;

        Ok(())
    }

    fn end(self) -> Result<Self::Ok> {
        self.ser.push(b'}')?;
        Ok(())
    }
}

pub struct SerializeStructVariant<'a, 'b, F> {
    ser: &'a mut Serializer<'b, F>,
    first: bool,
}

impl<'a, 'b: 'a, F> SerializeStructVariant<'a, 'b, F> {
    pub(crate) fn new(ser: &'a mut Serializer<'b, F>) -> Self {
        SerializeStructVariant { ser, first: true }
    }
}

impl<'a, 'b: 'a, F> ser::SerializeStructVariant for SerializeStructVariant<'a, 'b, F> {
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

        self.ser.push(b'"')?;
        self.ser.extend_from_slice(key.as_bytes())?;
        self.ser.extend_from_slice(b"\":")?;

        value.serialize(&mut *self.ser)?;

        Ok(())
    }

    fn end(self) -> Result<Self::Ok> {
        self.ser.extend_from_slice(b"}}")?;
        Ok(())
    }
}
