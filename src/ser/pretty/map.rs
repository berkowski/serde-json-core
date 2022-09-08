use serde::ser;
use crate::ser::{Error, Result, pretty::Serializer};

pub struct SerializeMap<'serializer, 'buffer, 'indent> {
    ser: &'serializer mut Serializer<'buffer, 'indent>,
    first: bool,
}

impl<'serializer, 'buffer: 'serializer, 'indent: 'serializer> SerializeMap<'serializer, 'buffer, 'indent> {
    pub(crate) fn new(ser: &'serializer mut Serializer<'buffer, 'indent>) -> Self {
        SerializeMap { ser, first: true }
    }
}

impl<'serializer, 'buffer: 'serializer, 'indent: 'serializer> ser::SerializeMap for  SerializeMap<'serializer, 'buffer, 'indent> {
    type Ok = ();
    type Error = Error;

    fn end(self) -> Result<Self::Ok> {
        self.ser.current_indent -= 1;
        if !self.first {
            self.ser.push(b'\n')?;
            self.ser.indent()?;
        }
        self.ser.push(b'}')?;
        Ok(())
    }

    fn serialize_key<T: ?Sized>(&mut self, key: &T) -> Result<()>
    where
        T: ser::Serialize,
    {
        if !self.first {
            self.ser.push(b',')?;
        }
        self.first = false;
        self.ser.push(b'\n')?;
        self.ser.indent()?;

        key.serialize(&mut *self.ser)?;
        self.ser.extend_from_slice(b":")?;
        Ok(())
    }

    fn serialize_value<T: ?Sized>(&mut self, value: &T) -> Result<()>
    where
        T: ser::Serialize,
    {
        value.serialize(&mut *self.ser)?;
        Ok(())
    }
}
