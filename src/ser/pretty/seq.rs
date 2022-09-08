use serde::ser;

use crate::ser::{Error, Result, pretty::Serializer};

pub struct SerializeSeq<'serializer, 'buffer, 'indent> {
    de: &'serializer mut Serializer<'buffer, 'indent>,
    first: bool,
}

impl<'serializer, 'buffer: 'serializer, 'indent: 'serializer> SerializeSeq<'serializer, 'buffer, 'indent> {
    pub(crate) fn new(de: &'serializer mut Serializer<'buffer, 'indent>) -> Self {
        SerializeSeq { de, first: true }
    }
}

impl<'serializer, 'buffer: 'serializer, 'indent: 'serializer> ser::SerializeSeq for SerializeSeq<'serializer, 'buffer, 'indent> {
    type Ok = ();
    type Error = Error;

    fn serialize_element<T: ?Sized>(&mut self, value: &T) -> Result<()>
    where
        T: ser::Serialize,
    {
        if !self.first {
            self.de.push(b',')?;
        }
        self.first = false;

        self.de.push(b'\n')?;
        self.de.indent()?;

        value.serialize(&mut *self.de)?;
        Ok(())
    }

    fn end(self) -> Result<Self::Ok> {
        self.de.current_indent -= 1;
        if !self.first {
            self.de.push(b'\n')?;
            self.de.indent()?;
        }
        self.de.push(b']')?;
        Ok(())
    }
}

impl<'serializer, 'buffer: 'serializer, 'indent: 'serializer> ser::SerializeTuple for SerializeSeq<'serializer, 'buffer, 'indent> {
    type Ok = ();
    type Error = Error;

    fn serialize_element<T: ?Sized>(&mut self, value: &T) -> Result<()>
    where
        T: ser::Serialize,
    {
        ser::SerializeSeq::serialize_element(self, value)
    }

    fn end(self) -> Result<Self::Ok> {
        ser::SerializeSeq::end(self)
    }
}
