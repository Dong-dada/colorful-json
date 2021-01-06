use std::str::Chars;
use std::iter::Peekable;

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum ValueType {
    NULL,
    FALSE,
    TRUE,
    NUMBER,
    STRING,
    ARRAY,
    OBJECT,
}

pub struct Value {
    value_type: ValueType,
}

impl Value {
    pub fn get_type(&self) -> ValueType {
        return self.value_type;
    }
}

#[derive(Debug, PartialEq)]
pub enum DecodingError {
    ExpectValue,
    InvalidValue,
    RootNotSingular,
}

pub struct Parser<'a> {
    chars: Peekable<Chars<'a>>,
}

impl Parser<'_> {
    pub fn new(text: &str) -> Parser {
        Parser {
            chars: text.chars().peekable(),
        }
    }

    pub fn parse(&mut self) -> Result<Value, DecodingError> {
        self.skip_white_space();

        let result = match self.chars.peek() {
            None => Err(DecodingError::ExpectValue),
            Some('n') => self.parse_null(),
            Some('t') => self.parse_true(),
            Some('f') => self.parse_false(),
            _ => Err(DecodingError::InvalidValue),
        };

        self.skip_white_space();
        if result.is_ok() && self.chars.next() != None {
            return Err(DecodingError::RootNotSingular);
        }

        return result;
    }

    fn skip_white_space(&mut self) {
        while let Some(c) = self.chars.peek() {
            if *c == '\r' || *c == '\t' || *c == '\n' || *c == ' '  {
                self.chars.next();
            } else {
                break;
            }
        }
    }

    fn parse_null(&mut self) -> Result<Value, DecodingError> {
        if self.chars.next() == Some('n')
            && self.chars.next() == Some('u')
            && self.chars.next() == Some('l')
            && self.chars.next() == Some('l')
        {
            return Ok(Value {
                value_type: ValueType::NULL,
            });
        }

        return Err(DecodingError::InvalidValue);
    }

    fn parse_true(&mut self) -> Result<Value, DecodingError> {
        if self.chars.next() == Some('t')
            && self.chars.next() == Some('r')
            && self.chars.next() == Some('u')
            && self.chars.next() == Some('e')
        {
            return Ok(Value {
                value_type: ValueType::TRUE,
            });
        }

        return Err(DecodingError::InvalidValue);
    }

    fn parse_false(&mut self) -> Result<Value, DecodingError> {
        if self.chars.next() == Some('f')
            && self.chars.next() == Some('a')
            && self.chars.next() == Some('l')
            && self.chars.next() == Some('s')
            && self.chars.next() == Some('e')
        {
            return Ok(Value {
                value_type: ValueType::FALSE,
            });
        }

        return Err(DecodingError::InvalidValue);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn get_value_type() {
        let value = Value {
            value_type: ValueType::NULL,
        };

        assert_eq!(value.get_type(), ValueType::NULL);
    }

    #[test]
    fn parse() {
        let mut decoder = Parser::new("");
        let decoded_value = decoder.parse();
        assert!(decoded_value.is_err());
        assert_eq!(decoded_value.err().unwrap(), DecodingError::ExpectValue);
    }

    #[test]
    fn parse_null() {
        let mut decoder = Parser::new("null");
        let decoded_value = decoder.parse();
        assert!(decoded_value.is_ok());
        assert_eq!(decoded_value.unwrap().value_type, ValueType::NULL);
    }

    #[test]
    fn parse_null_with_whitespace() {
        let mut decoder = Parser::new("\r\n\t null");
        let decoded_value = decoder.parse();
        assert!(decoded_value.is_ok());
        assert_eq!(decoded_value.unwrap().value_type, ValueType::NULL);
    }

    #[test]
    fn parse_invalid_null() {
        let mut decoder = Parser::new("nuii");
        let decoded_value = decoder.parse();
        assert!(decoded_value.is_err());
        assert_eq!(decoded_value.err().unwrap(), DecodingError::InvalidValue);
    }

    #[test]
    fn parse_uncompleted_null() {
        let mut decoder = Parser::new("n");
        let decoded_value = decoder.parse();
        assert!(decoded_value.is_err());
        assert_eq!(decoded_value.err().unwrap(), DecodingError::InvalidValue);
    }

    #[test]
    fn parse_true() {
        let mut decoder = Parser::new(" true ");
        let decoded_value = decoder.parse();
        assert!(decoded_value.is_ok());
        assert_eq!(decoded_value.unwrap().value_type, ValueType::TRUE);
    }

    #[test]
    fn parse_false() {
        let mut decoder = Parser::new(" false ");
        let decoded_value = decoder.parse();
        assert!(decoded_value.is_ok());
        assert_eq!(decoded_value.unwrap().value_type, ValueType::FALSE);
    }
}
