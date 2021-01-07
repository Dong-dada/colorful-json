use std::str::Chars;
use std::iter::{Peekable, FromIterator};
use std::option::Option::Some;

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

#[derive(Debug, PartialEq)]
pub struct Value {
    value_type: ValueType,
    number: f64,
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
            _ => self.parse_number(),
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
                number: 0.0
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
                number: 0.0
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
                number: 0.0
            });
        }

        return Err(DecodingError::InvalidValue);
    }

    fn parse_number(&mut self) -> Result<Value, DecodingError> {
        // number = [ "-" ] int [ fraction ] [ exponent ]
        // int = "0" / digit1-9 *digit
        // fraction = "." 1*digit
        // exp = ("e" / "E") ["-" / "+"] 1*digit
        let mut number_chars = vec![];

        // 解析 "-"
        if let Some(ch) = Parser::next_if(&mut self.chars, |ch| ch == '-') {
            number_chars.push(ch);
        }

        // 解析整数部分
        // int = "0" / digit1-9 *digit
        if let Some(ch) = Parser::next_if(&mut self.chars, |ch| ch == '0') {
            number_chars.push(ch);
        } else if let Some(ch) = Parser::next_if(&mut self.chars, |ch| ch >= '1' && ch <= '9') {
            number_chars.push(ch);
            while let Some(ch) = Parser::next_if(&mut self.chars, |ch| ch >= '0' && ch <= '9') {
                number_chars.push(ch);
            }
        } else {
            // 整数部分必须存在
            return Err(DecodingError::InvalidValue);
        }

        // 解析小数部分
        // fraction = "." 1*digit
        if let Some(ch) = Parser::next_if(&mut self.chars, |ch| ch == '.') {
            number_chars.push(ch);

            // 小数点后面至少要有一个数字
            if let Some(ch) = Parser::next_if(&mut self.chars, |ch| ch >= '0' && ch <= '9') {
                number_chars.push(ch);
            } else {
                return Err(DecodingError::InvalidValue);
            }

            // 解析小数点后的其它数字
            while let Some(ch) = Parser::next_if(&mut self.chars, |ch| ch >= '0' && ch <= '9') {
                number_chars.push(ch);
            }
        }

        // 解析指数部分
        // exp = ("e" / "E") ["-" / "+"] 1*digit
        if let Some(ch) = Parser::next_if(&mut self.chars, |ch| ch == 'e' || ch == 'E') {
            number_chars.push(ch);

            if let Some(ch) = Parser::next_if(&mut self.chars, |ch| ch == '-' || ch == '+') {
                number_chars.push(ch);
            }

            // 必须至少有一个数字
            if let Some(ch) = Parser::next_if(&mut self.chars, |ch| ch >= '0' && ch <= '9') {
                number_chars.push(ch);
            } else {
                return Err(DecodingError::InvalidValue);
            }

            // 解析剩余数字
            while let Some(ch) = Parser::next_if(&mut self.chars, |ch| ch >= '0' && ch <= '9') {
                number_chars.push(ch);
            }
        }

        // 将字符串形式的数字，转换为 double 来存储
        let number = String::from_iter(number_chars).parse::<f64>();
        return if number.is_err() {
            Err(DecodingError::InvalidValue)
        } else {
            Ok(Value {
                value_type: ValueType::NUMBER,
                number: number.unwrap()
            })
        }
    }

    fn next_if(chars: &mut Peekable<Chars>,  func: impl FnOnce(char) -> bool) -> Option<char> {
        if let Some(&ch) = chars.peek() {
            if func(ch) {
                chars.next();
                return Some(ch);
            }
        }

        return None;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn get_value_type() {
        let value = Value {
            value_type: ValueType::NULL,
            number: 0.0
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

    fn test_number(ok_value: f64, text: &str) {
        let mut decoder = Parser::new(text);
        let actual_result = decoder.parse();
        assert!(actual_result.is_ok());
        assert_eq!(actual_result.unwrap().number, ok_value);
    }

    fn test_error_number(error: DecodingError, text: &str) {
        let mut decoder = Parser::new(text);
        let actual_result = decoder.parse();
        assert!(actual_result.is_err());
        assert_eq!(actual_result.unwrap_err(), error);
    }

    #[test]
    fn parse_number() {
        test_number(0.0, "0");
        test_number(0.0, "-0");
        test_number(0.0, "-0.0");
        test_number(1.0, "1");
        test_number(-1.0, "-1");
        test_number(1.5, "1.5");
        test_number(-1.5, "-1.5");
        test_number(3.1416, "3.1416");
        test_number(1E10, "1E10");
        test_number(1e10, "1e10");
        test_number(1E+10, "1E+10");
        test_number(1E-10, "1E-10");
        test_number(-1E10, "-1E10");
        test_number(-1e10, "-1e10");
        test_number(-1E+10, "-1E+10");
        test_number(-1E-10, "-1E-10");
        test_number(1.234E+10, "1.234E+10");
        test_number(1.234E-10, "1.234E-10");
        test_number(0.0, "1e-10000");

        test_error_number(DecodingError::InvalidValue, "+0");
        test_error_number(DecodingError::InvalidValue, "+1");
        test_error_number(DecodingError::InvalidValue, ".123");
        test_error_number(DecodingError::InvalidValue, "1.");
        test_error_number(DecodingError::InvalidValue, "INF");
        test_error_number(DecodingError::InvalidValue, "inf");
        test_error_number(DecodingError::InvalidValue, "NAN");
        test_error_number(DecodingError::InvalidValue, "nan");
    }
}
