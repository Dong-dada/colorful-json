use std::iter::{FromIterator, Peekable};
use std::option::Option::Some;
use std::str::Chars;
use std::collections::HashMap;

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    NULL,
    FALSE,
    TRUE,
    NUMBER(f64),
    STRING(String),
    ARRAY(Vec<Value>),
    OBJECT(HashMap<String, Value>),
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
            if *c == '\r' || *c == '\t' || *c == '\n' || *c == ' ' {
                self.chars.next();
            } else {
                break;
            }
        }
    }

    fn parse_null(&mut self) -> Result<Value, DecodingError> {
        if self.next_if_match_str("null") {
            return Ok(Value::NULL);
        }

        return Err(DecodingError::InvalidValue);
    }

    fn parse_true(&mut self) -> Result<Value, DecodingError> {
        if self.next_if_match_str("true") {
            return Ok(Value::TRUE);
        }

        return Err(DecodingError::InvalidValue);
    }

    fn parse_false(&mut self) -> Result<Value, DecodingError> {
        if self.next_if_match_str("false") {
            return Ok(Value::FALSE);
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
        if self.next_if_match('-') {
            number_chars.push('-');
        }

        // 解析整数部分
        // int = "0" / digit1-9 *digit
        if self.next_if_match('0') {
            number_chars.push('0');
        } else if let Some(ch) = self.next_if_digit() {
            number_chars.push(ch);
            while let Some(ch) = self.next_if_digit() {
                number_chars.push(ch);
            }
        } else {
            // 整数部分必须存在
            return Err(DecodingError::InvalidValue);
        }

        // 解析小数部分
        // fraction = "." 1*digit
        if self.next_if_match('.') {
            number_chars.push('.');

            // 解析小数点后的数字
            let mut has_number = false;
            while let Some(ch) = self.next_if_digit() {
                number_chars.push(ch);
                has_number = true;
            }

            // 小数点后面至少要有一个数字
            if !has_number {
                return Err(DecodingError::InvalidValue);
            }
        }

        // 解析指数部分
        // exp = ("e" / "E") ["-" / "+"] 1*digit
        if self.next_if_match_ignore_case('e') {
            number_chars.push('e');

            if self.next_if_match('-') {
                number_chars.push('-');
            } else if self.next_if_match('+') {
                number_chars.push('+');
            }

            // 解析指数的数字部分
            let mut has_number = false;
            while let Some(ch) = self.next_if_digit() {
                number_chars.push(ch);
                has_number = true;
            }

            // 指数后面必须有个数字
            if !has_number {
                return Err(DecodingError::InvalidValue);
            }
        }

        // 将字符串形式的数字，转换为 double 来存储
        if let Ok(number) = String::from_iter(number_chars).parse::<f64>() {
            return Ok(Value::NUMBER(number));
        }

        return Err(DecodingError::InvalidValue);
    }

    fn next_if_match(&mut self, expect_char: char) -> bool {
        if let Some(char) = self.chars.peek() {
            if *char == expect_char {
                self.chars.next();
                return true;
            }
        }

        return false;
    }

    fn next_if_match_ignore_case(&mut self, expect_char: char) -> bool {
        if let Some(char) = self.chars.peek() {
            if char.eq_ignore_ascii_case(&expect_char) {
                self.chars.next();
                return true;
            }
        }

        return false;
    }

    fn next_if_match_str(&mut self, chars: &str) -> bool {
        for char in chars.chars() {
            if !self.next_if_match(char) {
                return false;
            }
        }

        return true;
    }

    fn next_if_digit(&mut self) -> Option<char> {
        if let Some(&char) = self.chars.peek() {
            if char >= '0' && char <= '9' {
                self.chars.next();
                return Some(char);
            }
        }

        return None;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

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
        assert_eq!(decoded_value.unwrap(), Value::NULL);
    }

    #[test]
    fn parse_null_with_whitespace() {
        let mut decoder = Parser::new("\r\n\t null");
        let decoded_value = decoder.parse();
        assert!(decoded_value.is_ok());
        assert_eq!(decoded_value.unwrap(), Value::NULL);
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
        assert_eq!(decoded_value.unwrap(), Value::TRUE);
    }

    #[test]
    fn parse_false() {
        let mut decoder = Parser::new(" false ");
        let decoded_value = decoder.parse();
        assert!(decoded_value.is_ok());
        assert_eq!(decoded_value.unwrap(), Value::FALSE);
    }

    fn test_number(ok_value: f64, text: &str) {
        let mut decoder = Parser::new(text);
        let actual_result = decoder.parse();
        assert!(actual_result.is_ok());
        assert_eq!(actual_result.unwrap(), Value::NUMBER(ok_value));
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

        /* the smallest number > 1 */
        test_number(1.0000000000000002, "1.0000000000000002");
        /* minimum denormal */
        test_number( 4.9406564584124654e-324, "4.9406564584124654e-324");
        test_number(-4.9406564584124654e-324, "-4.9406564584124654e-324");
        /* Max subnormal double */
        test_number( 2.2250738585072009e-308, "2.2250738585072009e-308");
        test_number(-2.2250738585072009e-308, "-2.2250738585072009e-308");
        /* Min normal positive double */
        test_number( 2.2250738585072014e-308, "2.2250738585072014e-308");
        test_number(-2.2250738585072014e-308, "-2.2250738585072014e-308");
        /* Max double */
        test_number( 1.7976931348623157e+308, "1.7976931348623157e+308");
        test_number(-1.7976931348623157e+308, "-1.7976931348623157e+308");

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
