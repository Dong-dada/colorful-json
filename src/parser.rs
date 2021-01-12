use std::collections::HashMap;
use std::convert::TryFrom;
use std::iter::{FromIterator, Peekable};
use std::option::Option::Some;
use std::result::Result::Ok;
use std::str::Chars;

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
        let result = self.parse_value();
        if result.is_ok() && self.chars.next() != None {
            return Err(DecodingError::RootNotSingular);
        }

        return result;
    }

    fn parse_value(&mut self) -> Result<Value, DecodingError> {
        self.skip_white_space();

        let result = match self.chars.peek() {
            None => Err(DecodingError::ExpectValue),
            Some('n') => self.parse_null(),
            Some('t') => self.parse_true(),
            Some('f') => self.parse_false(),
            Some('\"') => self.parse_string(),
            Some('[') => self.parse_array(),
            Some('{') => self.parse_object(),
            _ => self.parse_number(),
        };

        self.skip_white_space();
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

    fn parse_string(&mut self) -> Result<Value, DecodingError> {
        if !self.next_if_match('\"') {
            return Err(DecodingError::InvalidValue);
        }

        let mut chars: Vec<char> = Vec::new();
        while let Some(&char) = self.chars.peek() {
            if char == '\"' {
                break;
            }

            if self.next_if_match('\\') {
                // 转义字符
                match self.chars.peek() {
                    Some('\"') => chars.push('\"'),
                    Some('\\') => chars.push('\\'),
                    Some('/') => chars.push('/'),
                    Some('n') => chars.push('\n'),
                    Some('r') => chars.push('\r'),
                    Some('t') => chars.push('\t'),
                    // rust 不支持 \b, \f 转义字符，因此这两个转义字符被忽略
                    Some('b') => {}
                    Some('f') => {}
                    // Escaped Unicode
                    Some('u') => {
                        self.chars.next();
                        let mut numbers = [char; 4];
                        for i in 0..4 {
                            if let Some(number) = self.next_if_hex_digit() {
                                numbers[i] = number;
                            } else {
                                return Err(DecodingError::InvalidValue);
                            }
                        }
                        let numbers: String = numbers.iter().collect();
                        if let Ok(number) = u32::from_str_radix(numbers.as_str(), 16) {
                            if let Some(char) = Parser::from_u32(number) {
                                chars.push(char);
                                continue;
                            } else {
                                return Err(DecodingError::InvalidValue);
                            }
                        } else {
                            return Err(DecodingError::InvalidValue);
                        }
                    }
                    _ => return Err(DecodingError::InvalidValue),
                }
            } else {
                // 其它字符
                chars.push(char);
            }
            self.chars.next();
        }

        // 必须以 '\"' 结尾
        if !self.next_if_match('\"') {
            return Err(DecodingError::InvalidValue);
        }

        return Ok(Value::STRING(chars.into_iter().collect()));
    }

    fn parse_array(&mut self) -> Result<Value, DecodingError> {
        if !self.next_if_match('[') {
            return Err(DecodingError::InvalidValue);
        }

        self.skip_white_space();

        if self.next_if_match(']') {
            return Ok(Value::ARRAY(Vec::new()));
        }

        let mut array = Vec::new();
        while let Ok(value) = self.parse_value() {
            array.push(value);

            if self.next_if_match(']') {
                break;
            } else if self.next_if_match(',') {
                continue;
            } else {
                return Err(DecodingError::InvalidValue);
            }
        }

        return Ok(Value::ARRAY(array));
    }

    fn parse_object(&mut self) -> Result<Value, DecodingError> {
        if !self.next_if_match('{') {
            return Err(DecodingError::InvalidValue);
        }

        self.skip_white_space();

        if self.next_if_match('}') {
            return Ok(Value::OBJECT(HashMap::new()));
        }

        let mut object = HashMap::new();
        loop {
            self.skip_white_space();
            if let Ok(Value::STRING(key)) = self.parse_string() {
                self.skip_white_space();
                if !self.next_if_match(':') {
                    return Err(DecodingError::InvalidValue);
                }

                if let Ok(value) = self.parse_value() {
                    object.insert(key, value);
                    if self.next_if_match('}') {
                        break;
                    } else if self.next_if_match(',') {
                        continue;
                    } else {
                        return Err(DecodingError::InvalidValue);
                    }
                } else {
                    return Err(DecodingError::InvalidValue);
                }
            } else {
                return Err(DecodingError::InvalidValue);
            }
        }

        return Ok(Value::OBJECT(object));
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

    fn next_if_hex_digit(&mut self) -> Option<char> {
        if let Some(&char) = self.chars.peek() {
            if (char >= '0' && char <= '9')
                || (char >= 'a' && char <= 'f')
                || (char >= 'A' && char <= 'F')
            {
                self.chars.next();
                return Some(char);
            }
        }

        return None;
    }

    fn from_u32(i: u32) -> Option<char> {
        return if let Ok(char) = char::try_from(i) {
            Some(char)
        } else {
            None
        };
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn assert_result(expect_result: Result<Value, DecodingError>, text: &str) {
        let mut parser = Parser::new(text);
        let actual_result = parser.parse();
        assert_eq!(expect_result, actual_result);
    }

    #[test]
    fn basic_tests() {
        assert_result(Err(DecodingError::ExpectValue), "");

        // null
        assert_result(Ok(Value::NULL), "null");
        assert_result(Ok(Value::NULL), "\r\n\t null");
        assert_result(Err(DecodingError::InvalidValue), "nil");
        assert_result(Err(DecodingError::InvalidValue), "n");

        // true, false
        assert_result(Ok(Value::TRUE), "true");
        assert_result(Ok(Value::FALSE), "false");
        assert_result(Err(DecodingError::RootNotSingular), "true_");
        assert_result(Err(DecodingError::InvalidValue), "False_");
    }

    fn assert_number(expect_number: f64, text: &str) {
        let mut parser = Parser::new(text);
        let actual_result = parser.parse();
        assert!(actual_result.is_ok());
        assert_eq!(Value::NUMBER(expect_number), actual_result.unwrap());
    }

    #[test]
    fn number_tests() {
        assert_number(0.0, "0");
        assert_number(0.0, "-0");
        assert_number(0.0, "-0.0");
        assert_number(1.0, "1");
        assert_number(-1.0, "-1");
        assert_number(1.5, "1.5");
        assert_number(-1.5, "-1.5");
        assert_number(3.1416, "3.1416");
        assert_number(1E10, "1E10");
        assert_number(1e10, "1e10");
        assert_number(1E+10, "1E+10");
        assert_number(1E-10, "1E-10");
        assert_number(-1E10, "-1E10");
        assert_number(-1e10, "-1e10");
        assert_number(-1E+10, "-1E+10");
        assert_number(-1E-10, "-1E-10");
        assert_number(1.234E+10, "1.234E+10");
        assert_number(1.234E-10, "1.234E-10");
        assert_number(0.0, "1e-10000");

        /* the smallest number > 1 */
        assert_number(1.0000000000000002, "1.0000000000000002");
        /* minimum denormal */
        assert_number(4.9406564584124654e-324, "4.9406564584124654e-324");
        assert_number(-4.9406564584124654e-324, "-4.9406564584124654e-324");
        /* Max subnormal double */
        assert_number(2.2250738585072009e-308, "2.2250738585072009e-308");
        assert_number(-2.2250738585072009e-308, "-2.2250738585072009e-308");
        /* Min normal positive double */
        assert_number(2.2250738585072014e-308, "2.2250738585072014e-308");
        assert_number(-2.2250738585072014e-308, "-2.2250738585072014e-308");
        /* Max double */
        assert_number(1.7976931348623157e+308, "1.7976931348623157e+308");
        assert_number(-1.7976931348623157e+308, "-1.7976931348623157e+308");

        assert_result(Err(DecodingError::InvalidValue), "+0");
        assert_result(Err(DecodingError::InvalidValue), "+1");
        assert_result(Err(DecodingError::InvalidValue), ".123");
        assert_result(Err(DecodingError::InvalidValue), "1.");
        assert_result(Err(DecodingError::InvalidValue), "INF");
        assert_result(Err(DecodingError::InvalidValue), "inf");
        assert_result(Err(DecodingError::InvalidValue), "NAN");
        assert_result(Err(DecodingError::InvalidValue), "nan");
    }

    fn assert_string(expect_string: &str, text: &str) {
        let mut parser = Parser::new(text);
        let result = parser.parse();
        assert_eq!(Ok(Value::STRING(expect_string.to_string())), result);
    }

    #[test]
    fn string_tests() {
        assert_string("", r#"  ""  "#);
        assert_string("hello world", r#"  "hello world"  "#);

        // 转义字符
        assert_string("hello\n world", r#"  "hello\n world"  "#);
        assert_string("hello\t world", r#"  "hello\t world"  "#);
        assert_string("hello\r world", r#"  "hello\r world"  "#);
        assert_string("hello\\ world", r#"  "hello\\ world"  "#);
        assert_string("hello\" world", r#"  "hello\" world"  "#);
        assert_string("hello/ world", r#"  "hello\/ world"  "#);

        // 不支持的转移字符
        assert_string("hello world", r#"  "hello\b world"  "#);
        assert_string("hello world", r#"  "hello\f world"  "#);

        // escaped unicode
        assert_string("❤", r#"  "\u2764"  "#);

        assert_result(Err(DecodingError::InvalidValue), r#"  "hello  "#);
        assert_result(Err(DecodingError::InvalidValue), r#"  hello"  "#);
        assert_result(Err(DecodingError::RootNotSingular), r#"  "hello"hello"  "#);
    }

    fn assert_array(ok_value: Vec<Value>, text: &str) {
        let mut parser = Parser::new(text);
        let result = parser.parse();
        assert_eq!(Ok(Value::ARRAY(ok_value)), result);
    }

    #[test]
    fn array_tests() {
        assert_array(Vec::new(), r#" [] "#);
        assert_array(
            vec![
                Value::STRING("hello".to_string()),
                Value::STRING("world".to_string()),
                Value::NUMBER(1.0),
                Value::NUMBER(2.0),
                Value::ARRAY(vec![
                    Value::STRING("colorful".to_string()),
                    Value::STRING("json".to_string()),
                    Value::TRUE,
                    Value::NULL,
                ]),
            ],
            r#"  ["hello", "world", 1.0, 2.0, ["colorful", "json", true, null]]  "#,
        );
        assert_array(vec![
            Value::ARRAY(vec![]),
            Value::ARRAY(vec![])
        ], r#" [[],[]] "#);

        assert_result(Err(DecodingError::InvalidValue), r#"  ["abc", ["123", [1.0]]  "#);
    }

    fn assert_object(expect_object: HashMap<String, Value>, text: &str) {
        let mut parser = Parser::new(text);
        let result = parser.parse();
        assert_eq!(Ok(Value::OBJECT(expect_object)), result);
    }

    #[test]
    fn object_tests() {
        assert_object(HashMap::new(), r#" {} "#);
        assert_object(
            hashmap! {
                "name".to_string() => Value::STRING("董哒哒".to_string()),
                "age".to_string() => Value::NUMBER(42.0),
                "color".to_string() => Value::ARRAY(vec![
                    Value::STRING("blue".to_string()),
                    Value::STRING("yellow".to_string()),
                ]),
                "project".to_string() => Value::OBJECT(hashmap! {
                    "name".to_string() => Value::STRING("colorful-json".to_string()),
                    "done".to_string() => Value::TRUE,
                })
            },
            r#" {
            "name": "董哒哒",
            "age": 42,
            "color": [
                "blue",
                "yellow"
            ],
            "project": {
                "name": "colorful-json",
                "done": true
            }
        } "#,
        );

        assert_object(hashmap! {
            "".to_string() => Value::OBJECT(hashmap! {})
        }, r#" {"": {}} "#);

        assert_result(Err(DecodingError::InvalidValue), r#" {"name": { "age": 19 } "#);
        assert_result(Err(DecodingError::RootNotSingular), r#" "name": "董哒哒" "#);
    }
}
