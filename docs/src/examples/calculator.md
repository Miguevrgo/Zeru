# Calculator

This calculator reads arithmetic, one expression per line, and prints the
result:

```sh
$ zeru run calc.zr
> 1 + 2 * 3
7.0
> (1 + 2) * 3
9.0
> 1 / 0
error: division by zero
> 2 $ 3
error: unexpected character
```

```zeru,check
import std.os;

const BAD_CHAR: i32 = 1;
const BAD_SYNTAX: i32 = 2;
const DIV_ZERO: i32 = 3;

enum Token {
    Num(f64),
    Plus,
    Minus,
    Star,
    Slash,
    Open,
    Close,
    End,
}

// Splits `text` into tokens, ending with `Token::End`.
fn tokenize(text: str) Vec<Token>! {
    var tokens: Vec<Token> = Vec.new();
    var i: usize = 0;
    while i < text.len() {
        var c = text[i];
        if c == ' ' {
            i += 1;
            continue;
        }
        if c >= '0' && c <= '9' || c == '.' {
            var start = i;
            while i < text.len() && (text[i] >= '0' && text[i] <= '9' || text[i] == '.') {
                i += 1;
            }
            tokens.push(Token::Num(text[start..i].parse_float()!));
            continue;
        }
        match c {
            '+' => tokens.push(Token::Plus),
            '-' => tokens.push(Token::Minus),
            '*' => tokens.push(Token::Star),
            '/' => tokens.push(Token::Slash),
            '(' => tokens.push(Token::Open),
            ')' => tokens.push(Token::Close),
            default => {
                return Err(BAD_CHAR);
            }
        }
        i += 1;
    }
    tokens.push(Token::End);
    return tokens;
}

// Reads tokens in order, computing as it goes.
struct Parser {
    tokens: Vec<Token>,
    at: usize,

    fn peek(self) Token {
        return self.tokens[self.at];
    }

    fn next(var self) Token {
        self.at += 1;
        return self.tokens[self.at - 1];
    }

    // A sum: terms joined by + and -.
    fn sum(var self) f64! {
        var value = self.term()!;
        while true {
            match self.peek() {
                Token::Plus => {
                    self.next();
                    value += self.term()!;
                }
                Token::Minus => {
                    self.next();
                    value -= self.term()!;
                }
                default => {
                    return value;
                }
            }
        }
        return value;
    }

    // A term: factors joined by * and /.
    fn term(var self) f64! {
        var value = self.factor()!;
        while true {
            match self.peek() {
                Token::Star => {
                    self.next();
                    value *= self.factor()!;
                }
                Token::Slash => {
                    self.next();
                    var divisor = self.factor()!;
                    if divisor == 0.0 {
                        return Err(DIV_ZERO);
                    }
                    value /= divisor;
                }
                default => {
                    return value;
                }
            }
        }
        return value;
    }

    // A number, a negated factor, or a sum in parentheses.
    fn factor(var self) f64! {
        match self.next() {
            Token::Num(n) => {
                return n;
            }
            Token::Minus => {
                return -self.factor()!;
            }
            Token::Open => {
                var value = self.sum()!;
                match self.next() {
                    Token::Close => {
                        return value;
                    }
                    default => {
                        return Err(BAD_SYNTAX);
                    }
                }
            }
            default => {
                return Err(BAD_SYNTAX);
            }
        }
    }
}

fn evaluate(text: str) f64! {
    var parser = Parser { tokens: tokenize(text)!, at: 0 };
    var value = parser.sum()!;
    match parser.peek() {
        Token::End => {
            return value;
        }
        default => {
            return Err(BAD_SYNTAX);
        }
    }
}

fn main() {
    while true {
        print("> ");
        var line = os::stdin().read_line().unwrap();
        if line.is_none() {
            return;
        }
        var text = line.unwrap();
        if text.trim().len() == 0 {
            continue;
        }
        match evaluate(text) {
            Ok(value) => println("{}", value),
            Err(code) => {
                var why = match code {
                    BAD_CHAR => "unexpected character",
                    DIV_ZERO => "division by zero",
                    default => "not a valid expression",
                };
                println("error: {}", why);
            }
        }
    }
}
```

## How it works

**Two steps.** `tokenize` turns the text into a list of tokens, the
numbers and symbols, with the spaces gone. The `Parser` then reads the
tokens in order and computes the result.

**Tokens as an enum.** Each kind of token is a variant of `Token`, and a
number carries its value: `Token::Num(f64)`. Matching on a token handles
each kind in its own arm, and the compiler checks that none is forgotten
where there is no `default`.

**Precedence by structure.** A `sum` is made of terms joined by `+` and
`-`, and a `term` of factors joined by `*` and `/`. Since `sum` asks `term`
for each of its parts, multiplication binds tighter than addition with no
precedence table. A `factor` that finds an opening parenthesis starts a
whole new `sum`, so parentheses nest as deep as needed.

**Errors that travel.** Every parsing method returns `f64!`. When
`factor` finds something it cannot read, it returns `Err(BAD_SYNTAX)`, and
each `!` on the way up, in `term`, `sum` and `evaluate`, passes the error
on without another line of code. `main` is the only place that decides
what to do with it.

## Try it

- Add `%` for the remainder.
- Add `^` for powers, binding tighter than `*`.
- Remember the last result and let the next expression use it as `ans`.
