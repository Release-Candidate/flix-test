// SPDX-FileCopyrightText:  Copyright 2023 Roland Csaszar
// SPDX-License-Identifier: Apache-2.0
//
// Project:  flix-test
// File:     FlixParse.flix
// Date:     27.Apr.2023
//
// ==============================================================================

use GradedFunctor.{<$>}
use GradedApplicative.{*>,<*}
use GradedAlternative.{<|>, choice}

enum Error[i] with Eq, ToString {
    case EndOfInput,
    case Unexpected(i, i)
    case Empty
}

type alias ParserFunc[i: Type, o: Type, ef: Bool] = List[i] -> Result[Error[i], (o, List[i])] \ ef

pub enum Parser[i: Type, ef: Bool, o: Type] {
    case Parser(ParserFunc[i, o, ef])
}

mod Parser {

    use GradedFunctor.{<$>}
    use GradedAlternative.{<|>, choice}

    pub def empty(): Parser[i, ef, a] =
        Parser.Parser(_ -> checked_ecast(Err(Error.Empty)))

    pub def point(x: a): Parser[i, ef, a] =
        Parser.Parser(input -> checked_ecast(Ok(x, input)))

    pub def map(f: a -> b \ ef, p: Parser[i, ff, a]): Parser[i, ff and ef, b] =
        let Parser.Parser(pf) = p;
        Parser.Parser(input -> match pf(input) {
            case Ok(output, l) => Ok(f(output), l)
            case Err(err) => Err(err)
        })

    pub def ap(f: Parser[i, ef1, a -> b & ef2], x: Parser[i, ef, a]): Parser[i, ef and ef1 and ef2, b] =
        let Parser.Parser(pfab) = f;
        let Parser.Parser(pfa) = x;
        Parser.Parser(input -> match pfab(input) {
            case Err(err) => Err(err)
            case Ok(fab, la) => match pfa(la) {
                    case Err(err1) => Err(err1)
                    case Ok(y, ly) => Ok(fab(y), ly)
                }
            }
        )

    pub def flatMap(f: a -> Parser[i, ef1, b] & ef, x: Parser[i, ef2, a]): Parser[i, ef1 and ef2 and ef, b] =
        let Parser.Parser(pfa) = x;
        Parser.Parser(input -> match pfa(input) {
            case Err(err) => Err(err)
            case Ok(y, ly) =>
                let Parser.Parser(result) = f(y);
                result(ly)
            })

    pub def or(l: Parser[i, ef, a], r: Parser[i, ef, a]): Parser[i, ef, a] =
        let Parser.Parser(lpf) = l;
        let Parser.Parser(rpf) = r;
        Parser.Parser(input -> match lpf(input) {
            case Ok(ok, ll) => Ok(ok, ll)
            case Err(_) => match rpf(input) {
                case Ok(okR, lr) => Ok(okR, lr)
                case Err(errR) => Err(errR)
            }
        })

    pub def many(p: Parser[i, ef, o]): Parser[i, ef, List[o]] =
        let Parser.Parser(pf) = p;
        Parser.Parser(input -> match pf(input) {
            case Ok(hd, l1) =>
                let Parser.Parser(mpf) = many(p);
                match mpf(l1) {
                    case Ok(tl, l2) => Ok(hd :: tl, l2)
                    case Err(_) => Ok(hd :: Nil, l1)
                }
            case Err(err) => Err(err)
        })

    pub def star(p: Parser[i, ef, o]): Parser[i, ef, List[o]] =
        many(p) <|> GradedApplicative.point(Nil)

    pub def runParser(parser: Parser[i, ef, o], input: List[i]): Result[Error[i], (o, List[i])] \ ef =
        let Parser.Parser(pf) = parser;
        pf(input)

    pub def satisfy(pred: i -> (Bool, i) \ ef): Parser[i, ef, i] =
        Parser.Parser(input -> match input {
            case Nil => Err(Error.EndOfInput)
            case x :: xs => match pred(x) {
                case (false, c) => Err(Error.Unexpected(x, c))
                case (true, _) => Ok(x, xs)
                case _ => Err(Error.Empty)
            }
        })
}

instance GradedFunctor[Parser[i]] {

    pub def map(f: a -> b \ ef, p: Parser[i, ff, a]): Parser[i, ef and ff, b] =
        Parser.map(f, p)
}

instance GradedApplicative[Parser[i]] {

    pub def point(x: a): Parser[i, ef, a] =
        Parser.point(x)

    pub def ap(f: Parser[i, ef1, a -> b & ef2], x: Parser[i, ef, a]): Parser[i, ef and ef1 and ef2, b] =
        Parser.ap(f, x)
}

instance GradedMonad[Parser[i]] {

    pub def flatMap(f: a -> Parser[i, ef1, b] & ef, x: Parser[i, ef2, a]): Parser[i, ef1 and ef2 and ef, b] =
        Parser.flatMap(f, x)
}

instance GradedAlternative[Parser[i]] {

    pub def empty(): Parser[i, ef, a] =
        Parser.empty()

    pub def or(l: Parser[i, ef, a], r: Parser[i, ef, a]): Parser[i, ef, a] =
        Parser.or(l, r)
}

def anyToString(x: a): String with ToString[a] = "${x}"

def anyListToString(l: List[a]): String with ToString[a] =
    List.foldLeft((acc, x) -> String.concat(acc, "${x}"), "", l)

pub def runStringParser(parser: Parser[Char, Pure, o], input: String): Result[String, (String, String)] with ToString[o] =
    let l = String.toList(input);
    let mappedP = anyToString <$> parser;
    match Parser.runParser(mappedP, l) {
        case Ok(out, lo) => Ok(out, anyListToString(lo))
        case Err(error) => match error {
            case Error.EndOfInput => Err("Unexpected end of input.")
            case Error.Unexpected(r, e) => Err("Expected ${e} but got `${r}`.")
            case Error.Empty => Err("Empty")
        }
    }

pub def charMatch(input: Char): Parser[Char, Pure, Char] =
    Parser.satisfy(c -> (input == c, input))

pub def stringMatch(input: String): Parser[Char, Pure, String] =
    anyListToString <$> GradedTraversable.traverse(charMatch, String.toList(input))

pub def manyChar(input: Char): Parser[Char, Pure, String] =
    (l -> anyListToString(l)) <$> Parser.many(charMatch(input))

pub def starChar(input: Char): Parser[Char, Pure, String] =
    (l -> anyListToString(l)) <$> Parser.star(charMatch(input))

pub def manyString(input: String): Parser[Char, Pure, String] =
    (l -> anyListToString(l)) <$> Parser.many(stringMatch(input))

pub def starString(input: String): Parser[Char, Pure, String] =
    (l -> anyListToString(l)) <$> Parser.star(stringMatch(input))

@Test
def test1(): Bool =
    let hello = String.toList("Hello");
    let actual = Parser.runParser(charMatch('H'), hello);
    Assert.eq(Ok(('H', String.toList("ello"))), actual )

@Test
def test2(): Bool =
    let hello = String.toList("Hello");
    let actual = Parser.runParser(charMatch('h'), hello);
    Assert.eq(Err(Error.Unexpected('H', 'h')), actual)

@Test
def test3(): Bool =
    Assert.eq(Err(Error.EndOfInput), Parser.runParser(charMatch('h'), Nil))

@Test
def test4(): Bool =
    let hugo = String.toList("Hugo");
    let actual = Parser.runParser(stringMatch("Hugo"), hugo ++ ('!' :: Nil));
    Assert.eq(Ok("Hugo", '!' :: Nil), actual)

@Test
def test5(): Bool =
    let matcher = charMatch('H');
    let actual = runStringParser(matcher, "Hi!");
    Assert.eq(Ok("H", "i!"), actual)

@Test
def test6(): Bool =
    let matcher = stringMatch("Hi");
    let actual = runStringParser(matcher, "Hi World!");
    Assert.eq(Ok("Hi", " World!"), actual)

@Test
def test7(): Bool =
    let matcher = stringMatch("Hello") <|> stringMatch("Hi");
    let actual = runStringParser(matcher, "Hi World!");
    Assert.eq(Ok("Hi", " World!"), actual)

@Test
def test72(): Bool =
    let matcher = stringMatch("Hi") <|> stringMatch("Hello");
    let actual = runStringParser(matcher, "Hi World!");
    Assert.eq(Ok("Hi", " World!"), actual)

@Test
def test8(): Bool =
    let matcher = choice(stringMatch("He") :: stringMatch("Ho") :: stringMatch("Hi") :: Nil);
    let actual = runStringParser(matcher, "Hi World!");
    Assert.eq(Ok("Hi", " World!"), actual)

@Test
def test9(): Bool =
    let matcher = stringMatch("Hi ") *> stringMatch("Wor");
    let actual = runStringParser(matcher, "Hi World!");
    Assert.eq(Ok("Wor", "ld!"), actual)

@Test
def test10(): Bool =
    let matcher = stringMatch("Hi") <* stringMatch(" Wor");
    let actual = runStringParser(matcher, "Hi World!");
    Assert.eq(Ok("Hi", "ld!"), actual)

@Test
def test11(): Bool =
    let matcher = manyChar('i');
    let actual = runStringParser(matcher, "iiiii World!");
    Assert.eq(Ok("iiiii", " World!"), actual)

@Test
def test12(): Bool =
    let matcher = starChar('i');
    let actual = runStringParser(matcher, "Hiiiii World!");
    Assert.eq(Ok("", "Hiiiii World!"), actual)

@Test
def test13(): Bool =
    let matcher = manyString("ii");
    let actual = runStringParser(matcher, "iiiii World!");
    Assert.eq(Ok("iiii", "i World!"), actual)
