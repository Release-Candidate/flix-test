// SPDX-FileCopyrightText:  Copyright 2023 Roland Csaszar
// SPDX-License-Identifier: Apache-2.0
//
// Project:  flix-test
// File:     FlixParse.flix
// Date:     27.Apr.2023
//
// ==============================================================================

use GradedFunctor
use Alternative.{<|>, choice, empty}

enum Error[i] with Eq, ToString {
    case EndOfInput,
    case Unexpected(i, i)
    case Empty
}

type alias ParserType[i: Type, o: Type, ef: Bool] = List[i] -> Result[Error[i], (o, List[i])] \ ef

pub enum Parser[i: Type, ef: Bool, o: Type] {
    case Parser(List[i] -> Result[Error[i], (o, List[i])] \ ef)
}

mod Parser {

    pub def point(x: a): Parser[i, Pure, a] =
        Parser.Parser(input -> Ok(x, input))

    pub def map(f: a -> b \ ef, parseFunc: Parser[i, ff, a]): Parser[i, ff and ef, b] =
        let Parser.Parser(pf) = parseFunc;
        Parser.Parser(input -> match pf(input) {
            case Ok(output, l) => Ok(f(output), l)
            case Err(err) => Err(err)
        })

    pub def ap(f: Parser[i][ef1, a -> b & ef2], x: Parser[i][ef, a]): Parser[i][ef and ef1 and ef2, b] =
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

    pub def flatMap(f: a -> Parser[i][ef1, b] & ef, x: Parser[i][ef2, a]): Parser[i][ef1 and ef2 and ef, b] =
        let Parser.Parser(pfa) = x;
        Parser.Parser(input -> match pfa(input) {
            case Err(err) => Err(err)
            case Ok(y, ly) =>
                let Parser.Parser(result) = f(y);
                result(ly)
            })
}

instance GradedFunctor[Parser[i]] {

    pub def map(f: a -> b \ ef, parseFunc: Parser[i, ff, a]): Parser[i, ef and ff, b] =
        Parser.map(f, parseFunc)
}

instance GradedApplicative[Parser[i]] {

    pub def point(x: a): Parser[i][ef, a] =
        checked_cast(Parser.point(x))

    pub def ap(f: Parser[i][ef1, a -> b & ef2], x: Parser[i][ef, a]): Parser[i][ef and ef1 and ef2, b] =
        Parser.ap(f, x)
}

instance GradedMonad[Parser[i]] {

    pub def flatMap(f: a -> Parser[i][ef1, b] & ef, x: Parser[i][ef2, a]): Parser[i][ef1 and ef2 and ef, b] =
        Parser.flatMap(f, x)
}

pub def runParser(parser: ParserType[i, o, ef], input: List[i]): Result[String, (o, List[i])] \ ef with ToString[i] =
    match parser(input) {
        case Ok(o) => Ok(o)
        case Err(error) => match error {
            case Error.EndOfInput => Err("Unexpected end of input.")
            case Error.Unexpected(r, e) => Err("Expected ${e} but is ${r}.")
            case Error.Empty => Err("Empty")
        }
    }



pub def satisfy(pred: i -> (Bool, i) \ ef): ParserType[i, i, ef] =
    input -> match input {
        case Nil => Err(Error.EndOfInput)
        case x :: xs => match pred(x) {
            case (false, c) => Err(Error.Unexpected(x, c))
            case (true, _) => Ok(x, xs)
            case _ => Err(Error.Empty)
        }
    }

pub def charParser(input: Char): ParserType[Char, Char, Pure] =
    satisfy(c -> (input == c, input))


@Test
def test1(): Bool =
    let hello = String.toList("Hello");
    Assert.eq(Ok(('H', String.toList("ello"))), runParser(charParser('H'), hello))

@Test
def test2(): Bool =
    let hello = String.toList("Hello");
    Assert.eq(Err("Expected ${'h'} but is ${'H'}."), runParser(charParser('h'), hello))

@Test
def test3(): Bool =
    Assert.eq(Err("Unexpected end of input."), runParser(charParser('h'), Nil))
