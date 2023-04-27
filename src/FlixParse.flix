// SPDX-FileCopyrightText:  Copyright 2023 Roland Csaszar
// SPDX-License-Identifier: Apache-2.0
//
// Project:  flix-test
// File:     FlixParse.flix
// Date:     27.Apr.2023
//
// ==============================================================================

use Alternative.{<|>, choice, empty}

type alias Parser[i, a] = List[i] -> Result[Error[i], (a, List[i])]

pub def runParser[i: Type, a: Type](parser: Parser[i, a], input: List[i]): Result[String, (a, List[i])] with ToString[i] =
    match parser(input) {
        case Ok(o) => Ok(o)
        case Err(error) => match error {
            case Error.EndOfInput => Err("Unexpected end of input.")
            case Error.Unexpected(r, e) => Err("Expected ${e} but is ${r}.")
            case Error.Empty => Err("Empty")
        }
    }

enum Error[i] with Eq, ToString {
    case EndOfInput,
    case Unexpected(i, i)
    case Empty
}

pub def satisfy[i: Type](pred: i -> (Bool, i)): Parser[i, i] =
    input -> match input {
        case Nil => Err(Error.EndOfInput)
        case x :: xs => match pred(x) {
            case (false, c) => Err(Error.Unexpected(x, c))
            case (true, _) => Ok(x, xs)
            case _ => Err(Error.Empty)
        }
    }

pub def charParser(input: Char): Parser[Char, Char] = satisfy(c -> (input == c, input))

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
