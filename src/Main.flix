// SPDX-FileCopyrightText:  Copyright 2023 Roland Csaszar
// SPDX-License-Identifier: Apache-2.0
//
// Project:  flix-test
// File:     Main.flix
// Date:     27.Apr.2023
//
// ==============================================================================

use Alternative

enum Animal {
    case Cat,
    case Dog,
    case Giraffe
}

type alias Tier = Animal

def isTall(a: Tier): Bool = match ??? {
    case Animal.Cat => ???
    case Animal.Dog => ???
    case Animal.Giraffe => ???
}

// def main(): Unit \ IO =
//     let args = Environment.getArgs();
//     let t = Animal.Dog;
//     let _ = isTall(t);
//     match args {
//         case Nil => println("Missing argument: filename")
//         case file :: _ =>
//             match File.readLines(file) {
//                 case Err(_) =>
//                     println("Unable to read: ${file}")
//                 case Ok(lines) =>
//                     let totalLines = List.length(lines);
//                     let totalWords = List.sumWith(numberOfWords, lines);
//                     println("Lines: ${totalLines}, Words: ${totalWords}")
//             }
//     }

def numberOfWords(s: String): Int32 =
    s |> String.words |> List.length

def isOdd(n: Int32): Bool =
    if (n == 0) false else isEvn(n - 1)

def isEvn(n: Int32): Bool =
    if (n == 0) true else isOdd(n - 1)

def main(): Unit \ IO =
    "${10**5} ${Int32.mod(10, 3)}" |> println

instance Alternative[Option] {

    pub def empty(): Option[a] = None

    pub def or(l: Option[a], r: Option[a]): Option[a] = match (l, r) {
        case (None, x ) => x
        case (Some(x), _) => Some(x)
    }

}


def factorial(n: Int32): Int32 =
    def visit(x, acc) = match x {
        case 0 => acc
        case _ => visit(x - 1, x * acc)
    };
    visit(n, 1)

// def main(): Unit \ IO =
//     println(factorial(1_000_000))


// def main(): Unit \ IO =
//     let s: String = "Hello World";
//     let n: Int32 = s?;
//     println("The length of ${s} is ${n}!")
