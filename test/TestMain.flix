// SPDX-FileCopyrightText:  Copyright 2023 Roland Csaszar
// SPDX-License-Identifier: Apache-2.0
//
// Project:  flix-test
// File:     TestMain.flix
// Date:     27.Apr.2023
//
// ==============================================================================

use Alternative.{or, <|>, choice}

@Test
def testOr(): Bool =
    or(Some(5), Some(9)) == Some(5)

@Test
def testOr2(): Bool =
    Some(5) <|> Some(9) == Some(5)

@Test
def testOrNone(): Bool =
    None <|> Some(9) == Some(9)

@Test
def testOrNone2(): Bool =
    None <|> None == None: Option[Int32]

@Test
def testOrChained(): Bool =
    None <|> Some(1) <|> Some(2) <|> Some(3) == Some(1)

@Test
def testOrAssoc(): Bool =
    Some(1) <|> (Some(2) <|> Some(3)) == (Some(1) <|> Some(2)) <|> Some(3)

@Test
def testOrAssoc2(): Bool =
    None <|> (Some(2) <|> Some(3)) == (None <|> Some(2)) <|> Some(3)

@Test
def testChoice(): Bool =
    choice(None :: None :: Some(9) :: Some(1) :: Nil) == Some(9)
