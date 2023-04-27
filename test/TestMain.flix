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
    Assert.eq(Some(5), or(Some(5), Some(9)))

@Test
def testOr2(): Bool =
    Assert.eq(Some(5), Some(5) <|> Some(9))

@Test
def testOrNone(): Bool =
    Assert.eq(Some(9), None <|> Some(9))

@Test
def testOrNone2(): Bool =
    Assert.eq(None: Option[Int32], None <|> None)

@Test
def testOrChained(): Bool =
    Assert.eq(Some(1), None <|> Some(1) <|> Some(2) <|> Some(3))

@Test
def testOrAssoc(): Bool =
    Assert.eq((Some(1) <|> Some(2)) <|> Some(3), Some(1) <|> (Some(2) <|> Some(3)))

@Test
def testOrAssoc2(): Bool =
    Assert.eq((None <|> Some(2)) <|> Some(3), None <|> (Some(2) <|> Some(3)))

@Test
def testChoice(): Bool =
    Assert.eq(Some(9), choice(None :: None :: Some(9) :: Some(1) :: Nil))
