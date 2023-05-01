// SPDX-FileCopyrightText:  Copyright 2023 Roland Csaszar
// SPDX-License-Identifier: Apache-2.0
//
// Project:  flix-test
// File:     TestMain.flix
// Date:     27.Apr.2023
//
// ==============================================================================

mod TestAlternative {
use Alternative.{or, <|>, choice}

@Test
def testOr(): Bool =
    not Assert.eq(Some(5), or(Some(5), Some(9)))

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
}

mod TestGradedAlternative {
use GradedAlternative.{or, <|>, choice}

@Test
def testOr(): Bool =
    let Check.Check(cf) = or(Check.Check(_ -> Some(5)), Check.Check(_ -> Some(9)));
    Assert.eq(Some(5), cf(0))

@Test
def testOr2(): Bool =
    let Check.Check(cf) = Check.Check(_ -> Some(5)) <|> Check.Check(_ -> Some(9));
    Assert.eq(Some(5), cf(0))

@Test
def testOrNone(): Bool =
    let Check.Check(cf) = Check.Check(_ -> None) <|> Check.Check(_ -> Some(9));
    Assert.eq(Some(9), cf(0))

@Test
def testOrNone2(): Bool =
    let Check.Check(cf) = Check.Check(_ -> None) <|> Check.Check(_ -> None);
    Assert.eq(None: Option[Int32], cf(0))

@Test
def testOrChained(): Bool =
    let Check.Check(cf) = Check.Check(_ -> None) <|> Check.Check(_ -> Some(1))
        <|> Check.Check(_ -> Some(2)) <|> Check.Check(_ -> Some(3));
    Assert.eq(Some(1), cf(0))

@Test
def testOrAssoc(): Bool =
    let Check.Check(cf1) = Check.Check(_-> Some(1))
        <|> (Check.Check(_ -> Some(2)) <|> Check.Check(_ -> Some(3)));
    let Check.Check(cf2) = (Check.Check(_-> Some(1)) <|> Check.Check(_ -> Some(2)))
        <|> Check.Check(_ -> Some(3));
    Assert.eq(cf1(0), cf2(0))

@Test
def testOrAssoc2(): Bool =
    let Check.Check(cf1) = Check.Check(_-> None)
        <|> (Check.Check(_ -> Some(2)) <|> Check.Check(_ -> Some(3)));
    let Check.Check(cf2) = (Check.Check(_-> None) <|> Check.Check(_ -> Some(2)))
        <|> Check.Check(_ -> Some(3));
    Assert.eq(cf1(0), cf2(0))

@Test
def testChoice(): Bool =
    let Check.Check(cf) = choice(Check.Check(_ -> None) :: Check.Check(_ -> None)
         :: Check.Check(_ -> Some(9)) :: Check.Check(_ -> Some(1)) :: Nil);
    Assert.eq(Some(9), cf(0))
}
