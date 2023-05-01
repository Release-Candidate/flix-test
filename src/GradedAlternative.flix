// SPDX-FileCopyrightText:  Copyright 2023 Roland Csaszar
// SPDX-License-Identifier: Apache-2.0
//
// Project:  flix-test
// File:     GradedAlternative.flix
// Date:     29.Apr.2023
//
// ==============================================================================


///
/// Example: `Check`, a function from `i` to `Option`.
///
/// ```flix
/// enum Check[i: Type, ef: Bool, a: Type] {
///     case Check(i -> Option[a] \ ef)
/// }
///
/// instance GradedAlternative[Check[i]] {
///
///     pub def empty(): Check[i, ef, a] = Check.Check(_ -> checked_ecast(None))
///
///     pub def or(l: Check[i][ef, a], r: Check[i, ef, a]): Check[i, ef, a] =
///         let Check.Check(lf) = l;
///         let Check.Check(rf) = r;
///         Check.Check(y -> match lf(y) {
///             case None => rf(y)
///             case Some(lfr) => Some(lfr)
///         })
/// }
/// ```
///
/// Laws for `Check`:
///
/// rightIdentity: forall(l: m[ef, a]) with Eq[m[ef, a]] . GradedAlternative.or(l, GradedAlternative.empty()) == l
///
///
/// leftIdentity: forall(r: m[ef, a]) with Eq[m[ef, a]] . GradedAlternative.or(GradedAlternative.empty(), r) == r
///
/// /// `or/<|>` is associative.
/// associative: forall(x: m[ef, a], y: m[ef, a], z: m[ef, a]) with Eq[m[ef, a]] . GradedAlternative.or(x, GradedAlternative.or(y, z)) == GradedAlternative.or(GradedAlternative.or(x, y), z)
///
/// leftOne: forall(r: m[ef, a], x: a) with Eq[m[ef, a]] . GradedAlternative.or(GradedApplicative.point(x), r) == GradedApplicative.point(x)
///
/// Usage for `Check`:
///
/// ```flix
// use GradedAlternative.{or, <|>, choice}

// @Test
// def testOr(): Bool =
//     let Check.Check(cf) = or(Check.Check(_ -> Some(5)), Check.Check(_ -> Some(9)));
//     Assert.eq(Some(5), cf(0))

// @Test
// def testOr2(): Bool =
//     let Check.Check(cf) = Check.Check(_ -> Some(5)) <|> Check.Check(_ -> Some(9));
//     Assert.eq(Some(5), cf(0))

// @Test
// def testOrNone(): Bool =
//     let Check.Check(cf) = Check.Check(_ -> None) <|> Check.Check(_ -> Some(9));
//     Assert.eq(Some(9), cf(0))

// @Test
// def testOrNone2(): Bool =
//     let Check.Check(cf) = Check.Check(_ -> None) <|> Check.Check(_ -> None);
//     Assert.eq(None: Option[Int32], cf(0))

// @Test
// def testOrChained(): Bool =
//     let Check.Check(cf) = Check.Check(_ -> None) <|> Check.Check(_ -> Some(1))
//         <|> Check.Check(_ -> Some(2)) <|> Check.Check(_ -> Some(3));
//     Assert.eq(Some(1), cf(0))

// @Test
// def testOrAssoc(): Bool =
//     let Check.Check(cf1) = Check.Check(_-> Some(1))
//         <|> (Check.Check(_ -> Some(2)) <|> Check.Check(_ -> Some(3)));
//     let Check.Check(cf2) = (Check.Check(_-> Some(1)) <|> Check.Check(_ -> Some(2)))
//         <|> Check.Check(_ -> Some(3));
//     Assert.eq(cf1(0), cf2(0))

// @Test
// def testOrAssoc2(): Bool =
//     let Check.Check(cf1) = Check.Check(_-> None)
//         <|> (Check.Check(_ -> Some(2)) <|> Check.Check(_ -> Some(3)));
//     let Check.Check(cf2) = (Check.Check(_-> None) <|> Check.Check(_ -> Some(2)))
//         <|> Check.Check(_ -> Some(3));
//     Assert.eq(cf1(0), cf2(0))

// @Test
// def testChoice(): Bool =
//     let Check.Check(cf) = choice(Check.Check(_ -> None) :: Check.Check(_ -> None)
//          :: Check.Check(_ -> Some(9)) :: Check.Check(_ -> Some(1)) :: Nil);
//     Assert.eq(Some(9), cf(0))
/// ```
///
pub lawful class GradedAlternative[m: Bool -> Type -> Type] with GradedApplicative[m] {

    /// The identity of the monoid.
    ///
    /// `empty() <|> x == x <|> empty() == x`.
    pub def empty(): m[ef, a]

    /// Choose the left `l` or the right `r`.
    ///
    /// Always return the left `l`, if `l` is not `empty()`, 'invalid', 'false',
    /// an error, ...
    ///
    /// The same as `l <|> r`.
    pub def or(l: m[ef, a], r: m[ef, a]): m[ef, a]

    /// Alternative with `empty` and `or/<|>` forms a monoid:

    /// `empty()` is the right identitiy of `or/<|>`.
    law rightIdentity: forall(l: m[ef, a]) with Eq[m[ef, a]] . GradedAlternative.or(l, GradedAlternative.empty()) == l

    /// `empty()` is the left identitiy of `or/<|>`.
    law leftIdentity: forall(r: m[ef, a]) with Eq[m[ef, a]] . GradedAlternative.or(GradedAlternative.empty(), r) == r

    /// `or/<|>` is associative.
    law associative: forall(x: m[ef, a], y: m[ef, a], z: m[ef, a]) with Eq[m[ef, a]] . GradedAlternative.or(x, GradedAlternative.or(y, z)) == GradedAlternative.or(GradedAlternative.or(x, y), z)

    /// `Applicative.point` is always valid/true/...
    law leftOne: forall(r: m[ef, a], x: a) with Eq[m[ef, a]] . GradedAlternative.or(GradedApplicative.point(x), r) == GradedApplicative.point(x)
}

mod GradedAlternative {
    /// An alias of `Alternative.or`.
    pub def <|>[m: Bool -> Type -> Type, ef: Bool, a: Type](l: m[ef, a], r: m[ef, a]): m[ef, a] with GradedAlternative[m] =
        GradedAlternative.or(l, r)

    /// `choice([a1, a2, ..., an])` is the same as `a1 <|> a2 <|> ... <|> an`.
    // The leftmost valid/true/... value is returned.
    pub def choice[m: Bool -> Type -> Type, ef: Bool, a: Type](l: List[m[ef, a]]): m[ef, a] with GradedAlternative[m] =
        match l {
            case Nil => GradedAlternative.empty()
            case x :: xs => x <|> choice(xs)
        }
}
