// SPDX-FileCopyrightText:  Copyright 2023 Roland Csaszar
// SPDX-License-Identifier: Apache-2.0
//
// Project:  flix-test
// File:     Alternative.flix
// Date:     27.Apr.2023
//
// ==============================================================================

///
/// Example: `Option`
///
/// ```flix
/// instance Alternative[Option] {
///
///     pub def empty(): Option[a] = None
///
///     pub def or(l: Option[a], r: Option[a]): Option[a] = match (l, r) {
///         case (None, x ) => x
///      // case (None, None) => None
///      // case (None, Some(y)) => Some(y)
///         case (Some(x), _) => Some(x)
///     }
///
/// }
/// ```
///
/// Laws for `Option`:
///
/// rightIdentity: forall(l: Option[a]) with Eq[Option[a]] . Alternative.or(l, Alternative.empty())
///                                                          == Alternative.or(l, None)
///                                                          == l    Q.e.d.
///
/// leftIdentity: forall(r: Option[a]) with Eq[Option[a]] . Alternative.or(Alternative.empty(), r)
///                                                         == Alternative.or(None, r)
///                                                         == r    Q.e.d.
///
/// associative: forall(x: Option[a], y: Option[a], z: Option[a]) with Eq[Option[a]] .
///                       Alternative.or(x, Alternative.or(y, z)) == Alternative.or(Alternative.or(x, y), z)
///
///       * if x == None: Alternative.or(None, Alternative.or(y, z)) [leftIdentity] == Alternative.or(y, z) and
///                       Alternative.or(Alternative.or(None, y), z) [leftIdentity] == Alternative.or(y, z)
///                       Q.e.d.
///       * if x == Some(u): Alternative.or(Some(u), Alternative.or(y, z)) == Some(u)
///                          Alternative.or(Alternative.or(Some(u), y), z) == Alternative.or(Some(u), z) == Some(u)
///                          Q.e.d.
///
/// leftOne: forall(r: Option[a], x: a) with Eq[Option[a]] . Alternative.or(Applicative.point(x), r) == Applicative.point(x)
///          Remark: Applicative.point(x) = Some(x)
///                                                          Alternative.or(Some(x), r) == Some(x) == Applicative.point(x)
///                                                          Q.e.d.
///
/// Usage for `Option`:
///
/// ```flix
/// use Alternative.{or, <|>, choice}
///
/// @Test
/// def testOr(): Bool =
///     or(Some(5), Some(9)) == Some(5)
///
/// @Test
/// def testOr2(): Bool =
///     Some(5) <|> Some(9) == Some(5)
///
/// @Test
/// def testOrNone(): Bool =
///     None <|> Some(9) == Some(9)
///
/// @Test
/// def testOrNone2(): Bool =
///     None <|> None == None: Option[Int32]
///
/// @Test
/// def testOrChained(): Bool =
///     None <|> Some(1) <|> Some(2) <|> Some(3) == Some(1)
//
/// @Test
/// def testOrAssoc(): Bool =
///     Some(1) <|> (Some(2) <|> Some(3)) == (Some(1) <|> Some(2)) <|> Some(3)
///
/// @Test
/// def testOrAssoc2(): Bool =
///     None <|> (Some(2) <|> Some(3)) == (None <|> Some(2)) <|> Some(3)
///
/// @Test
/// def testChoice(): Bool =
///     choice(None :: None :: Some(9) :: Some(1) :: Nil) == Some(9)
/// ```
///
pub lawful class Alternative[m: Type -> Type] with Applicative[m] {

    /// The identity of the monoid.
    ///
    /// `empty() <|> x == x <|> empty() == x`.
    pub def empty(): m[a]

    /// Choose the left `l` or the right `r`.
    ///
    /// Always return the left `l`, if `l` is not `empty()`, 'invalid', 'false',
    /// an error, ...
    ///
    /// The same as `l <|> r`.
    pub def or(l: m[a], r: m[a]): m[a]

    /// Alternative with `empty` and `or/<|>` forms a monoid:

    /// `empty()` is the right identitiy of `or/<|>`.
    law rightIdentity: forall(l: m[a]) with Eq[m[a]] . Alternative.or(l, Alternative.empty()) == l

    /// `empty()` is the left identitiy of `or/<|>`.
    law leftIdentity: forall(r: m[a]) with Eq[m[a]] . Alternative.or(Alternative.empty(), r) == r

    /// `or/<|>` is associative.
    law associative: forall(x: m[a], y: m[a], z: m[a]) with Eq[m[a]] . Alternative.or(x, Alternative.or(y, z)) == Alternative.or(Alternative.or(x, y), z)

    /// `Applicative.point` is always valid/true/...
    law leftOne: forall(r: m[a], x: a) with Eq[m[a]] . Alternative.or(Applicative.point(x), r) == Applicative.point(x)
}

mod Alternative {
    /// An alias of `Alternative.or`.
    pub def <|>(l: m[a], r: m[a]): m[a] with Alternative[m] = Alternative.or(l, r)

    /// `choice([a1, a2, ..., an])` is the same as `a1 <|> a2 <|> ... <|> an`.
    // The leftmost valid/true/... value is returned.
    pub def choice(l: List[m[a]]): m[a] with Alternative[m] =
        match l {
            case Nil => Alternative.empty()
            case x :: xs => x <|> choice(xs)
        }
}
