// SPDX-FileCopyrightText:  Copyright 2023 Roland Csaszar
// SPDX-License-Identifier: Apache-2.0
//
// Project:  flix-test
// File:     Main.flix
// Date:     27.Apr.2023
//
// ==============================================================================

use Alternative

def main(): Unit \ IO =
    "${10**5} ${Int32.mod(10, 3)}" |> println

instance Alternative[Option] {

    pub def empty(): Option[a] = None

    pub def or(l: Option[a], r: Option[a]): Option[a] = match (l, r) {
        case (None, x ) => x
        case (Some(x), _) => Some(x)
    }

}

enum Check[i: Type, ef: Bool, a: Type] {
    case Check(i -> Option[a] \ ef)
}

instance GradedFunctor[Check[i]] {

    pub def map(f: a -> b & ef1, x: Check[i, ef, a]): Check[i, ef and ef1, b] =
        let Check.Check(fa) = x;
        Check.Check(y -> match fa(y){
            case None => None,
            case Some(fay) => Some(f(fay))
        })
}

instance GradedApplicative[Check[i]] {

    pub def point(x: a): Check[i, ef, a] =
        Check.Check(_ -> checked_ecast(Some(x)))

    pub def ap(f: Check[i, ef1, a -> b & ef2], x: Check[i, ef, a]): Check[i, ef and ef1 and ef2, b] =
        let Check.Check(fab) = f;
        let Check.Check(fa) = x;
        Check.Check(y -> match fa(y) {
            case None => None,
            case Some(fay) => match fab(y) {
                case None => None,
                case Some(fabr) => Some(fabr(fay))
            }
        })
}

instance GradedAlternative[Check[i]] {

    pub def empty(): Check[i, ef, a] = Check.Check(_ -> checked_ecast(None))

    pub def or(l: Check[i][ef, a], r: Check[i, ef, a]): Check[i, ef, a] =
        let Check.Check(lf) = l;
        let Check.Check(rf) = r;
        Check.Check(y -> match lf(y) {
            case None => rf(y)
            case Some(lfr) => Some(lfr)
        })
}
