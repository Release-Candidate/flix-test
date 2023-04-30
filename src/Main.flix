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
