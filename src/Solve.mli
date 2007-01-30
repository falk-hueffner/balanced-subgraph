(* scs -- solve the sign-consistent subgraph problem
   Copyright (C) 2006  Falk Hüffner

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License along
   with this program; if not, write to the Free Software Foundation, Inc.,
   51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.  *)

(** Solve the SCS problem. Returns a coloring with minimum number of
    conflict edges.  *)
val solve : Scs.t -> bool IntMap.t

(** [solve_all_colorings g c] optimally solves SCS under all possible
    colorings of the vertices in [c], except that (because of symmetry)
    the highest vertex in [c] is always colored 0. Returns a map of the
    optimal colorings, where the keys are binary representation of the
    coloring of [c].  *)
val solve_all_colorings : Scs.t -> IntSet.t -> bool IntMap.t IntMap.t
