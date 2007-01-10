(* ulp -- solve the undirected labeling problem
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

(** Solve the ULP problem. Returns a list of edges to delete to make a
    graph sign-consistent. Note that while the return value does not
    differentiate between deleting positive and negative edges, the result
    is still eindeutig, since for one type all edges have to be deleted in
    any valid solution.  *)
val solve : Ulp.t -> bool IntMap.t

val solve_all_colorings : Ulp.t -> IntSet.t -> bool IntMap.t IntMap.t
