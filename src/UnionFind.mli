(* ulp -- solve the undirected labeling problem
   Copyright (C) 2006  Falk H�ffner

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

(** Union-Find structure for integers.  *)

(** The type of a Union-Find structure.  *)
type t

(** Make a new Union-Find structure where every element is a
    singleton.  *)
val make : IntSet.t -> t

(** Join two equivalence classes by representatives.  *)
val join : t -> int -> int -> t

(** Return a list of equivalence classes.  *)
val classes : t -> IntSet.t list

(** [equivalence_classes s r] calculates the equivalence classes of
    [s] under the equivalence relation [r].  *)  
val equivalence_classes : IntSet.t -> (int -> int -> bool) -> IntSet.t list