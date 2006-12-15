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

(** Sets of nonnegative integers.  *)
    
(** The type of the set.  *)
type t

(** Exception raised by [add].  *)
exception Already_present

(** The empty set.  *)
val empty : t

(** Test whether the set is empty. O(1) time.  *)
val is_empty : t -> bool

(** [contains s i] returns true if [i] is element of [s].  Raises
    [Invalid_argument] when [i] is negative. O(log n) time.  *)
val contains : t -> int -> bool

(** [put s i] returns a set containing all elements of [s], plus
    [i]. If [i] was already in [s], [s] is returned unchanged.  Raises
    [Invalid_argument] when [i] is negative.  O(log n) time.  *)
val put : t -> int -> t

(** [add s i] returns a set containing all elements of [s], plus
    [i]. Raises [Already_present] if [i] was already in [s]. Raises
    [Invalid_argument] when [i] is negative.  O(log n) time.  *)
val add : t -> int -> t

(** [fold f s a] computes [(f iN ... (f i1 a)...)], where [i1 ... In]
    are the elements of [s].  *)
val fold : ('a -> int -> 'a) -> t -> 'a -> 'a

(** [output c s] prints a debug representation of [s] to channel [c].
    O(n) time.  *)
val output : out_channel -> t -> unit
