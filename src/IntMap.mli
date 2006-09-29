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

(** Mappings from nonnegative integer keys to arbitrary types.  *)

(** The type of the map.  *)
type 'a t

(** The empty map.  *)
val empty : 'a t

(** Test whether the map is empty. O(1) time.  *)
val is_empty : 'a t -> bool


(** [has_key m i] returns true if there is a mapping [(i, x)] in
    [m]. Raises [Invalid_argument] when [i] is negative. O(log n) time.  *)
val has_key : 'a t -> int -> bool

(** [max_key m] returns the maximum key in [m]. Raises [Not_found]
    when [m] is empty. O(log n) time.  *)
val max_key : 'a t -> int
  
(** [get m i] returns the current binding of [i] in [m], or raises
    [Not_found] if no such binding exists. O(log n) time.  *)
val get : 'a t -> int -> 'a

(** [get_default m i x] returns the current binding of [i] in [m], or
    [x] if no such binding exists. Raises [Invalid_argument] when [i] is
    negative. O(log n) time.  *)
val get_default : 'a t -> int -> 'a -> 'a

(** [add m i x] returns a map containing the same bindings as [m],
    plus a binding of [i] to [x]. If [x] was already bound in [m], its
    previous binding disappears. Raises [Invalid_argument] when [i] is
    negative. O(log n) time.  *)
val add : 'a t -> int -> 'a -> 'a t

(** [fold f m a] computes [(f iN xN ... (f i1 x1 a)...)], where [i1
    ... iN] are the keys of all bindings in [m] (in increasing order),
    and [x1 ... xN] are the associated data.  *)
val fold : ('b -> int -> 'a -> 'b) -> 'a t -> 'b -> 'b
