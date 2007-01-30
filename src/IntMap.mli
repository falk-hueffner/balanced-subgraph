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

(** Mappings from nonnegative integer keys to arbitrary types.  *)

(** The type of the map.  *)
type 'a t

(** Exception raised by [add].  *)
exception Already_present

(** The empty map.  *)
val empty : 'a t

(** Test whether the map is empty. O(1) time.  *)
val is_empty : 'a t -> bool

(** Returns the number of key/value pairs. O(1) time.  *)
val size : 'a t -> int

(** [has_key m i] returns true if there is a mapping [(i, x)] in
    [m]. Raises [Invalid_argument] when [i] is negative. O(log n) time.  *)
val has_key : 'a t -> int -> bool

(** [max_key m] returns the maximum key in [m]. Raises [Not_found]
    when [m] is empty. O(log n) time.  *)
val max_key : 'a t -> int
val min_key : 'a t -> int
  
(** [get m i] returns the current binding of [i] in [m], or raises
    [Not_found] if no such binding exists. O(log n) time.  *)
val get : 'a t -> int -> 'a

(** [get_opt m i] returns the current binding of [i] in [m], or [None]
    if no such binding exists. O(log n) time.  *)
val get_opt : 'a t -> int -> 'a option

(** [get_default m i x] returns the current binding of [i] in [m], or
    [x] if no such binding exists. Raises [Invalid_argument] when [i] is
    negative. O(log n) time.  *)
val get_default : 'a t -> int -> 'a -> 'a

(** [choose m] returns an arbitrary key/value pair of [m], or raises
    [Not_found] when [m] is empty. O(log n) time.  *)
val choose : 'a t -> int * 'a

(** [pop m] returns [i, x, m'], where [i, x] is an arbitrary key/value
    pair of [m], and [m'] is [m] with [i] removed. Raises [Not_found] when
    [m] is empty. O(log n) time.  *)
val pop : 'a t -> int * 'a * 'a t

(** [set m i x] returns a map containing the same bindings as [m],
    plus a binding of [i] to [x]. If [x] was already bound in [m], its
    previous binding disappears. Raises [Invalid_argument] when [i] is
    negative. O(log n) time.  *)
val set : 'a t -> int -> 'a -> 'a t

(** Like [set m i x], except that it raises [Already_present] if there
    is already a binding of [i] in [m]. O(log n) time.  *)
val add : 'a t -> int -> 'a -> 'a t

(** Like [set m i x], except that it raises [Not_found] if there
    is not already a binding of [i] in [m]. O(log n) time.  *)
val update : 'a t -> int -> 'a -> 'a t

(** [modify f m i] returns [m] where the binding [x] of [i] is
    replaced by [f x]. Raises [Not_found] when [i] has no binding.
    O(log n) time.  *)
val modify : ('a -> 'a) -> 'a t -> int -> 'a t

(** [modify_default f m i x] returns [m] where the binding [y] of [i]
    is replaced by [f y], or set to [f x] if [i] has no binding. O(log n)
    time.  *)
val modify_default : ('a -> 'a) -> 'a t -> int -> 'a -> 'a t

(** [delete m i] returns a map containing the same bindings as [m],
    except for [i] which is unbound in the returned map. Raises
    [Not_found] when [i] has no binding.  *)
val delete : 'a t -> int -> 'a t

(** [remove m i] returns a map containing the same bindings as [m],
    except for [i] which is unbound in the returned map. Returns [m]
    unchanged when [i] has no binding.  *)
val remove : 'a t -> int -> 'a t

(** [map f m] returns a map with same domain as [m], where the
    associated value [x] of all bindings of [m] has been replaced by the
    result of the application of [f] to [a]. The bindings are passed to
    [f] in increasing order of the keys.  *)
val map : (int -> 'a -> 'b) -> 'a t -> 'b t
  
(** [fold f m a] computes [(f iN xN ... (f i1 x1 a)...)], where [i1
    ... iN] are the keys of all bindings in [m] (in increasing order),
    and [x1 ... xN] are the associated data.  *)
val fold : ('b -> int -> 'a -> 'b) -> 'a t -> 'b -> 'b

(** [iter f m] calls [f i1 x1, ..., f iN xN], where [i1 ... iN] are
    the keys of all bindings in [m] (in increasing order), and [x1 ... xN]
    are the associated data.  *)
val iter : (int -> 'a -> unit) -> 'a t -> unit

(** [filter p m] retains a map containing those pairs [k, x] from [m]
    for which [p k x] returns true.  *)
val filter : (int -> 'a -> bool) -> 'a t -> 'a t

(** [output p c m] prints a debug representation of [s] to channel
    [c], using [p] to print values.  O(n) time.  *)
val output : (out_channel -> 'a -> unit) -> out_channel -> 'a t -> unit
