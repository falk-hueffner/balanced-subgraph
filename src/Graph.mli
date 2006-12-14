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

(** Undirected functional graphs. Vertices are represented
    by nonnegative integers. No multiple edges are possible (but
    self-loops are).  *)

(** The type of a graph.  *)
type t

(** The empty graph.  *)
val empty : t

(** [neighbors g i] returns the set of neighbors of [i] in [g]. Raises
    [Not_found] if [i] is not in [g]. O(log n) time.  *)
val neighbors : t -> int -> IntSet.t

(** [new_vertex g] returns [g', i], where [g'] is [g] with an
    additional new vertex [i]. O(log n) time.  *)
val new_vertex : t -> t * int

(** [connect g v w] returns [g] with vertices [v] and [w] connected.
    Raises [Not_found] when [v] or [w] do not exist in [g]. Raises
    [IntMap.Already_present] when [v] and [w] are already connected.
    O(log n) time.  *)
val connect : t -> int -> int -> t

(** [fold_edges f g a] computes [(f iN jN ... (f i1 j1 a)...)], where
    [(i1, j1) ... (iN, jN)] are the edges of [g]. O(m) time.  *)
val fold_edges : ('a -> int -> int -> 'a) -> t -> 'a -> 'a

(** [iter_edges f g] calls [f u v] for each edge [(u, v)] in [g].
    O(m) time.  *)
val iter_edges : (int -> int -> unit) -> t -> unit

(** [output c g] prints a debug representation of [g] to
    channel [c]. O(m) time.  *)
val output : out_channel -> t -> unit
