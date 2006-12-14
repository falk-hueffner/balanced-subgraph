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

(** Edge-labeled undirected functional graphs. Vertices are represented
    by nonnegative integers. No multiple edges are possible (but
    self-loops are).  *)

(** The type of a graph, parameterized by the edge label type.  *)
type 'a t

(** The empty graph.  *)
val empty : 'a t

(** [neighbors g i] returns a map that maps neighbors of [i] in [g] to
    the corresponding edge label. Raises [Not_found] if [i] is not in [g].
    O(log n) time.  *)
val neighbors : 'a t -> int -> 'a IntMap.t

(** [new_vertex g] returns [g', i], where [g'] is [g] with an
    additional new vertex [i]. O(log n) time.  *)
val new_vertex : 'a t -> 'a t * int

(** [connect g v w] returns [g] with vertices [v] and [w] connected.
    Raises [Not_found] when [v] or [w] do not exist in [g]. Raises
    [IntMap.Already_present] when [v] and [w] are already connected.
    O(log n) time.  *)
val connect : 'a t -> int -> int -> 'a -> 'a t

(** [fold_edges f g a] computes [(f iN jN ... (f i1 j1 a)...)], where
    [(i1, j1) ... (iN, jN)] are the edges of [g]. O(m) time.  *)
val fold_edges : ('b -> int -> int -> 'a -> 'b) -> 'a t -> 'b -> 'b
  
(** [iter_edges f g] calls [f u v] for each edge [(u, v)] in
    [g]. O(m) time.  *)
val iter_edges : (int -> int -> 'a -> unit) -> 'a t -> unit

(** [output c output_label g] prints a debug representation of [g] to
    channel [c]. O(m) time.  *)
val output : out_channel -> (out_channel -> 'a -> unit) -> 'a t -> unit
