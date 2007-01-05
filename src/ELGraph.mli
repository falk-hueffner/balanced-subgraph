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

(** True if the graph contains no vertices.  *)
val is_empty : 'a t -> bool
  
(** Number of vertices. O(1) time.  *)
val num_vertices : 'a t -> int

(** Number of edges. O(n) time.  *)
val num_edges : 'a t -> int

val has_vertex : 'a t -> int -> bool

(** Returns the vertex set of the graph. O(n) time.  *)
val vertex_set : 'a t -> IntSet.t

(** [neighbors g i] returns a map that maps neighbors of [i] in [g] to
    the corresponding edge label. Raises [Not_found] if [i] is not in [g].
    O(log n) time.  *)
val neighbors : 'a t -> int -> 'a IntMap.t

(** [deg g i] returns the number of neighbors of [i] in [g]. O(log n)
    time.  *)
val deg : 'a t -> int -> int

(** [max_vertex g] returns the highest vertex of [g], or raises
    [Not_found] if [g] is empty.  *)
val max_vertex : 'a t -> int

(** [new_vertex g] returns [g', i], where [g'] is [g] with an
    additional new vertex [i]. O(log n) time.  *)
val new_vertex : 'a t -> 'a t * int

val add_vertex : 'a t -> int -> 'a t
val set_vertex : 'a t -> int -> 'a t

(** [is_connected g i j] returns true if there is an edge between [i]
    and [j] in [g]. O(log n) time. *)
val is_connected : 'a t -> int -> int -> bool

(** [get_label g i j] returns the label of the edge [(i, j)], or
    raises [Not_found] when there is no such edge. O(log n) time. *)
val get_label : 'a t -> int -> int -> 'a

val get_label_default : 'a t -> int -> int -> 'a -> 'a

(** [connect g v w l] returns [g] with vertices [v] and [w] connected
    and labeled by [l].  Raises [Not_found] when [v] or [w] do not exist
    in [g]. Raises [IntMap.Already_present] when [v] and [w] are already
    connected.  O(log n) time.  *)
val connect : 'a t -> int -> int -> 'a -> 'a t

val disconnect : 'a t -> int -> int -> 'a t
val unconnect : 'a t -> int -> int -> 'a t

(** [set_label g i j l] sets the label of the edge [(i, j)] to l. If
    [(i, j)] is not already an edge, it will be created. O(log n) time. *)
val set_label : 'a t -> int -> int -> 'a -> 'a t

(** [modify_label_default g f i j l] sets the label [l0] of the edge
    [(i, j)] to [f l0]. If [(i, j)] is not already an edge, it will be
    created and the label set to [f l]. O(log n) time. *)
val modify_label_default : ('a -> 'a) -> 'a t -> int -> int -> 'a -> 'a t

(** [delete_vertex g i] returns [g] with vertex [i] deleted. O(m)
    time.  *)
val delete_vertex : 'a t -> int -> 'a t

(** [fold_neighbors f g i a] computes [(f jN lN ... (f j1 l1 a)...)],
    where [j1, ..., jN] are the neighbors of [i] in [g], and [lN] is the
    label of [(iN, jN)]. O(log n + deg i) time.  *)
val fold_neighbors : ('b -> int -> 'a -> 'b) -> 'a t -> int -> 'b -> 'b

(** [fold_vertices f g a] computes [(f iN nN ... (f i1 n1 a)...)],
    where [i1, ... iN] are the vertices of [g] and [nN] is the map of
    neighbors of [iN]. O(n) time.  *)
val fold_vertices : ('b -> int -> 'a IntMap.t -> 'b) -> 'a t -> 'b -> 'b

(** [iter_vertices f g] calls [f u nu] for each vertex [u] in [g],
    where [nu] is the map of neighbors of [u].  O(n) time.  *)
val iter_vertices : (int -> 'a IntMap.t -> unit) -> 'a t -> unit

(** [fold_edges f g a] computes [(f iN jN lN ... (f i1 j1 lN a)...)],
    where [(i1, j1) ... (iN, jN)] are the edges of [g], and [lN] is the
    label of [(iN, jN)]. O(m) time.  *)
val fold_edges : ('b -> int -> int -> 'a -> 'b) -> 'a t -> 'b -> 'b

(** [iter_edges f g] calls [f u v l] for each edge [(u, v)] with label
    [l] in [g]. O(m) time.  *)
val iter_edges : (int -> int -> 'a -> unit) -> 'a t -> unit

val unlabeled : 'a t -> Graph.t

(** [subgraph g s] returns the subgraph of [g] induced by [s].  O(n^2 log n) time.  *)
val subgraph : 'a t -> IntSet.t -> 'a t

(** [output c output_label g] prints a debug representation of [g] to
    channel [c]. O(m) time.  *)
val output : out_channel -> (out_channel -> 'a -> unit) -> 'a t -> unit
