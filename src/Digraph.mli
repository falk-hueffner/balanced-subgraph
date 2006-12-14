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

(** Directed functional graphs. Vertices are represented
    by nonnegative integers. No multiple arcs are possible (but
    self-loops are).  *)

(** The type of a graph, parameterized by the arc label type.  *)
type t

(** The empty graph.  *)
val empty : t

(** [has_arc g i j] returns true if there is an arc from [i] to
    [j]. Raises [Not_found] when [i] or [j] are not in [g]. *)
val has_arc : t -> int -> int -> bool

(** [neighbors g i] returns a tuple of the predecessors and the
    successors of [i] in [g]. *)
val neighbors : t -> int -> IntSet.t * IntSet.t

(** [new_vertex g] returns [g', i], where [g'] is [g] with an
    additional new vertex [i]. O(log n) time.  *)
val new_vertex : t -> t * int

(** [connect g v w] returns [g] with an additional arc from [v] to
    [w].  Raises [Not_found] when [v] or [w] do not exist in [g]. Raises
    [IntMap.Already_present] when there is already an arc from [v] to
    [w]. O(log n) time.  *)
val connect : t -> int -> int -> t

(** [fold_vertices f g a] computes [(f iN pN sN ... (f i1 p1 s1
    a)...)], where [i1, ... iN] are the vertices of [g], [pN] is the set
    of predecessors of [iN], and [sN] is the set of successors of [iN].
    O(n) time.  *)
val fold_vertices : ('a -> int -> IntSet.t -> IntSet.t -> 'a) -> t -> 'a -> 'a

(** [fold_arcs f g a] computes [(f iN jN ... (f i1 j1 a)...)], where
    [(i1, j1) ... (iN, jN)] are the arcs of [g]. O(m) time.  *)
val fold_arcs : ('a -> int -> int -> 'a) -> t -> 'a -> 'a

(** [iter_arcs f g] calls [f u v] for each arc [(u, v)]. O(m) time.  *)
val iter_arcs : (int -> int -> unit) -> t -> unit

(** [output c g] prints a debug representation of [g] to
    channel [c]. O(m) time.  *)
val output : out_channel -> t -> unit
