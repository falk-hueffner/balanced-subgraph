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

(** The ULP solving algorithm.  *)

module StringMap : Map.S

(** Edge label. Invariant: [eq] + [ne] > 0.  *)
type edge = {
  eq: int;				(** multiplicity of positive edges (>= 0) *)
  ne: int;				(** multiplicity of negative edges (>= 0) *)
}

(** A graph suitable to represent ULP instances.  *)
type t = edge ELGraph.t

val num_edges : t -> int

(** Read description from a channel.  *)    
val input : in_channel -> (t * int StringMap.t * string IntMap.t)

(** Write debug representation to a channel.  *)
val output : out_channel -> t -> unit

val output_edge : out_channel -> edge -> unit

val is_sign_consistent : t -> bool

exception Not_sign_consistent

val color : t -> bool IntMap.t

val coloring_cost : t -> bool IntMap.t -> int

val to_array : t -> (int * int) array array
