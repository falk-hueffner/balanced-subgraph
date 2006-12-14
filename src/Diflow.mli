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

(** Directed network flow. Having both arcs (x, y) and (y, x) present
    will probably confuse some functions.  *)

(** The type of a flow network.  *)
type t

(** [make g cap] creates a flow network from [g], where arc [(i, j)]
    has capacity [cap i j]. O(m) time. *)
val make : Digraph.t -> (int -> int -> int) -> t

(** [augment g s t] tries to find an augmenting path from [s] to [t]
    in [g] and then augments the flow along it. If no such path exists, g
    is returned unchanged. O(m) time.  *)
val augment : t -> int -> int -> t

(** [output c g] prints a debug representation of [g] to channel [c].
    O(m) time.  *)
val output : out_channel -> t -> unit
