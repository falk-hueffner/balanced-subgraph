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

(** Functional queues.  *)

(** The type of a queue.  *)
type 'a t

(** The empty queue.  *)
val empty : 'a t

(** [push q x] prepends [x] to [q]. Amortized O(1). *)
val push : 'a t -> 'a -> 'a t

(** [pop q] returns a tuple of the top element and [q] without the top
    element. Amortized O(1).  *)
val pop : 'a t -> 'a * 'a t
