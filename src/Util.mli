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

(** General helper functions.  *)

val verbose : bool ref
val max_cut_size : int ref

val timer : unit -> float

val fold_n : ('a -> int -> 'a) -> int -> 'a -> 'a

val list_contains : 'a list -> 'a -> bool
 
(** [output_int channel i] prints [i] on [channel].  *)
val output_int : out_channel -> int -> unit

(** [output_bool channel b] prints [b] as '0' or '1' on [channel].  *)
val output_bool : out_channel -> bool -> unit

(** [output_list c p l] prints a debug representation of list [l] to
    channel [c], using [p] to print values.  O(n) time.  *)
val output_list : (out_channel -> 'a -> unit) -> out_channel -> 'a list -> unit

(** [output_array p c a] prints a debug representation of array [a] to
    channel [c], using [p] to print values.  O(n) time.  *)
val output_array : (out_channel -> 'a -> unit) -> out_channel -> 'a array -> unit

(** [split_string s] splits the string [s] into a list of strings
    which are separated by whitespace.  *)
val split_string : string -> string list
