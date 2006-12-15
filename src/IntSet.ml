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

(* Because Ocaml is too lame to optimize it, this wastes one word per
   node, and some computation time. Might optimize by hand later. *)
type t = unit IntMap.t;;

exception Already_present;;

let empty = IntMap.empty;;

let is_empty = IntMap.is_empty;;

let contains = IntMap.has_key;;
let draw = IntMap.max_key;;

let put s x = IntMap.set s x ();;
let add s x = IntMap.add s x ();;

let rec fold f = IntMap.fold (fun a i () -> f a i);;

let output channel s =
  Printf.fprintf channel "{";  
  ignore (fold
    (fun first i ->
       if not first then Printf.fprintf channel ", ";
       Printf.fprintf channel "%d" i;
       false)
    s
    true);
  Printf.fprintf channel "}";
;;
