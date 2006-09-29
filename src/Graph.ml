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

type 'a t = 'a IntMap.t IntMap.t;;

let empty = IntMap.empty;;

let has_vertex = IntMap.has_key;;

let new_vertex g =
  let i = if IntMap.is_empty g then 0 else (IntMap.max_key g) + 1 in
    IntMap.add g i IntMap.empty, i
;;

let neighbors = IntMap.get;;

let connect g i j label =
  if not (has_vertex g i && has_vertex g j) then invalid_arg "Graph.connect: invalid vertex";
  if i = j then invalid_arg "Graph.connect: cannot handle self loop";
  let neighbors_i = neighbors g i in
  if IntMap.has_key neighbors_i j then invalid_arg "Graph.connect: cannot handlde double edge";
  let neighbors_j = neighbors g j in
  let neighbors_i' = IntMap.add neighbors_i j label in
  let neighbors_j' = IntMap.add neighbors_j i label in
  let g = IntMap.add g i neighbors_i' in
  let g = IntMap.add g j neighbors_j' in
    g
;;

let fold_vertices f g x = IntMap.fold f g x;;
let iter_vertices f g = fold_vertices (fun () i neighbors -> f i neighbors) g ();;

let fold_edges f g accu =
  fold_vertices
    (fun accu i neighbors ->
       IntMap.fold
         (fun accu j label -> if i < j then f accu i j label else accu)
         neighbors
         accu)
    g
    accu
;;

let iter_edges f g = fold_edges (fun () i j -> f i j) g ();;

let output channel output_label g =
  Printf.fprintf channel "{\n";
  iter_vertices (fun i neighbors ->
                   if IntMap.is_empty neighbors
                   then Printf.fprintf channel "%d\n" i) g;
  iter_edges (fun i j l ->
		Printf.fprintf channel "%d %d " i j;
		output_label channel l;
		Printf.fprintf channel "\n") g;
  Printf.fprintf channel "}\n";
;;
