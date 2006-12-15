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

type t = IntSet.t IntMap.t;;

let empty = IntMap.empty;;

let has_vertex = IntMap.has_key;;
let neighbors = IntMap.get;;
let max_vertex = IntMap.max_key;;

let new_vertex g =
  let i = if IntMap.is_empty g then 0 else (IntMap.max_key g) + 1 in
    IntMap.add g i IntSet.empty, i
;;

let connect g i j =
  if not (has_vertex g i && has_vertex g j) then invalid_arg "Graph.connect: invalid vertex";
  let neighbors_i = neighbors g i in
  let neighbors_j = neighbors g j in
  let neighbors_i' = IntSet.add neighbors_i j in
  let neighbors_j' = IntSet.add neighbors_j i in
  let g = IntMap.set g i neighbors_i' in
  let g = IntMap.set g j neighbors_j' in
    g
;;

let fold_vertices = IntMap.fold;;
let iter_vertices f g = fold_vertices (fun () i neighbors -> f i neighbors) g ();;

let vertex_set g = fold_vertices (fun s i _ -> IntSet.add s i) g IntSet.empty;;

let fold_edges f g accu =
  fold_vertices
    (fun accu i neighbors ->
       IntSet.fold
         (fun accu j -> if i < j then f accu i j else accu)
         neighbors
         accu)
    g
    accu
;;

let iter_edges f g = fold_edges (fun () i j -> f i j) g ();;

let output channel g =
  Printf.fprintf channel "{\n";
  (* List degree-0 vertices.  *)
  iter_vertices (fun i neighbors ->
                   if IntSet.is_empty neighbors
                   then Printf.fprintf channel "%d\n" i) g;
  iter_edges (fun i j -> Printf.fprintf channel "%d %d\n" i j) g;
  Printf.fprintf channel "}\n";
;;
