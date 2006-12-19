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
let num_vertices = IntMap.size;;
let max_vertex = IntMap.max_key;;

let add_vertex g i = IntMap.add g i IntMap.empty;;
let new_vertex g =
  let i = if IntMap.is_empty g then 0 else (IntMap.max_key g) + 1 in
    add_vertex g i, i
;;

let neighbors = IntMap.get;;
let deg g i = IntMap.size (neighbors g i);;

let is_connected g i j = IntMap.has_key (IntMap.get g i) j;;
let get_label g i j = IntMap.get (IntMap.get g i) j;;
let get_label_default g i j l = IntMap.get_default (IntMap.get g i) j l;;

let connect g i j label =
  let g = IntMap.modify (fun neighbors_i -> IntMap.add neighbors_i j label) g i in
  let g = IntMap.modify (fun neighbors_j -> IntMap.add neighbors_j i label) g j in
    g
;;

let set_label g i j label =
  let g = IntMap.modify (fun neighbors_i -> IntMap.set neighbors_i j label) g i in
  let g = IntMap.modify (fun neighbors_j -> IntMap.set neighbors_j i label) g j in
    g
;;

let modify_label_default f g i j label =
  let label = f (get_label_default g i j label) in
    set_label g i j label
;;

let fold_vertices = IntMap.fold;;
let iter_vertices = IntMap.iter;;

let fold_neighbors f g i = IntMap.fold f (neighbors g i);;
let iter_neighbors f g i = IntMap.iter f (neighbors g i);;

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

let iter_edges f g = fold_edges (fun () i j l -> f i j l) g ();;

let vertex_set g = fold_vertices (fun s i _ -> IntSet.add s i) g IntSet.empty;;

let num_edges g =
  let num =
    fold_vertices
      (fun num v neighbors -> num + IntMap.size neighbors + if is_connected g v v then 1 else 0) g 0
  in
    assert (num mod 2 = 0);
    num / 2
;;

let unlabeled g =
  let g' = fold_vertices (fun g' i _ -> Graph.add_vertex g' i) g Graph.empty in
    fold_edges (fun g' i j _ -> Graph.connect g' i j) g g'
;;

let delete_vertex g i =
  let n = neighbors g i in
  let g = IntMap.remove g i in
    IntMap.fold
      (fun g j _ ->
	 IntMap.modify (fun n -> IntMap.remove n i) g j)
      n
      g      
;;

let subgraph g s =
  let g' = IntSet.fold add_vertex s empty in
  fold_edges
    (fun g' i j l ->
       if IntSet.contains s i && IntSet.contains s j
       then connect g' i j l
       else g')
    g
    g'
;;

let output channel output_label g =
  Printf.fprintf channel "{ n = %d, m = %d\n" (num_vertices g) (num_edges g);
  (* List degree-0 vertices.  *)
  iter_vertices (fun i neighbors ->
                   if IntMap.is_empty neighbors
                   then Printf.fprintf channel "%3d\n" i) g;
  iter_edges (fun i j l ->
		Printf.fprintf channel "%3d %3d " i j;
		output_label channel l;
		Printf.fprintf channel "\n") g;
  Printf.fprintf channel "}\n";
;;
