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

type 'a t = ('a IntMap.t * 'a IntMap.t) IntMap.t (* preds, succs *);;

let empty = IntMap.empty;;

let has_vertex = IntMap.has_key;;

let new_vertex g =
  let i = if IntMap.is_empty g then 0 else (IntMap.max_key g) + 1 in
    IntMap.add g i (IntMap.empty, IntMap.empty), i
;;

let succs g i = let _, succs = IntMap.get g i in succs;;
let preds g i = let preds, _ = IntMap.get g i in preds;;

let connect g i j label =
  if not (has_vertex g i && has_vertex g j) then invalid_arg "Graph.connect: invalid vertex";
  let preds_i, succs_i = IntMap.get g i in
  let preds_j, succs_j = IntMap.get g i in
  let succs_i = IntMap.add succs_i j label in
  let preds_j = IntMap.add preds_j i label in
  let g = IntMap.set g i (preds_i, succs_i) in
  let g = IntMap.set g j (preds_j, succs_j) in
    g
;;

let fold_vertices f g x =
  IntMap.fold
    (fun accu i (preds, succs) -> f accu i preds succs)
    g
    x
;;
let iter_vertices f g = fold_vertices (fun () i preds succs -> f i preds succs) g ();;

let fold_edges f g accu =
  fold_vertices
    (fun accu i _ succs ->
       IntMap.fold
         (fun accu j label -> f accu i j label)
         succs
         accu)
    g
    accu
;;

let iter_edges f g = fold_edges (fun () i j l -> f i j l) g ();;

let output channel output_label g =
  Printf.fprintf channel "{\n";
  (* List degree-0 vertices.  *)
  iter_vertices (fun i preds succs ->
                   if IntMap.is_empty preds && IntMap.is_empty succs
                   then Printf.fprintf channel "%d\n" i) g;
  iter_edges (fun i j l ->
		Printf.fprintf channel "%d %d " i j;
		output_label channel l;
		Printf.fprintf channel "\n") g;
  Printf.fprintf channel "}\n";
;;
