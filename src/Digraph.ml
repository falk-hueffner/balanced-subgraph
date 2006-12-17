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

type t = (IntSet.t * IntSet.t) IntMap.t (* preds, succs *);;

let empty = IntMap.empty;;

let has_vertex = IntMap.has_key;;

let add_vertex g i = IntMap.add g i (IntSet.empty, IntSet.empty);;
let new_vertex g =
  let i = if IntMap.is_empty g then 0 else (IntMap.max_key g) + 1
  in
    add_vertex g i, i
;;

let neighbors = IntMap.get;;
let succs g i = let _, succs = IntMap.get g i in succs;;
let preds g i = let preds, _ = IntMap.get g i in preds;;

let has_arc g i j = IntSet.contains (succs g i) j;;
let get_label g i j = IntMap.get (succs g i) j;;

let connect g i j =
  let preds_i, succs_i = IntMap.get g i in
  let preds_j, succs_j = IntMap.get g j in
  let succs_i = IntSet.add succs_i j in
  let preds_j = IntSet.add preds_j i in
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

let fold_arcs f g accu =
  fold_vertices
    (fun accu i _ succs ->
       IntSet.fold
         (fun accu j -> f accu i j)
         succs
         accu)
    g
    accu
;;

let iter_arcs f g = fold_arcs (fun () i j -> f i j) g ();;

let output channel g =
  Printf.fprintf channel "{\n";
  (* List degree-0 vertices.  *)
  iter_vertices (fun i preds succs ->
                   if IntSet.is_empty preds && IntSet.is_empty succs
                   then Printf.fprintf channel "%d\n" i) g;
  iter_arcs (fun i j ->  Printf.fprintf channel "%d %d\n" i j) g;
  Printf.fprintf channel "}\n";
;;
