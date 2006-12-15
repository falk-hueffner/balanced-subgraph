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

let vertex_cut_transform g =
  let d = (Graph.max_vertex g) + 1 in
  let entry i = i in
  let outlet i = i + d in
  let g' = Graph.fold_vertices
    (fun g' i _ ->
       let g' = Digraph.add_vertex g' (entry i) in
       let g' = Digraph.add_vertex g' (outlet i) in
       let g' = Digraph.connect g' (entry i) (outlet i) in
	 g')
    g
    Digraph.empty in
  Graph.fold_edges
    (fun g' i j ->
       let g' = Digraph.connect g' (outlet i) (entry j) in
       let g' = Digraph.connect g' (outlet j) (entry i) in
	 g')
    g
    g'
;;

let is_n_connected g n v w =
  let g' = vertex_cut_transform g in
  let d = (Graph.max_vertex g) + 1 in
  let outlet_v = v + d in
  let entry_w = w in
  let flow = Diflow.make g' (fun _ _ -> 1) in
  let rec loop i flow =
    if i >= n || Diflow.influx flow outlet_v < i
    then flow
    else loop (i + 1) (Diflow.augment flow outlet_v entry_w) in
  let flow = loop 0 flow in
    Diflow.influx flow outlet_v >= n
;;
