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

module StringMap = Map.Make(struct type t = string let compare = String.compare end);;

type sign = Equal | Unequal
type edge = {
  equal_edges:   int;
  unequal_edges: int;
};;

let output_edge channel e =
  if e.equal_edges > 0 then begin
    output_char channel '+';
    if e.equal_edges > 1 then Printf.fprintf channel "(*%d)" e.equal_edges;
  end;
  if e.unequal_edges > 0 then begin
    if e.equal_edges > 0 then output_char channel ' ';
    output_char channel '-';
    if e.unequal_edges > 1 then Printf.fprintf channel "(*%d)" e.unequal_edges;
  end
;;

let input_signed_graph channel =
  let strip_comment s =
    if String.contains s '#' then String.sub s 0 (String.index s '#') else s in      
  let rec loop g vertex_numbers vertex_names lineno =
    try
      let line = strip_comment (input_line channel) in
	match Util.split_string line with
	    [] -> loop g vertex_numbers vertex_names (lineno + 1)
          | [v; w; s] ->
	      let vertex_number g vertex_numbers vertex_names v =
		if StringMap.mem v vertex_numbers
		then g, vertex_numbers, vertex_names, StringMap.find v vertex_numbers
		else
		  let g, i = ELGraph.new_vertex g in
		    g, StringMap.add v i vertex_numbers, IntMap.add vertex_names i v, i in
              let g, vertex_numbers, vertex_names, i =
		vertex_number g vertex_numbers vertex_names v in
              let g, vertex_numbers, vertex_names, j =
		vertex_number g vertex_numbers vertex_names w in
(*  		Printf.eprintf "%d %d %d\n%!" lineno i j; *)
	      let label =
		if ELGraph.is_connected g i j
		then ELGraph.get_label g i j
		else { equal_edges = 0; unequal_edges = 0 } in
	      let label =
		if s = "1" || s = "+"
		then { label with   equal_edges = label.  equal_edges + 1}
		else { label with unequal_edges = label.unequal_edges + 1} in
	      let g = ELGraph.set_label g i j label in
		loop g vertex_numbers vertex_names (lineno + 1)
          | _ -> invalid_arg "bad edge syntax"
    with End_of_file -> g, vertex_numbers, vertex_names
  in
    loop ELGraph.empty StringMap.empty IntMap.empty 1
;;

let iter_vertex_pairs f g =
  Graph.iter_vertices
    (fun i _ ->
	Graph.iter_vertices
	  (fun j _ -> if i < j then f i j)
	  g)
    g
;;

let is_sign_consistent g =
  let rec dfs v color colors =
    if IntMap.has_key colors v
    then IntMap.get colors v = color
    else
      let colors = IntMap.set colors v color in
	ELGraph.fold_neighbors
	  (fun consistent w e ->
	     consistent
	     && (not (e.equal_edges > 0 && e.unequal_edges > 0))
	     && if e.equal_edges > 0
	       then dfs w color colors
	       else dfs w (not color) colors)
	  g v true
  in
    dfs (ELGraph.max_vertex g) false IntMap.empty
;;
  
  
let () =
  let g, vertex_numbers, vertex_names = input_signed_graph stdin in
    ELGraph.output stderr output_edge g;

    (*
  let components = Cut.biconnected_components (ELGraph.unlabeled g) in
    List.iter
      (fun component ->
	 let g' = ELGraph.subgraph g component in
	   ELGraph.output stdout output_edge g')
      components;
    *)
  let l = Cut.cut_corner (ELGraph.unlabeled g) in
    List.iter
      (fun (s, exits) ->
	 if IntSet.size exits <= 2 then
	   Printf.eprintf "exits = %a g = %a\n"
	     IntSet.output exits
	     (fun channel g -> ELGraph.output channel output_edge g) (ELGraph.subgraph g s))
      l

;;
