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

module StringMap = Map.Make(struct type t = string let compare = String.compare end);;

let input_graph channel =
  let strip_comment s =
    if String.contains s '#' then String.sub s 0 (String.index s '#') else s in      
  let rec loop g vertex_numbers vertex_names =
    try
      let line = strip_comment (input_line channel) in
	match Util.split_string line with
	    [] -> loop g vertex_numbers vertex_names
          | [v; w] ->
	      let vertex_number g vertex_numbers vertex_names v =
		if StringMap.mem v vertex_numbers
		then g, vertex_numbers, vertex_names, StringMap.find v vertex_numbers
		else
		  let g, i = Graph.new_vertex g in
		    g, StringMap.add v i vertex_numbers, IntMap.add vertex_names i v, i in
              let g, vertex_numbers, vertex_names, i =
		vertex_number g vertex_numbers vertex_names v in
              let g, vertex_numbers, vertex_names, j =
		vertex_number g vertex_numbers vertex_names w in
		loop (Graph.connect g i j) vertex_numbers vertex_names
          | _ -> invalid_arg "bad edge syntax"
    with End_of_file -> g, vertex_numbers, vertex_names
  in
    loop Graph.empty StringMap.empty IntMap.empty
;;

let iter_vertex_pairs f g =
  Graph.iter_vertices
    (fun i _ ->
	Graph.iter_vertices
	  (fun j _ -> if i < j then f i j)
	  g)
    g
;;

let () =
  let g, vertex_numbers, vertex_names = input_graph stdin in
  (* Graph.output stderr g; *)
  (*
  let components = Cut.biconnected_components g in
    List.iter
      (fun component ->
	 let g' = Graph.subgraph g component in
	   Graph.output stdout g')
      components;     
  *)
    Cut.cut_corner g;

;;
