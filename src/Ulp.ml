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

type sign = Eq | Ne

type edge = {
  eq: int;
  ne: int;
};;

type graph = edge ELGraph.t;;

let strip_comment s =
  if String.contains s '#' then String.sub s 0 (String.index s '#') else s
;;

let input channel =
  let rec loop g lineno =
    try
      let line = strip_comment (input_line channel) in
	match Util.split_string line with
	    [] -> loop g (lineno + 1)
          | [v; w; s] ->
	      let i = int_of_string v in
	      let j = int_of_string w in
	      let g = ELGraph.set_vertex g i in
	      let g = ELGraph.set_vertex g j in
	      let g = ELGraph.modify_label_default
		(fun label ->
		   if s = "0"
		   then { label with eq = label.eq + 1}
		   else if s = "1"
		   then { label with ne = label.ne + 1}
		   else invalid_arg "bad edge label")
		g i j { eq = 0; ne = 0 }
	      in
		loop g (lineno + 1)
          | _ -> invalid_arg "bad edge syntax"
    with End_of_file -> g
  in
    loop ELGraph.empty 0
;;

let input_named channel =
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
	      let label =
		if ELGraph.is_connected g i j
		then ELGraph.get_label g i j
		else { eq = 0; ne = 0 } in
	      let label =
		if s = "1" || s = "+"
		then { label with eq = label.eq + 1}
		else { label with ne = label.ne + 1} in
	      let g = ELGraph.set_label g i j label in
		loop g vertex_numbers vertex_names (lineno + 1)
          | _ -> invalid_arg "bad edge syntax"
    with End_of_file -> g, vertex_numbers, vertex_names
  in
    loop ELGraph.empty StringMap.empty IntMap.empty 1
;;


let output_edge channel e =
  if e.eq > 0 then begin
    output_char channel '+';
    if e.eq > 1 then Printf.fprintf channel "(*%d)" e.eq;
  end;
  if e.ne > 0 then begin
    if e.eq > 0 then output_char channel ' ';
    output_char channel '-';
    if e.ne > 1 then Printf.fprintf channel "(*%d)" e.ne;
  end
;;

let output channel = ELGraph.output channel output_edge;;

let solve_brute_force g =
  let n = ELGraph.num_vertices g in
  let numbers, _ = ELGraph.fold_vertices
    (fun (numbers, n) i _ -> IntMap.add numbers i n, n + 1) g (IntMap.empty, 0) in
  let rec loop best_del best_edges colors =
    if colors >= (1 lsl (n - 1))
    then best_del, best_edges
    else
      let color v = colors land (1 lsl (IntMap.get numbers v)) <> 0 in
      let del, edges =
	ELGraph.fold_edges
	  (fun (del, edges) v w { eq = eq; ne = ne } ->
	     if color v = color w
	     then (if ne = 0 then del, edges else del + ne, (v, w, Ne) :: edges)
	     else (if eq = 0 then del, edges else del + eq, (v, w, Eq) :: edges))
	  g
	  (0, [])
      in
(* 	Printf.fprintf stdout "colors = %d del = %d edges = %a\n%!" colors del *)
(* 	(Util.output_list (fun c (i, j, l) -> Printf.fprintf c "%d %d %b" i j (l = Ne))) edges; *)
	if del < best_del
	then loop del edges (colors + 1)
	else loop best_del best_edges (colors + 1) in
  let best_del, best_edges = loop max_int [] 0 in
    best_edges;
;;

let solve g =
  solve_brute_force g
;;

