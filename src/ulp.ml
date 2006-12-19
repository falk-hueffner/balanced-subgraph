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
  equal:   int;
  unequal: int;
};;

let output_edge channel e =
  if e.equal > 0 then begin
    output_char channel '+';
    if e.equal > 1 then Printf.fprintf channel "(*%d)" e.equal;
  end;
  if e.unequal > 0 then begin
    if e.equal > 0 then output_char channel ' ';
    output_char channel '-';
    if e.unequal > 1 then Printf.fprintf channel "(*%d)" e.unequal;
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
		else { equal = 0; unequal = 0 } in
	      let label =
		if s = "1" || s = "+"
		then { label with   equal = label.  equal + 1}
		else { label with unequal = label.unequal + 1} in
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
	     && (not (e.equal > 0 && e.unequal > 0))
	     && if e.equal > 0
	       then dfs w color colors
	       else dfs w (not color) colors)
	  g v true
  in
    dfs (ELGraph.max_vertex g) false IntMap.empty
;;

(* No useful cuts at all in G.  *)
let solve_uncuttable g =
  ()
;;

let solve_brute_force g =
  let n = ELGraph.num_vertices g in
  let numbers, _ = ELGraph.fold_vertices
    (fun (numbers, n) i _ -> IntMap.add numbers i n, n + 1) g (IntMap.empty, 0) in
  let rec loop best_del best_colors colors =
    if colors >= (1 lsl (n - 1))
    then best_del, best_colors
    else
      let color v = colors land (1 lsl (IntMap.get numbers v)) <> 0 in
      let del =
	ELGraph.fold_edges
	  (fun del v w { equal = eq; unequal = un } ->
	     if color v = color w then del + un else del + eq)
	  g
	  0
      in
	if del < best_del
	then loop del colors (colors + 1)
	else loop best_del best_colors (colors + 1) in
  let best_del, best_colors = loop max_int 0 0 in
    ELGraph.iter_vertices
      (fun v _ ->
	 Printf.printf "%2d %b\n" v (best_colors land (1 lsl (IntMap.get numbers v)) <> 0))
      g;
    Printf.printf "deletions: %d\n" best_del;
;;

let reduce_cut g s c =
  let merge g v1 v2 =
    ELGraph.fold_neighbors
      (fun g j { equal = eq1; unequal = un1 } ->
	 ELGraph.modify_label_default
	   (fun { equal = eq2; unequal = un2 } -> { equal = eq1 + eq2; unequal = un1 + un2 })
	   g v1 j { equal = 0; unequal = 0 })
      g v2 g in
  let c_n = IntSet.size c in
  let g' = ELGraph.subgraph g s in
  let g', b = ELGraph.new_vertex g in (*BUG*)

    ELGraph.iter_vertices
      (fun i n -> Printf.eprintf "%3d %3d %a\n" i (ELGraph.deg g' i) (fun c -> IntMap.output c output_edge) n) g';

  let g', w = ELGraph.new_vertex g' in
    prerr_newline();
    for c_colors = 0 to (1 lsl (c_n - 1)) - 1 do
      let g', _ = IntSet.fold
	(fun (g', i) v ->
	   (if c_colors land (1 lsl i) <> 0
	   then merge g' b i
	   else merge g' w i), (i + 1))
	c
	(g', 0)
      in

	ELGraph.iter_vertices
	  (fun i n -> Printf.eprintf "%3d %3d %a\n" i (ELGraph.deg g' i) (fun c -> IntMap.output c output_edge) n) g';
	
	ELGraph.output stdout output_edge g';
    done
;;

let solve_2cut g =
  Printf.eprintf "solve_2cut\tn = %3d m = %4d\n" (ELGraph.num_vertices g) (ELGraph.num_edges g);
  let s, c = Cut.cut_corner (ELGraph.unlabeled g) in
    Printf.eprintf "xx s = %a c = %a\n" IntSet.output s IntSet.output c;
;;

let solve_deg2 g =
  Printf.eprintf "solve_deg2\tn = %3d m = %4d\n"
     (ELGraph.num_vertices g) (ELGraph.num_edges g);
  let g =
    let rec loop g s =
      if IntSet.is_empty s
      then g
      else
	let v, s = IntSet.pop s in
	let n = ELGraph.neighbors g v in
	  if IntMap.size n <> 2
	  then loop g s
	  else begin
	    let v1, { equal = p1; unequal = m1 } =
	      IntMap.choose n in
	    let v2, { equal = p2; unequal = m2 } =
	      IntMap.choose (IntMap.remove n v1) in
(* 	       Printf.eprintf "%d %d / %d %d\n" p1 m1 p2 m2; *)
	      Printf.eprintf "v = %3d v1 = %3d v2 = %3d\n" v v1 v2;
	      let p, m =
		if not (ELGraph.is_connected g v1 v2)
		then 0, 0
		else let { equal = p; unequal = m } = ELGraph.get_label g v1 v2 in p, m in
	      let p = p + min (m1 + p2) (p1 + m2) in
	      let m = m + min (m1 + m2) (p1 + p2) in
	      let g = ELGraph.delete_vertex g v in	  	 
	      let g = ELGraph.set_label g v1 v2 { equal = p; unequal = m } in
	      let s = IntSet.put s v1 in
	      let s = IntSet.put s v2 in
		loop g s
	  end
    in
      loop g (ELGraph.vertex_set g)
  in
    solve_2cut g
;;

(* G is biconnected.  *)
let solve_component g =
   Printf.eprintf "solve_component\tn = %3d m = %4d\n"
     (ELGraph.num_vertices g) (ELGraph.num_edges g);
  if is_sign_consistent g
  then prerr_string " already sign consistent\n"
  else
    solve_deg2 g
;;

let solve g =
  Printf.eprintf "solve\t\tn = %3d m = %4d\n" (ELGraph.num_vertices g) (ELGraph.num_edges g);
  if is_sign_consistent g
  then prerr_string " already sign consistent\n"
  else
    let components = Cut.biconnected_components (ELGraph.unlabeled g) in
      List.iter
	(fun component -> solve_component (ELGraph.subgraph g component))
	components;
;;

let () =
  let g, vertex_numbers, vertex_names = input_signed_graph stdin in
    ELGraph.output stderr output_edge g;
    let s = IntSet.of_list [3; 4] in
    let c = IntSet.of_list [0; 1; 2] in
      reduce_cut g s c
	
(*     solve_brute_force g; *)
    
(*     ELGraph.output stderr output_edge g; *)
(*     solve g; *)
    (*
  let components = Cut.biconnected_components (ELGraph.unlabeled g) in
    List.iter
      (fun component ->
	 let g' = ELGraph.subgraph g component in
	   ELGraph.output stdout output_edge g')
      components;
    *)
    (*
  let l = Cut.cut_corner (ELGraph.unlabeled g) in
    List.iter
      (fun (s, exits) ->
	 if IntSet.size exits <= 2 then
	   Printf.eprintf "exits = %a g = %a\n"
	     IntSet.output exits
	     (fun channel g -> ELGraph.output channel output_edge g) (ELGraph.subgraph g s))
      l
    *)
;;
