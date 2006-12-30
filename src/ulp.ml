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

module StringMap = Map.Make(struct type t = string    let compare = String.compare end);;
module GadgetMap = Map.Make(struct type t = int array let compare = compare end);;

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
(*   Printf.eprintf "solve_brute_force\tn = %3d m = %4d\n" (ELGraph.num_vertices g) (ELGraph.num_edges g); *)
(*   Printf.eprintf "g = %a\n" (fun c -> ELGraph.output c output_edge) g; *)
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
	 ()(* Printf.printf "%2d %b\n" v (best_colors land (1 lsl (IntMap.get numbers v)) <> 0) *)
      )
      g;
(*     Printf.printf "deletions: %d\n" best_del; *)
    best_del;
;;

let reduce_cut g s c =
  let g' = ELGraph.subgraph g (IntSet.union s c) in
(*   Printf.eprintf "reduce_cut c = %a g = %a\n" IntSet.output s (fun c -> ELGraph.output c output_edge) g'; *)
  let merge g v1 v2 =
(*     Printf.eprintf "merge %d %d\n" v1 v2; *)
    let g = 
      ELGraph.fold_neighbors
	(fun g j { equal = eq1; unequal = un1 } ->
	   ELGraph.modify_label_default
	     (fun { equal = eq2; unequal = un2 } -> { equal = eq1 + eq2; unequal = un1 + un2 })
	     g v1 j { equal = 0; unequal = 0 })
	g v2 g in
      ELGraph.delete_vertex g v2 in
  let c_n = IntSet.size c in
  let g' = ELGraph.subgraph g (IntSet.union s c) in
  let g', w = ELGraph.new_vertex g' in
  let g', b = ELGraph.new_vertex g' in
  let g' = ELGraph.connect g' b w { equal = 0; unequal = 1 lsl 24; } in
  let rec loop c_colors results =
      if c_colors >= (1 lsl (c_n - 1))
      then List.rev results
      else
	let g', _ = IntSet.fold
	  (fun (g', i) v ->
	     (if c_colors land (1 lsl i) <> 0
	      then merge g' b v
	      else merge g' w v), (i + 1))
	  c
	  (g', 0)
	in
	  (* 	ELGraph.output stdout output_edge g'; *)
	  loop (c_colors + 1) ((solve_brute_force g') :: results)
    in
      loop 0 []
;;

let rec solve_cut_corner g =
  Printf.eprintf "solve_cut_corner\tn = %3d m = %4d\n" (ELGraph.num_vertices g) (ELGraph.num_edges g);
  let s, c = Cut.cut_corner (ELGraph.unlabeled g) in
    Printf.eprintf "xx s = %a c = %a\n%!" IntSet.output s IntSet.output c;
    (*
(*     ELGraph.output stderr output_edge g; *)
    let r = reduce_cut g s c in
      Util.output_list stderr Util.output_int r; prerr_newline();
    let m = List.fold_left min max_int r in
    let r = List.map (fun i -> i - m) r in
      Util.output_list stderr Util.output_int r; prerr_newline();
    *)

    let g = IntSet.fold ELGraph.delete_vertex s g in
      solve_cut_corner g
      
    
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
    solve_cut_corner g
;;

(* G is biconnected.  *)
let solve_component g =
   Printf.eprintf "solve_component\tn = %3d m = %4d\n"
     (ELGraph.num_vertices g) (ELGraph.num_edges g);
  if is_sign_consistent g
  then prerr_string " already sign consistent\n"
  else if ELGraph.num_vertices g <= 5
  then ignore (solve_brute_force g)
  else solve_deg2 g
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

let find_gadgets c_size s_size max_mult =
  let g = Util.fold_n ELGraph.add_vertex (c_size + s_size) ELGraph.empty in
  ELGraph.output stdout output_edge g;
  let l_min = -max_mult and l_max = max_mult in
  let l = Array.make m l_min in
  let bump () =
    let rec loop i =
      if i >= Array.length l
      then false
      else if l.(i) >= l_max
      then begin l.(i) <- l_min; loop (i + 1) end
      else begin l.(i) <- l.(i) + 1; true end
    in
      loop 0
  in
    l.(0) <- l_min - 1;
    while bump () do
    done

;;

let () =
  (*
  let g, vertex_numbers, vertex_names = input_signed_graph stdin in
  let m = ELGraph.fold_edges (fun m i j { equal = eq; unequal = ne } -> if i > j then m else m + eq + ne) g 0 in
    ELGraph.output stdout output_edge g;
    Printf.printf "m = %d\n" m;
  *)
  find_gadgets 3 1 2
    
    (***
  let g, vertex_numbers, vertex_names = input_signed_graph stdin in
    solve g;
    exit 0;
    ***)
    (*
    ELGraph.output stderr output_edge g;
    let s = IntSet.of_list [3; 4] in
    let c = IntSet.of_list [0; 1; 2] in
      reduce_cut g s c
    *)


(*
  
  let g = ELGraph.empty in
  let g = ELGraph.add_vertex g 0 in
  let g = ELGraph.add_vertex g 1 in
  let g = ELGraph.add_vertex g 2 in
  let g = ELGraph.add_vertex g 3 in
(*   let g = ELGraph.add_vertex g 4 in *)
  let edges = [(0, 1); (1, 2); (0, 2)] in
  let edges = [(0, 1); (1, 2); (0, 2); (0, 3); (1, 3); (2, 3)] in
  let s = IntSet.empty in
  let s = IntSet.singleton 3 in
(*   let edges = [(0, 1); (1, 2); (0, 2); (0, 3); (1, 3); (2, 3); (0, 4); (1, 4); (2, 4); (3, 4)] in *)
  let m = List.length edges in
  let l_min = -5 and l_max = 5 in
  let l = Array.make m l_min in
  let bump () =
    let rec loop i =
      if i >= Array.length l
      then false
      else if l.(i) >= l_max
      then begin l.(i) <- l_min; loop (i + 1) end
      else begin l.(i) <- l.(i) + 1; true end
    in
      loop 0
  in
    l.(0) <- l_min - 1;
    while bump () do
(*       print_string "**l = "; Array.iter (fun i -> Printf.printf "%d " i) l; print_newline(); *)
      let g, _ = List.fold_left
	(fun (g, i) (v, w) ->
	   if l.(i) > 0
	   then ELGraph.connect g v w { equal = l.(i); unequal = 0 }, i + 1
	   else if l.(i) < 0
	   then ELGraph.connect g v w { equal = 0; unequal = -l.(i) }, i + 1
	   else g, i + 1)
	(g, 0)
	edges
      in
(* 	ELGraph.output stdout output_edge g; *)
	let r = reduce_cut g s (IntSet.of_list [0; 1; 2]) in
(* 	let r = reduce_cut g (IntSet.of_list [3]) (IntSet.of_list [0; 1; 2]) in *)
 	let m = List.fold_left min max_int r in
(*  	let r = List.map (fun i -> i - m) r in *)
(* 	  Util.output_list stdout Util.output_int r; *)
(* 	  ELGraph.output stdout output_edge g; *)
	  List.iter (fun i -> Printf.printf "%d " i) r;
	  print_newline ();
    done
	
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

*)

  
;;
