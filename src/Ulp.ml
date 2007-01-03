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

let isdigit = function
    '0' .. '9' -> true
  | _ -> false
;;

(* try int_of_string... is not what we want, since it accepts e.g. "0x17"  *)
let is_int s =
  let rec loop p =
    if p >= String.length s
    then true
    else isdigit s.[p] && loop (p + 1)
  in
    String.length s > 0 && loop 0
;;

let input_named channel =
  let rec loop lines lineno =
    try
      let line = strip_comment (input_line channel) in
      let line = Util.split_string line in
	if line = [] then loop lines (lineno + 1)
	else
	  let v, w, s =
	    match line with
		[v; w; s] -> v, w, s
	      | [v; w]    -> v, w, "1"
	      | _ -> invalid_arg "bad edge syntax" in
	    if s <> "0" && s <> "1" then invalid_arg "bad edge label"
	    else loop ((v, w, s) :: lines) (lineno + 1)
    with End_of_file -> lines in
  let lines = loop [] 0 in
  let vertex_numbers, vertex_names, max_id =
    List.fold_left
      (fun (vertex_numbers, vertex_names, max_id) (v, w, _) ->
	 let update vertex_numbers vertex_names max_id v =
	   if is_int v && not (StringMap.mem v vertex_numbers)
	   then
	     let i = int_of_string v
	     in
	       StringMap.add v i vertex_numbers,
	       IntMap.add vertex_names i v,
	       max max_id i
	   else vertex_numbers, vertex_names, max_id in
	 let vertex_numbers, vertex_names, max_id = update vertex_numbers vertex_names max_id v in
	 let vertex_numbers, vertex_names, max_id = update vertex_numbers vertex_names max_id w in
	   vertex_numbers, vertex_names, max_id)
      (StringMap.empty, IntMap.empty, 1)
      lines in
  let vertex_numbers, vertex_names, max_id =
    List.fold_left
      (fun (vertex_numbers, vertex_names, max_id) (v, w, _) ->
	 let update vertex_numbers vertex_names max_id v =
	   if not (is_int v)
	   then
	     let i = max_id + 1
	     in
	       StringMap.add v i vertex_numbers,
	       IntMap.add vertex_names i v,
	       i
	   else vertex_numbers, vertex_names, max_id in
	 let vertex_numbers, vertex_names, max_id = update vertex_numbers vertex_names max_id v in
	 let vertex_numbers, vertex_names, max_id = update vertex_numbers vertex_names max_id w in
	   vertex_numbers, vertex_names, max_id)
      (vertex_numbers, vertex_names, max_id)
      lines in
  let g = IntMap.fold (fun g i _ -> ELGraph.add_vertex g i) vertex_names ELGraph.empty in
  let g =
    List.fold_left
      (fun g (v, w, s) ->
	 let i = StringMap.find v vertex_numbers in
	 let j = StringMap.find w vertex_numbers in
(* 	   Printf.eprintf "%s %s %d %d\n" v w i j; *)
	   ELGraph.modify_label_default
	     (fun label ->
		if s = "0"
		then { label with eq = label.eq + 1}
		else { label with ne = label.ne + 1})
	     g i j { eq = 0; ne = 0 })
      g
      lines
  in
     g, vertex_numbers, vertex_names
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

exception Not_sign_consistent;;

let color g =
  if ELGraph.is_empty g
  then IntMap.empty
  else
    let rec dfs v color colors =
      match IntMap.get_opt colors v with
	  Some c -> if c = color then colors else raise Not_sign_consistent
	| None ->
	    let colors = IntMap.set colors v color in
	      ELGraph.fold_neighbors
		(fun colors w e ->
		   if e.eq > 0 && e.ne > 0
		   then raise Not_sign_consistent
		   else if e.eq > 0
		   then dfs w color colors
		   else dfs w (not color) colors)
		g v colors
    in
      dfs (ELGraph.max_vertex g) false IntMap.empty
;;

let coloring_cost g colors =
  ELGraph.fold_edges
    (fun k i j { eq = eq; ne = ne } ->
       if IntMap.get colors i = IntMap.get colors j
       then k + ne
       else k + eq)
    g 0
;;

let is_sign_consistent g =
  if ELGraph.is_empty g
  then true
  else
    let rec dfs v color colors =
      if IntMap.has_key colors v
      then IntMap.get colors v = color
      else
	let colors = IntMap.set colors v color in
	  ELGraph.fold_neighbors
	    (fun consistent w e ->
	       consistent
	       && (not (e.eq > 0 && e.ne > 0))
	       && if e.eq > 0
	       then dfs w color colors
	       else dfs w (not color) colors)
	    g v true
    in
      dfs (ELGraph.max_vertex g) false IntMap.empty
;;

let solve_brute_force g =
  if !Util.verbose
  then( Printf.eprintf "bf\tn = %3d m = %4d\n%!" (ELGraph.num_vertices g) (ELGraph.num_edges g);
(* 	Printf.eprintf "V = %a\n" IntSet.output (ELGraph.vertex_set g); *)
      );
  try
    color g
  with Not_sign_consistent ->
    let n = ELGraph.num_vertices g in
      if n >= 30 then failwith "solve_brute_force: too large";
      let numbers, _ = ELGraph.fold_vertices
	(fun (numbers, n) i _ -> IntMap.add numbers i n, n + 1) g (IntMap.empty, 0) in
      let rec loop best_del best_colors colors =
	if colors >= (1 lsl (n - 1))
	then best_del, best_colors
	else
	  let color v = colors land (1 lsl (IntMap.get numbers v)) <> 0 in
	  let del =
	    ELGraph.fold_edges
	      (fun del v w { eq = eq; ne = ne } ->
		 if color v = color w then del + ne else del + eq)
	      g 0
	  in
(* 	    Printf.eprintf "colors = %d del = %d\n%!" colors del; *)
	    if del < best_del
	    then loop del colors (colors + 1)
	    else loop best_del best_colors (colors + 1) in
      let best_del, best_colors = loop max_int 0 0 in
      let colors, _ =
	ELGraph.fold_vertices
	  (fun (colors, n) i _ ->
	     IntMap.add colors i (best_colors land (1 lsl n) <> 0), n + 1)
	  g (IntMap.empty, 0)
      in
	colors
;;

let solve_all_colorings g c =
(*   Printf.eprintf "g = %a\n" output g; *)
  let merge g v1 v2 =
    let g = 
      ELGraph.fold_neighbors
	(fun g j { eq = eq1; ne = un1 } ->
	   ELGraph.modify_label_default
	     (fun { eq = eq2; ne = un2 } -> { eq = eq1 + eq2; ne = un1 + un2 })
	     g v1 j { eq = 0; ne = 0 })
	g v2 g in
      ELGraph.delete_vertex g v2 in
  let c_n = IntSet.size c in
  let g', w = ELGraph.new_vertex g in
  let g', b = ELGraph.new_vertex g' in
  let g' = ELGraph.connect g' b w { eq = 0; ne = 1 lsl 24; } in
  let rec loop colorings colors  =
    if colors >= (1 lsl (c_n - 1))
    then colorings
    else
      let g', _ = IntSet.fold
	(fun (g', i) v ->
	   (if colors land (1 lsl i) <> 0
	    then merge g' w v
	    else merge g' b v), (i + 1))
	c
	(g', 0) in
(*  	Printf.eprintf "sacg' = %a\n" output g'; *)
      let coloring = solve_brute_force g' in
(*  	Printf.eprintf "coloring = %a\n" (IntMap.output Util.output_bool) coloring; *)
      let coloring = IntMap.remove coloring w in
      let coloring = IntMap.remove coloring b in
      let coloring, _ =
	IntSet.fold
	  (fun (coloring, i) v -> IntMap.add coloring v (colors land (1 lsl i) <> 0), i + 1)
	  c (coloring, 0) in
      let colorings = IntMap.add colorings colors coloring
      in				       
	loop colorings (colors + 1)
  in
    loop IntMap.empty 0
;;

let merge_colorings m1 m2 =
  IntMap.fold
    (fun m k v ->
       match IntMap.get_opt m k with
	   Some v' when v' = v -> m
	 | Some _ -> assert false
	 | None -> IntMap.add m k v)
    m1
    m2
;;
let invert_coloring m =
  IntMap.fold (fun m k v -> IntMap.add m k (not v)) m IntMap.empty
;;

let rec solve_cut_corner g =
  if !Util.verbose
  then Printf.eprintf "corn\tn = %3d m = %4d\n%!" (ELGraph.num_vertices g) (ELGraph.num_edges g);
  let s, c = Cut.cut_corner (ELGraph.unlabeled g) in
(*     Printf.eprintf "s = %a c = %a\n%!" IntSet.output s IntSet.output c; *)
    if IntSet.size c > 2
    then solve_brute_force g
    else
      let g' = ELGraph.subgraph g (IntSet.union s c) in
      let colorings = solve_all_colorings g' c in
(*  	Printf.eprintf "colorings = %a\n%!" (IntMap.output (IntMap.output Util.output_bool)) colorings; *)
      let costs = IntMap.map (fun _ coloring -> coloring_cost g' coloring) colorings in
(*  	Printf.eprintf "costs: %a\n%!" (IntMap.output Util.output_int) costs; *)
	let g' = IntSet.fold ELGraph.delete_vertex s g in
	let v, c' = IntSet.pop c in
	let w, _  = IntSet.pop c' in
        (* FIXME remove edge if 0/0 *)
	let g' = ELGraph.set_label g' v w { eq = IntMap.get costs 1; ne = IntMap.get costs 0; } in
(*  	  Printf.eprintf "g' = %a\n" output g'; *)
	let coloring_g = solve_component g' in
 	let coloring_g = 
 	  if IntMap.get coloring_g w = false then coloring_g else invert_coloring coloring_g in
	let coloring_sc =
	  IntMap.get
	    colorings (if IntMap.get coloring_g v = IntMap.get coloring_g w then 0 else 1) in
(*  	  Printf.eprintf "coloring_g = %a\n" (IntMap.output Util.output_bool) coloring_g; *)
(*  	  Printf.eprintf "coloring_sc = %a\n%!" (IntMap.output Util.output_bool) coloring_sc; *)
	  merge_colorings coloring_g coloring_sc

and solve_component g =
  if !Util.verbose
  then Printf.eprintf "comp\tn = %3d m = %4d\n%!" (ELGraph.num_vertices g) (ELGraph.num_edges g);
  if ELGraph.num_vertices g <= 5
  then solve_brute_force g
  else solve_cut_corner g
;;

let solve g =
  if !Util.verbose
  then Printf.eprintf "solve\tn = %3d m = %4d\n%!" (ELGraph.num_vertices g) (ELGraph.num_edges g);
  let components = Cut.biconnected_components (ELGraph.unlabeled g) in
    List.fold_left
      (fun colors component ->
	 let map_intersection m1 m2 =
	   IntMap.fold
	     (fun s k _ -> if IntMap.has_key m2 k then IntSet.add s k else s)
	     m1
	     IntSet.empty in
	 let colors' = solve_component (ELGraph.subgraph g component) in
(* 	   Printf.eprintf "colors  = %a\n" (IntMap.output Util.output_bool) colors; *)
(* 	   Printf.eprintf "colors' = %a\n" (IntMap.output Util.output_bool) colors'; *)
	 let cut = map_intersection colors colors' in
	   if IntSet.is_empty cut
	   then merge_colorings colors colors'
	   else
	     let v = IntSet.choose cut in
(* 	       Printf.eprintf "v = %d c1 = %b c2 = %b\n" v (IntMap.get colors v) (IntMap.get colors' v); *)
	       if IntMap.get colors v = IntMap.get colors' v
	       then merge_colorings colors colors'
	       else merge_colorings colors (invert_coloring colors'))
      IntMap.empty
      components
;;
