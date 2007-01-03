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
  then Printf.eprintf "bf\tn = %3d m = %4d\n%!" (ELGraph.num_vertices g) (ELGraph.num_edges g);
  if is_sign_consistent g
  then begin
    if !Util.verbose
    then Printf.eprintf "already sign-consistent\n%!";
    []
  end else
    let n = ELGraph.num_vertices g in
      if n >= 30 then failwith "solve_brute_force: too large";
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
	best_edges
;;

let solve_component g =
  if !Util.verbose
  then Printf.eprintf "comp\tn = %3d m = %4d\n%!" (ELGraph.num_vertices g) (ELGraph.num_edges g);
  solve_brute_force g
;;

let solve g =
  if !Util.verbose
  then Printf.eprintf "solve\tn = %3d m = %4d\n%!" (ELGraph.num_vertices g) (ELGraph.num_edges g);
  let components = Cut.biconnected_components (ELGraph.unlabeled g) in
    List.fold_left
      (fun edges component -> edges @ (solve_component (ELGraph.subgraph g component)))
      []
      components
;;
