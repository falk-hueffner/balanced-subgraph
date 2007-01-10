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

open Ulp;;				(* for the record field labels *)

let star_cover g =
  let rec deg1 g =
    ELGraph.fold_vertices
      (fun r i neigh ->
	 if ELGraph.deg g i = 1
	 then Some (let j, _ = IntMap.choose neigh in j)
	 else r) g None in
  let rec maxdeg g =
    let r, _ =
      ELGraph.fold_vertices
	(fun (r, d) i _ ->
	   let d' = ELGraph.deg g i in if d' > d then Some i, d'  else r, d) g (None, 0)
    in
      r in
  let rec loop g vs =
    match
      let r = deg1 g in if r = None then maxdeg g else r
    with
	Some i -> loop (ELGraph.delete_vertex g i) (IntSet.add vs i)
      | None -> vs
  in
    loop g IntSet.empty
;;

let gray_code x = x lxor (x lsr 1);;
let rec ctz x = if x land 1 = 1 then 0 else 1 + (ctz (x lsr 1));;
let gray_change x = ctz ((gray_code x) lxor (gray_code (x + 1)));;

external c_find_cut_partition :
  (int * int) array array -> int array -> int array -> int -> (int * int) list
  = "c_find_cut_partition" "c_find_cut_partition";;

let flow_to_array g =
  let n = ELGraph.max_vertex g in
  let a = Array.make (n + 1) [| |] in
    for i = 0 to n do
      if not (ELGraph.has_vertex g i)
      then a.(i) <- [| |]
      else begin
	a.(i) <- Array.make (ELGraph.deg g i) (0, 0);
	ignore (ELGraph.fold_neighbors
		  (fun j w l -> a.(i).(j) <- (w, l.Flow.cap); j + 1) g i 0);
      end
    done;
    a
;;

let solve_iterative_compression g =
  let d = false in
  if !Util.verbose
  then Printf.eprintf "itco\tn = %3d m = %4d\n%!" (ELGraph.num_vertices g) (ELGraph.num_edges g);
  let g = ELGraph.fold_vertices (fun g i _ -> ELGraph.unconnect g i i) g g in
  let g = ELGraph.fold_edges
    (fun g i j l ->
       if l.ne > 0 && l.eq > 0 then
	 if l.ne > l.eq
	 then ELGraph.set_label g i j {ne = l.ne - l.eq; eq = 0}
	 else if l.eq > l.ne
	 then ELGraph.set_label g i j {ne = 0; eq = l.eq - l.ne}
	 else ELGraph.disconnect g i j
       else g) g g in
  let compress g cover =
    let flow = ELGraph.fold_edges
      (fun flow i j {eq = eq; ne = ne} ->
	 Flow.connect flow i j (eq + ne)) g Flow.empty in
    let cover_g =
      List.fold_left
	(fun g (i, j) -> ELGraph.set_connect g i j {eq = 1; ne = 0})
	ELGraph.empty cover in
    let s = star_cover cover_g in
    let flow, t, s_of_t, t_of_s, pairs, _ = IntSet.fold
      (fun (flow, t, s_of_t, t_of_s, pairs, k) i ->
	 let flow, j = Flow.new_vertex flow in
	 let t = IntSet.add t j in
	 let s_of_t = IntMap.add s_of_t j i in
	 let t_of_s = IntMap.add t_of_s i j in
	 let pairs = IntMap.add pairs k (i, j) in
	   flow, t, s_of_t, t_of_s, pairs, k + 1)
      s (flow, IntSet.empty, IntMap.empty, IntMap.empty, IntMap.empty, 0) in
    let flow =
      List.fold_left
	(fun flow (i, j) ->
	   let i, j = if IntSet.contains s i then i, j else j, i in
	   let cap = Flow.capacity flow i j in
	   let flow = Flow.disconnect flow i j in
	   let j' = IntMap.get t_of_s i in
	     Flow.connect flow j j' cap) flow cover in
    let num_pairs = IntSet.size s in
    let k =
      List.fold_left
	(fun k (i, j) -> let l = ELGraph.get_label g i j in k + l.eq + l.ne) 0 cover in
      if !Util.verbose then Printf.eprintf "m = %d k = %d, star_cover = %d, cover = %a\n%!"
	(ELGraph.num_edges g) k (IntSet.size s) (Util.output_list (fun c (i, j) -> Printf.fprintf c "(%d, %d)" i j)) cover;
    if d then Printf.eprintf "\ncompress g = %a cover = %a k = %d%!" output g (Util.output_list (fun c (i, j) -> Printf.fprintf c "(%d, %d)%!" i j)) cover k;
    if d then Printf.eprintf " flow = %a\n%!" Flow.output flow;
    let rec loop iter s t pairs =
      if d then Printf.eprintf "iter = %d s = %a t = %a\n%!" iter IntSet.output s IntSet.output t;      
      let flow, augmented =
	Util.fold_n (fun (flow, _) _ ->
		       let flow, augmented =
			 Flow.augment flow s t
		       in
			 if d then Printf.eprintf "flow = %a\n%!" Flow.output flow; flow, augmented
		    ) k (flow, false)
      in
	if augmented then
	  if (iter + 1) >= (1 lsl num_pairs) then (if d then Printf.eprintf "not compressed\n"; g, cover) else
	    let a = gray_change iter in
	    let i, j = IntMap.get pairs a in
	      if d then Printf.eprintf "i = %d j = %d\n" i j;
	      let s = IntSet.delete s i in
	      let t = IntSet.delete t j in
	      let s = IntSet.add s j in
	      let t = IntSet.add t i in
	      let pairs = IntMap.update pairs a (j, i) in
		loop (iter + 1) s t pairs
	else
	  let cut = Flow.cut flow s in
	  let cut =
	    List.map
	      (fun (i, j) ->
		 let i = IntMap.get_default s_of_t i i in
		 let j = IntMap.get_default s_of_t j j in
	           i, j)
	      cut 
	  in
	    if d then Printf.eprintf "compressed to %a\n" (Util.output_list (fun c (i, j) -> Printf.fprintf c "(%d, %d)" i j)) cut;
	    g, cut
    in
(*       loop 0 s t pairs in *)
    let cover' =
      c_find_cut_partition (flow_to_array flow) (IntSet.to_array s) (IntSet.to_array t) k in
    let cover = if cover' = [] then cover else cover' in
    let cover =
      List.map
	(fun (i, j) ->
	   let i = IntMap.get_default s_of_t i i in
	   let j = IntMap.get_default s_of_t j j in
	     i, j)
	cover in
      g, cover in
  let _, cover =    
    ELGraph.fold_edges
      (fun (g, cover) i j l ->
	 let g = ELGraph.set_vertex g i in
	 let g = ELGraph.set_vertex g j in
	 let deq = if l.eq > 0 then 1 else 0 in
	 let dne = if l.ne > 0 then 1 else 0 in
	   Util.fold_n
	     (fun (g, cover) _ ->
		let g = ELGraph.modify_label_default
		  (fun {eq = eq; ne = ne} -> {eq = eq + deq; ne = ne + dne})
		  g i j {eq = 0; ne = 0}
		in
		  if not (Util.list_contains cover (i, j))
		    && (is_sign_consistent
			  (List.fold_left (fun g (i, j) -> ELGraph.disconnect g i j) g cover))
		  then g, cover
		  else
		    let cover =
		      if  not (Util.list_contains cover (i, j))
		      then (i, j) :: cover
		      else cover
		    in
		      compress g cover)
	     (max l.eq l.ne)
	     (g, cover))
      g
      (ELGraph.empty, []) in
    if d then Printf.eprintf "coloring...\n%!";
  let g' = List.fold_left (fun g' (i, j) -> if d then Printf.eprintf "unc %d %d\n%!" i j; ELGraph.unconnect g' i j) g cover in
    color g'
;;

let solve_occ g =
  if !Util.verbose
  then Printf.eprintf "occ\tn = %3d m = %4d\n%!" (ELGraph.num_vertices g) (ELGraph.num_edges g);
(*   output stderr g; *)
  let occ_out, occ_in = Unix.open_process "occ -e" in
  let gadget_edges, _ =
    ELGraph.fold_edges
      (fun (gadget_edges, v) i j { eq = eq; ne = ne } ->
	 if i = j then gadget_edges, v else
	 let ne =
	   if ne > 0
	   then (Printf.fprintf occ_in "%d %d\n" i j;
(*   		 Printf.eprintf        "%d %d\n" i j; *)
		 ne - 1)
	   else ne in
	 let gadget_edges, v =
	   Util.fold_n
	     (fun (gadget_edges, v) _ ->
		Printf.fprintf occ_in "%d %d\n%d %d\n%d %d\n" i v  v (v+1)  (v+1) j;
(*   		Printf.eprintf        "%d %d\n%d %d\n%d %d\n" i v  v (v+1)  (v+1) j; *)
		let gadget_edges = IntMap.add gadget_edges v     (i, j) in
		let gadget_edges = IntMap.add gadget_edges (v+1) (i, j) in
		  gadget_edges, v+2)
	     ne
	     (gadget_edges, v) in
	 let gadget_edges, v =
	   Util.fold_n
	     (fun (gadget_edges, v) _ ->
		Printf.fprintf occ_in "%d %d\n%d %d\n" i v  v j;
(*   		Printf.eprintf        "%d %d\n%d %d\n" i v  v j; *)
		let gadget_edges = IntMap.add gadget_edges v     (i, j) in
		  gadget_edges, v+1)
	     eq
	     (gadget_edges, v)
	 in
	   gadget_edges, v)
      g
      (IntMap.empty, (ELGraph.max_vertex g) + 1)
  in
    close_out occ_in;
    let rec loop edges =
      try
	let line = input_line occ_out in
(*   	  Printf.eprintf "line = '%s'\n%!" line; *)
	match Util.split_string line with
	    [ v; w ] ->
	      let i = int_of_string v and j = int_of_string w in
	      let i, j =
		if not (ELGraph.has_vertex g i) then IntMap.get gadget_edges i
		else if not (ELGraph.has_vertex g j) then IntMap.get gadget_edges j
		else i, j
	      in
(*  		Printf.eprintf "is = %d %d\n%!" i j; *)
		loop ((i, j) :: edges)
	  | _ -> assert false
      with End_of_file -> edges in
    let edges = loop [] in
    let g' = List.fold_left (fun g' (i, j) -> ELGraph.unconnect g' i j) g edges in
    let g' = ELGraph.fold_vertices (fun g' i _ -> ELGraph.unconnect g' i i) g' g' in
      color g'
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
      if n >= 8 then solve_iterative_compression g else
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

let solve_all_colorings g c =
  let d = false in
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
  let g' = ELGraph.connect g' b w { eq = 0; ne = 1 lsl 6; } in (* FIXME *)
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
      if d then Printf.eprintf "sacg' = %a\n" output g';
      let coloring = solve_brute_force g' in
      if d then Printf.eprintf "coloring = %a\n" (IntMap.output Util.output_bool) coloring;
      let coloring = if IntMap.get coloring w then coloring else invert_coloring coloring in
      let coloring = IntMap.delete coloring w in
      let coloring = IntMap.delete coloring b in
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

let find_lincomb v vs =
  if !Util.verbose
  then Printf.eprintf "linco\tv = %a\n%!" (Util.output_array Util.output_int) v;
  let db = false in
  if db then Printf.eprintf "find_lincomb\n%!";
  let normalize v =
    let m = Array.fold_left min max_int v in
      Array.map (fun i -> i - m) v in
  let is_zero v =
    let rec loop i =
      if i >= Array.length v
      then true
      else v.(i) = 0 && loop (i + 1)
    in
      loop 0 in
  let can_apply v d =
    let rec loop i =
      if i >= Array.length v
      then true
      else v.(i) - d.(i) >= 0 && loop (i + 1)
    in
      loop 0 in
  let apply v d =
    Array.mapi (fun i x -> x - d.(i)) v in
    
  let rec loop v vs' max_cost =
    if is_zero v
    then Some (0, [], [])
    else
      match vs' with
	  [] -> None
	| (cost, d, edges) :: vs' ->
	    if cost <= max_cost && can_apply v d
	    then let v' = apply v d in
	      match loop v' vs (max_cost - cost) with
		  None -> loop v vs' max_cost
		| Some (cost', ds, gadgets) -> Some (cost + cost', d :: ds, edges :: gadgets)
	    else
	      loop v vs' max_cost in
  let v = normalize v in
  let rec trial v max_cost max_max_cost =
    if db then Printf.eprintf "trial max_cost = %d max_max_cost = %d\n%!" max_cost max_max_cost;
    if max_cost > max_max_cost
    then None
      else
	match loop v vs max_cost with
	    None -> trial v (max_cost + 1) max_max_cost
	  | something -> something in
  let rec shift d =
    if d >= 3 then None
    else begin
      if db then Printf.eprintf "shift %d: %a\n%!" d (Util.output_array Util.output_int) v;
      let v = Array.map ((+) d) v in
      let max_max_cost = ((Array.fold_left (+) 0 v) + 1) / 2 in (* every vec. has at least 2 1s *)
	match trial v 0 max_max_cost with
	    None -> shift (d + 1)
	  | Some (cost, ds, gadgets) -> Some (cost + d, ds, gadgets)
    end
  in
    shift 0
;;

let make_cut_gadget g c costs gadgets =
  let d = false in
  if d then Printf.eprintf "make_cut_gadget costs = %a\n" (IntMap.output Util.output_int) costs;
  let costs = Array.init (IntMap.size costs) (fun i -> IntMap.get costs i) in
  let apply_gadget g edges =
    let vs, _ = IntSet.fold (fun (vs, n) ci -> IntMap.add vs n ci, n + 1) c (IntMap.empty, 0) in
    let g, _ = 
      List.fold_left
	(fun (g, vs) (i, j, l) ->
	   let add g vs v =
	     if IntMap.has_key vs v
	     then g, vs
	     else
	       let g, v' = ELGraph.new_vertex g in
		 g, (IntMap.add vs v v') in
	   let g, vs = add g vs i in
	   let g, vs = add g vs j in
	   let i = IntMap.get vs i in
	   let j = IntMap.get vs j in
	   let g = ELGraph.modify_label_default
	     (fun {eq = eq; ne = ne} -> {eq = eq + l.eq; ne = ne + l.ne})
	     g i j {eq = 0; ne = 0}		   
	   in		   
	     g, vs)
	(g, vs)
	edges
    in
      g
  in
    match find_lincomb costs gadgets with
	None -> assert false
      | Some (cost, costvecs, gadgets) ->
	  if d then Printf.eprintf " cost = %d\n" cost;
	  List.fold_left apply_gadget g gadgets
;;

let rec solve_cut_corner g =
  let d = false in
  if !Util.verbose
  then Printf.eprintf "corn\tn = %3d m = %4d\n%!" (ELGraph.num_vertices g) (ELGraph.num_edges g);
    if d then Printf.eprintf " g = %a\n%!" output g;
  let deg2 =
    ELGraph.fold_vertices
      (fun deg2 i n ->
	 if IntMap.size n = 2
	 then Some (IntSet.singleton i, IntMap.fold (fun n i _ -> IntSet.add n i) n IntSet.empty)
	 else deg2)
      g None in
  let scs =
    match deg2 with
	Some (s, c) -> [s, c]
      | None -> Cut.cut_corner (ELGraph.unlabeled g) in
  let scs = List.sort
    (fun (s1, c1) (s2, c2) ->
       if IntSet.size c1 <> IntSet.size c2
       then compare (IntSet.size c1) (IntSet.size c2)
       else compare (IntSet.size s2) (IntSet.size s1)) scs in
  let rec loop = function
      [] -> solve_brute_force g
    | (s, c) :: rest ->
	if !Util.verbose then Printf.eprintf " s = %a c = %a\n%!" IntSet.output s IntSet.output c;
	let sc = ELGraph.subgraph g (IntSet.union s c) in
(* 	if d then Printf.eprintf "g = %a" output g; *)
(* 	if d then Printf.eprintf " sc = %a" output sc; *)
	let colorings = solve_all_colorings sc c in
	let costs = IntMap.map (fun _ coloring -> coloring_cost sc coloring) colorings in
	if d then Printf.eprintf " costs: %a\n%!" (IntMap.output Util.output_int) costs;
	let rc = IntSet.fold ELGraph.delete_vertex s g in
	let rc =
	  ELGraph.fold_edges
	    (fun rc i j _ ->
	       if IntSet.contains c i && IntSet.contains c j
	       then ELGraph.disconnect rc i j else rc) g rc in
	let rc' =
	  make_cut_gadget rc c costs Gadgets.gadgets.(IntSet.size c) in
	if not (ELGraph.num_vertices rc' < ELGraph.num_vertices g
	        || (ELGraph.num_vertices rc' = ELGraph.num_vertices g
	            && Ulp.num_edges rc' < Ulp.num_edges g))
	then ( if d then Printf.eprintf "gadget failed\n";
(* 	       Printf.eprintf "rc  = %a\n" output rc; *)
(* 	       Printf.eprintf "rc' = %a\n" output rc'; *)
	       loop rest )
	else
	  let coloring = solve rc' in
	  let coloring = 
 	    if IntMap.get coloring (IntSet.max c) = false
	    then coloring else invert_coloring coloring in
	  let coloring = IntMap.filter (fun i _ -> ELGraph.has_vertex rc i) coloring in
	  if d then Printf.eprintf "coloring: %a\n%!" (IntMap.output Util.output_bool) coloring;
	  let code, _ =
	    IntSet.fold
	      (fun (code, n) v ->
	         if IntMap.get coloring v
	         then code lor (1 lsl n), n+1 else code, n+1) c (0, 0) in
	  let coloring_sc = IntMap.get colorings code in
	    merge_colorings coloring coloring_sc
  in
    loop scs

and solve_component g =
  let g = ELGraph.fold_edges
    (fun g i j {eq = eq; ne = ne} ->
       if eq > 0 && ne > 0
       then
	 if eq = ne
	 then ELGraph.disconnect g i j
	 else
	   let m = min eq ne in ELGraph.set_label g i j {eq = eq - m; ne = ne - m}
       else g) g g in
  if !Util.verbose
  then Printf.eprintf "comp\tn = %3d m = %4d\n%!" (ELGraph.num_vertices g) (ELGraph.num_edges g);
  if ELGraph.num_vertices g <= 5
  then solve_brute_force g
  else solve_cut_corner g

and solve g =
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
