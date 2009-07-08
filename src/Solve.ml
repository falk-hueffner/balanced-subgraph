(* bsg -- solve the balanced subgraph problem
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

open Bsg;;				(* for the record field labels *)

let vertex_cover g =
  let rec vertex_cover' g max_vc =
    let rec loop g vc m md todo =
      if IntSet.is_empty todo
      then g, vc, m, md
      else
	let i, todo = IntSet.pop todo in
	let d = ELGraph.deg g i in
	let g = if d = 0 then ELGraph.delete_vertex g i else g in
	let g, vc, todo =
	  if d = 1 then
	    let j = IntMap.min_key (ELGraph.neighbors g i) in
	    let g = ELGraph.delete_vertex g i in
	    let todo = ELGraph.fold_neighbors (fun todo k _ -> IntSet.put todo k) g j todo in
	    let g = ELGraph.delete_vertex g j in
	    let todo = IntSet.remove todo j in
	    let vc = IntSet.add vc j in
	      g, vc, todo
	  else g, vc, todo in
	let m, md = if d >= 2 && d > m then i, d else m, md in
	  loop g vc m md todo in
    let g, vc, m, md = loop g IntSet.empty (-1) 0 (ELGraph.vertex_set g) in
    let m, md =
      ELGraph.fold_vertices
	(fun (m, md) i n_i ->
	   let d = IntMap.size n_i in
	     if d > m then i, d else m, md)
	g (-1, 0)
    in
      if md = 0 then Some vc
      else
	match
	  let g' = ELGraph.delete_vertex g m in
	  let vc1 = vertex_cover' g' (max_vc - 1) in
	  let g' = ELGraph.fold_neighbors (fun g' i _ -> ELGraph.delete_vertex g' i) g m g' in
	  let max_vc' = match vc1 with None -> max_vc | Some vc -> IntSet.size vc - 1 in
	  let vc2 = vertex_cover' g' (max_vc' - md) in
	    match vc1, vc2 with
		None, None -> failwith "vertex_cover"
	      | Some vs1, None -> vc1
	      | None, Some vs2 -> vc2
	      | Some vs1, Some vs2 ->
		  if IntSet.size vs1 + 1 <= IntSet.size vs2 + md
		  then Some (IntSet.add vs1 m)
		  else Some (ELGraph.fold_neighbors (fun vs2 i _ -> IntSet.add vs2 i) g m vs2)
	with
	    None -> None
	  | Some vc' -> Some (IntSet.union vc vc')
  in
    match vertex_cover' g max_int with
	Some vs -> vs
      | None -> failwith "vertex_cover"
;;

let gray_code x = x lxor (x lsr 1);;
let rec ctz x = if x land 1 = 1 then 0 else 1 + (ctz (x lsr 1));;
let gray_change x = ctz ((gray_code x) lxor (gray_code (x + 1)));;

external c_find_cut_partition :
  (int * int) array array -> int array -> int array -> int -> (int * int) list
  = "c_find_cut_partition" "c_find_cut_partition";;

let to_array g =
  let n = ELGraph.max_vertex g in
  let a = Array.make (n + 1) [| |] in
    for i = 0 to n do
      if not (ELGraph.has_vertex g i)
      then a.(i) <- [| |]
      else begin
	a.(i) <- Array.make (ELGraph.deg g i) (0, 0);
	ignore (ELGraph.fold_neighbors (fun j w l -> a.(i).(j) <- (w, l.eq + l.ne); j + 1) g i 0);
      end
    done;
    a
;;

let solve_iterative_compression g =
  if !Util.verbose
  then Printf.eprintf "iterative compression\tn = %3d m = %4d (%4d)\n%!"
    (ELGraph.num_vertices g) (ELGraph.num_edges g) (Bsg.num_edges g);
  let m0 = Bsg.num_edges g in
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
    let cover_g =
      List.fold_left
	(fun g (i, j) -> ELGraph.set_connect g i j {eq = 1; ne = 0})
	ELGraph.empty cover in
    let s = vertex_cover cover_g in
    let g', t, s_of_t, t_of_s, pairs, _ = IntSet.fold
      (fun (g', t, s_of_t, t_of_s, pairs, k) i ->
	 let g', j = ELGraph.new_vertex g' in
	 let t = IntSet.add t j in
	 let s_of_t = IntMap.add s_of_t j i in
	 let t_of_s = IntMap.add t_of_s i j in
	 let pairs = IntMap.add pairs k (i, j) in
	   g', t, s_of_t, t_of_s, pairs, k + 1)
      s (g, IntSet.empty, IntMap.empty, IntMap.empty, IntMap.empty, 0) in
    let g' =
      List.fold_left
	(fun g' (i, j) ->
	   let i, j = if IntSet.contains s i then i, j else j, i in
	   let l = ELGraph.get_label g i j in
	   let g' = ELGraph.disconnect g' i j in
	   let j' = IntMap.get t_of_s i in
	     ELGraph.connect g' j j' l) g' cover in
    let k =
      List.fold_left
	(fun k (i, j) -> let l = ELGraph.get_label g i j in k + l.eq + l.ne) 0 cover in
    if !Util.verbose then Printf.eprintf " m = %d/%d k = %d vc = %d cover = %d\n%!"
      (Bsg.num_edges g) m0 k (IntSet.size s) (List.length cover);
    let cover' =
      c_find_cut_partition (to_array g') (IntSet.to_array s) (IntSet.to_array t) k in
    let cover = if cover' = [] then cover else cover' in
    let cover =
      List.map
	(fun (i, j) ->
	   let i = IntMap.get_default s_of_t i i in
	   let j = IntMap.get_default s_of_t j j in
	     i, j)
	cover in
      cover in
  let cover =
    if !Util.downward_compress
    then
      let cover = Bsg.cover g (Bsg.heuristic g) in
      let rec loop cover =
	let cover' = compress g cover in
	  if cover' <> cover then loop cover' else cover
      in
	loop cover    
    else
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
			  g, compress g cover)
		 (max l.eq l.ne)
		 (g, cover))
	  g
	  (ELGraph.empty, [])
      in
	cover in
  let g' = List.fold_left (fun g' (i, j) -> ELGraph.unconnect g' i j) g cover in
    color g'
;;

let solve_brute_force g =
  if !Util.verbose
  then( Printf.eprintf "brute force\tn = %3d m = %4d (%4d)\n%!"
    (ELGraph.num_vertices g) (ELGraph.num_edges g) (Bsg.num_edges g);
      );
  try
    color g
  with Not_sign_consistent ->
    let n = ELGraph.num_vertices g in
      Util.max_unreducible_size := max !Util.max_unreducible_size n;
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

let array_for_all p a =
  let rec loop i =
    i >= Array.length a || (p a.(i) && loop (i + 1))
  in
    loop 0
;;

let array_is_zero = array_for_all (fun x -> x = 0);;

let can_apply v d =  
  let rec loop i =
    i >= Array.length v || (v.(i) - d.(i) >= 0 && loop (i + 1))
  in
    loop 0
;;

let normalize v =
  let m = Array.fold_left min max_int v in
    Array.map (fun i -> i - m) v
;;

let array_max_i v =
  let vmax = ref v.(0) and imax = ref 0 in
    for i = 1 to Array.length v - 1 do
      if v.(i) > !vmax then (imax := i; vmax := v.(i))
    done;
    !imax
;;

let array_for_all p a =
  let rec loop i =
    i >= Array.length a || (p a.(i) && loop (i + 1))
  in
    loop 0
;;

let is_zero = array_for_all (fun x -> x = 0);;
let apply v d = Array.mapi (fun i x -> x - d.(i)) v;;
let parity v = (Array.fold_left (lxor) 0 v) land 1;;
let zeros v = Array.fold_left (fun zeros x -> if x = 0 then zeros + 1 else zeros) 0 v;;

let rec find_lincomb4 v gadgets =
  let v = normalize v in
  let v, chosen =
    if parity v = 0 then v, [] else
      let v = if zeros v <= 1 then v else Array.map ((+) 1) v in
      let rec loop = function
	  [] -> failwith "find_lincomb4"
	| (cost, d, edges) :: gadgets ->
	    if parity d <> 1 || not (can_apply v d) then loop gadgets
	    else d, edges in
      let d, edges = loop gadgets in
      let v = apply v d in
	v, [edges] in
  let rec loop v chosen =
    if is_zero v
    then chosen
    else
      let v = if zeros v < 3 then v else Array.map ((+) 1) v in
      let max_i = array_max_i v in
      let rec loop2 = function
	  [] -> failwith "find_lincomb4"
	| (cost, d, edges) :: gadgets ->
	    if parity d <> 0 || d.(max_i) = 0 || not (can_apply v d) then loop2 gadgets
	    else d, edges in
      let d, edges = loop2 gadgets in
      let v = apply v d in
      let chosen = edges :: chosen in
	loop v chosen
  in
     Some (loop v chosen)
;;

let find_lincomb v vs = 
  if !Util.verbose
  then Printf.eprintf "linear combination\tv = %a\n%!" (Util.output_array Util.output_int) v;
  if Array.length v = 4 then find_lincomb4 v vs else
  let rec loop v vs' max_cost =
    if array_is_zero v
    then Some []
    else
      match vs' with
	  [] -> None
	| (cost, d, edges) :: vs' ->
	    if cost <= max_cost && can_apply v d
	    then let v' = apply v d in
	      match loop v' vs (max_cost - cost) with
		  None -> loop v vs' max_cost
		| Some gadgets -> Some (edges :: gadgets)
	    else
	      loop v vs' max_cost in
  let v = normalize v in
  let rec trial v max_cost max_max_cost =
    if max_cost > max_max_cost
    then None
      else
	match loop v vs max_cost with
	    None -> trial v (max_cost + 1) max_max_cost
	  | gadgets -> gadgets in
  let min_gadget_sum =
    List.fold_left
      (fun min_gadget_sum (_, v, _) ->
	 min min_gadget_sum (Array.fold_left (+) 0 v)) max_int vs in
  let rec shift d =
    let v = Array.map ((+) d) v in
      if !Util.verbose
      then Printf.eprintf " shift %d: %a\n%!" d (Util.output_array Util.output_int) v;
      let s = Array.fold_left (+) 0 v in
	if s >= 24
	then None else
	  let max_max_cost = ((Array.fold_left (+) 0 v) + (min_gadget_sum)) / min_gadget_sum in
	    match trial v 0 max_max_cost with
		None -> shift (d + 1)
	      | gadgets -> gadgets in
    shift 0
;;

let make_cut_gadget g c costs gadgets =
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
	None -> None
      | Some gadgets -> Some (List.fold_left apply_gadget g gadgets)
;;

(* merge w into v *)
let merge_vertices g v w =
  let invert ({eq = eq; ne = ne} as label) =
    if Bsg.is_negative g v w then {eq = ne; ne = eq} else label in
  let label_plus {eq = eq1; ne = ne1} {eq = eq2; ne = ne2} =
    let eq = eq1 + eq2 and ne = ne1 + ne2 in
    let m = min eq ne in
      {eq = eq - m; ne = ne - m} in
    (*     {eq = eq1 + eq2; ne = ne1 + ne2} in *)
  let nw = ELGraph.neighbors g w in
  let g = ELGraph.delete_vertex g w in
    IntMap.fold
      (fun g x wxlabel ->
	 let label = ELGraph.get_label_default g v x {eq = 0; ne = 0} in
	 let label = label_plus label (invert wxlabel) in
	   if label.eq = 0 && label.ne = 0
	   then ELGraph.unconnect g v x
	   else ELGraph.set_label g v x label)
      nw g
;;

let unreducible_sc = Hashtbl.create 31;;

let rec solve_all_colorings g c =
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
  let g' = ELGraph.connect g' b w { eq = 0; ne = Bsg.num_edges g; } in
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
      let coloring = solve g' in
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

and solve_cut_corner g =
  if !Util.verbose
  then Printf.eprintf "cut corner\tn = %3d m = %4d (%4d)\n%!"
    (ELGraph.num_vertices g) (ELGraph.num_edges g) (Bsg.num_edges g);
  if !Util.max_cut_size < 2 || ELGraph.num_vertices g <= 10 then solve_heavy_edge g else
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
      [] -> solve_heavy_edge g
    | (s, c) :: rest ->
	if !Util.verbose then Printf.eprintf " |C| = %d |S| = %d\n%!"
	  (IntSet.size c) (IntSet.size s);
	let sc = ELGraph.subgraph g (IntSet.union s c) in
	if Hashtbl.mem unreducible_sc sc then loop rest else
	let () = () in
	let colorings = solve_all_colorings sc c in
	let costs = IntMap.map (fun _ coloring -> coloring_cost sc coloring) colorings in
	let rc = IntSet.fold ELGraph.delete_vertex s g in
	let rc =
	  ELGraph.fold_edges
	    (fun rc i j _ ->
	       if IntSet.contains c i && IntSet.contains c j
	       then ELGraph.disconnect rc i j else rc) g rc in
	match make_cut_gadget rc c costs Gadgets.gadgets.(IntSet.size c) with
	    None -> (if !Util.verbose
		     then Printf.eprintf " failed to find good linear combination\n%!";
		     Hashtbl.add unreducible_sc sc ();
		     loop rest)
	  | Some rc' ->
	if not (ELGraph.num_vertices rc' < ELGraph.num_vertices g
	        || (ELGraph.num_vertices rc' = ELGraph.num_vertices g
	            && Bsg.num_edges rc' < Bsg.num_edges g))
	then begin
	  if !Util.verbose then Printf.eprintf " failed to reduce\n%!";
	  Hashtbl.add unreducible_sc sc ();
	  loop rest
	end else
	  let coloring = solve rc' in
	  let coloring = 
 	    if IntMap.get coloring (IntSet.max c) = false
	    then coloring else invert_coloring coloring in
	  let coloring = IntMap.filter (fun i _ -> ELGraph.has_vertex rc i) coloring in
	  let code, _ =
	    IntSet.fold
	      (fun (code, n) v ->
	         if IntMap.get coloring v
	         then code lor (1 lsl n), n+1 else code, n+1) c (0, 0) in
	  let coloring_sc = IntMap.get colorings code in
	    merge_colorings coloring coloring_sc
  in
    loop scs

and solve_heavy_edge g =
  if 1 = 0 then solve_brute_force g else
  if ELGraph.num_vertices g <= 5 then solve_brute_force g else let () = () in
  if !Util.verbose
  then Printf.eprintf "solve_heavy_edge\tn = %3d m = %4d (%4d)\n%!"
    (ELGraph.num_vertices g) (ELGraph.num_edges g) (Bsg.num_edges g);
(*     Printf.eprintf "  initial: %a\n" Bsg.output g; *)
  let to_check = ELGraph.vertex_set g in
  let same x = x in
  let rec loop g to_check merged =
    if IntSet.is_empty to_check then g, merged
    else
      let i, to_check = IntSet.pop to_check in
      let maxj, maxw, sumw =
	IntMap.fold
	  (fun (maxj, maxw, sumw) j {eq = eq; ne = ne} ->
	     let w = eq + ne - (min eq ne) in
	     let sumw = sumw + w
	     in
	       if w >= maxw then j, w, sumw
	       else maxj, maxw, sumw)
	  (ELGraph.neighbors g i) (-1, -1, 0)
      in
	if maxw >= sumw - maxw then
	  (if !Util.verbose
	   then Printf.eprintf " merge %d %d (w = %d sum = %d)\n" i maxj maxw sumw;
(* 	     Printf.eprintf "  before: %a\n" Bsg.output g; *)
	   let op = if Bsg.is_negative g i maxj then not else same in
	   let g = merge_vertices g i maxj in
	   let g = ELGraph.disconnect g i i in
(* 	     Printf.eprintf "  merged: %a\n" Bsg.output g; *)
	   let merged = IntMap.add merged maxj (i, op) in
	   let to_check = IntSet.remove to_check maxj in
	   let to_check =
	     ELGraph.fold_neighbors
	       (fun to_check j _ -> IntSet.put to_check j) g i to_check
	   in
(* 	     IntMap.output Util.output_int stderr merged; prerr_newline (); *)
	     loop g to_check merged)
	else loop g to_check merged in
  let g, merged = loop g to_check IntMap.empty in
  let rec lookup coloring v =
    match IntMap.get_opt merged v with
	None -> IntMap.get coloring v
      | Some (w, op) -> op (lookup coloring w)
  in
    if IntMap.is_empty merged
    then solve_brute_force g
    else
      (* restart, might have double edges, self loops, or zero weight edges  *)
      let coloring = solve g in
	IntMap.fold
	  (fun coloring v _ -> IntMap.add coloring v (lookup coloring v))
	  merged coloring

and solve_biconnected g =
  if !Util.verbose
  then Printf.eprintf "solve_biconn\tn = %3d m = %4d (%4d)\n%!"
    (ELGraph.num_vertices g) (ELGraph.num_edges g) (Bsg.num_edges g);
  if !Util.max_cut_size < 1 || ELGraph.num_vertices g <= 5
  then solve_brute_force g
  else
    let components = Cut.biconnected_components (ELGraph.unlabeled g) in
      List.fold_left
	(fun colors component ->
	   let map_intersection m1 m2 =
	     IntMap.fold
	       (fun s k _ -> if IntMap.has_key m2 k then IntSet.add s k else s)
	       m1
	       IntSet.empty in
	   let colors' = solve_cut_corner (ELGraph.subgraph g component) in
	   let cut = map_intersection colors colors' in
	     if IntSet.is_empty cut
	     then merge_colorings colors colors'
	     else
	       let v = IntSet.choose cut in
		 if IntMap.get colors v = IntMap.get colors' v
		 then merge_colorings colors colors'
		 else merge_colorings colors (invert_coloring colors'))
	IntMap.empty
	components
	
and solve g =
  if !Util.verbose
  then Printf.eprintf "solve\t\tn = %3d m = %4d (%4d)\n%!"
    (ELGraph.num_vertices g) (ELGraph.num_edges g) (Bsg.num_edges g);
  if ELGraph.num_vertices g <= 5
  then solve_brute_force g
  else
    let g, dk = ELGraph.fold_edges
      (fun (g, dk) i j {eq = eq; ne = ne} ->
	 if eq > 0 && ne > 0
	 then
	   if eq = ne
	   then ELGraph.disconnect g i j, dk + eq
	   else
	     let m = min eq ne in ELGraph.set_label g i j {eq = eq - m; ne = ne - m}, dk + m
	 else g, dk) g (g, 0) in
    if !Util.verbose && dk > 0 then Printf.eprintf " double edges: k reduced by %d\n%!" dk;
    let g, dk =
      ELGraph.fold_vertices
	(fun (g, dk) v n ->
	   if IntMap.has_key n v
	   then ELGraph.disconnect g v v, dk + 1
	   else g, dk)
	g
	(g, 0)
    in
      if !Util.verbose && dk > 0 then Printf.eprintf " self-loops: k reduced by %d\n%!" dk;
      solve_biconnected g
;;
