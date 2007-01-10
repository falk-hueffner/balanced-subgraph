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

let usage_msg = "Find maximum sign consistent subgraphs";;

let stats_only = ref false;;

let specs = [
  ("-s", Arg.Set(stats_only),
         "Print statistics only");
  ("-v", Arg.Set(Util.verbose),
         "Print progress to stderr");
];;

let find_lincomb v vs =
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
(*     Printf.eprintf "trial max_cost = %d max_max_cost = %d\n%!" max_cost max_max_cost; *)
    if max_cost > max_max_cost
    then None
      else
	match loop v vs max_cost with
	    None -> trial v (max_cost + 1) max_max_cost
	  | something -> something in
  let rec shift d =
    if d >= 2 then None
    else begin
(*       Printf.eprintf "shift %d: %a\n%!" d (Util.output_array Util.output_int) v; *)
      let v = Array.map ((+) d) v in
      let max_max_cost = ((Array.fold_left (+) 0 v) + 1) / 2 in (* every vec. has at least 2 1s *)
	match trial v 0 max_max_cost with
	    None -> shift (d + 1)
	  | Some (cost, ds, gadgets) -> Some (cost + d, ds, gadgets)
    end
  in
    shift 0
;;

let fold_pairs f s1 s2 accu =
  IntSet.fold
    (fun accu x1 -> IntSet.fold (fun accu x2 -> f accu x1 x2) s2 accu) s1 accu
;;

let add_gadget gadgets g c_set cost =
  let colorings = Solve.solve_all_colorings g c_set in
(*   let costs = IntMap.map (fun _ coloring -> Ulp.coloring_cost g coloring) colorings in *)
  let edges = ELGraph.fold_edges (fun r i j l -> (i, j, l) :: r) g [] in
  let costs = Array.init
    (IntMap.size colorings)
    (fun i ->  Ulp.coloring_cost g (IntMap.get colorings i))
  in
(*     Printf.printf "%d %a\n%!" cost (Util.output_array Util.output_int) costs; *)
    if (* true ||  *)
      match find_lincomb costs gadgets with
	  None -> ((* Printf.printf "fresh\n%!"; *) true)
	| Some (cost', _, _) ->  ((* Printf.printf "old: %d new: %d\n%!" cost' cost; *) cost < cost')
    then (cost, costs, edges) :: gadgets
    else gadgets
;;

let print_gadget costs cost edges =
  Printf.printf "(%d,[|" cost;
  for i = 0 to Array.length costs - 1 do
    if i > 0 then print_string ";";
    Printf.printf "%d" costs.(i);
  done;
  print_string "|],[";
  List.iter
    (fun (i, j, {Ulp.eq = eq; Ulp.ne = ne}) ->
       Printf.printf "(%d,%d,{eq=%d;ne=%d});" i j eq ne)
    edges;
  print_string "]);\n";
;;

let single_edge_gadgets gadgets c_size =
  let c_set = Util.fold_n IntSet.add c_size IntSet.empty in
  let g = Util.fold_n ELGraph.add_vertex c_size ELGraph.empty in
  let n = ELGraph.num_vertices g in
  let edges = fold_pairs (fun l i j -> if i < j then (i, j) :: l else l) c_set c_set [] in
  let m = List.length edges in
    List.fold_left
      (fun gadgets (i, j) ->
	 let gadgets =
	   add_gadget gadgets (ELGraph.connect g i j {Ulp.eq = 1; Ulp.ne = 0} ) c_set 0 in
	 let gadgets =
	   add_gadget gadgets (ELGraph.connect g i j {Ulp.eq = 0; Ulp.ne = 1} ) c_set 0
	 in
	   gadgets)
      gadgets edges
;;

let extra_vertices_gadgets gadgets c_size s_size =
  let c_set = Util.fold_n IntSet.add c_size IntSet.empty in
  let s_set = Util.fold_n (fun s i -> IntSet.add s (i + c_size)) s_size IntSet.empty in
  let g = Util.fold_n ELGraph.add_vertex (c_size + s_size) ELGraph.empty in
  let edges = fold_pairs (fun l i j -> (i, j) :: l) s_set c_set [] in
  let edges = fold_pairs (fun l i j -> if i < j then (i, j) :: l else l) s_set s_set edges in
(*     Util.output_list (fun c (i, j) -> Printf.fprintf c "(%d, %d)" i j) stdout edges; *)
  let m = List.length edges in
  let max_mult = 1 in
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
    let rec loop gadgets =
      if not (bump ()) then gadgets
      else
	let g, _ = List.fold_left
	  (fun (g, i) (v, w) ->
	     if l.(i) > 0
	     then ELGraph.connect g v w { Ulp.eq = l.(i); Ulp.ne = 0 }, i + 1
	     else if l.(i) < 0
	     then ELGraph.connect g v w { Ulp.eq = 0; Ulp.ne = -l.(i) }, i + 1
	     else g, i + 1)
	  (g, 0)
	  edges in
	let s_c_edges =
	  fold_pairs
	    (fun s_c_edges i j -> s_c_edges + if ELGraph.is_connected g i j then 1 else 0)
	    s_set c_set 0 in
	let gadgets =
	  if s_c_edges >= 3 && ELGraph.is_connected_graph (ELGraph.subgraph g s_set)
	  then add_gadget gadgets g c_set s_size
	  else gadgets
	in
	  loop gadgets
    in
      loop gadgets
;;

(*
let () =
  let gadgets = [] in
  let gadgets = single_edge_gadgets gadgets 3 in
  let gadgets = extra_vertices_gadgets gadgets 3 1 in
    (*
    prerr_string "--------------\n";
  let gadgets = extra_vertices_gadgets gadgets 5 2 in
  let gadgets = extra_vertices_gadgets gadgets 5 3 in
    *)
    List.iter
      (fun (cost, costs, edges) -> print_gadget costs cost edges)
      (List.sort compare gadgets);
    (*
    let vv = [
(*       [| 3; 3; 2; 4; 4; 3; 3; 4 |]; *)
(*       [| 5; 7; 5; 7; 7; 6; 7; 6 |]; *)
(*       [| 5; 5; 7; 7; 7; 7; 7; 7 |]; *)
(*       [| 3; 5; 5; 5; 4; 5; 6; 4 |]; *)
(*       [| 2; 4; 4; 5; 3; 5; 5; 4 |]; *)
(*       [| 5; 5; 4; 4; 4; 5; 5; 5 |]; *)
(*       [| 4; 7; 6; 7; 8; 9; 8; 7 |]; *)
(*       [| 6; 5; 5; 6; 6; 5; 5; 6 |]; *)
(*       [| 6; 5; 5; 4; 5; 4; 4; 3 |]; *)
(*       [| 5; 4; 5; 4; 4; 5; 4; 5 |]; *)
(*       [| 7; 7; 5; 7; 9; 7; 7; 7 |]; *)
(*       [| 2; 3; 3; 2; 3; 3; 4; 2 |]; *)
(*       [| 8; 7; 8; 7; 7; 8; 7; 8 |]; *)
(*       [| 4; 4; 4; 4; 4; 4; 4; 4 |]; *)
(*       [| 6; 6; 5; 5; 7; 5; 7; 6 |]; *)
(*       [| 1; 2; 4; 1; 2; 3; 3; 0 |]; *)
      [| 4; 6; 4; 4; 5; 7; 6; 5; 4; 7; 6; 6; 3; 5; 5; 5 |]; (* 2 enough!  *)
      [| 5; 5; 9; 6; 4; 7; 7; 6; 6; 6; 8; 5; 5; 8; 6; 6 |]; (* 2 not enough, apparently... *)
      [| 9; 9; 8; 8; 6; 6; 7; 7; 8; 8; 7; 7; 7; 7; 8; 8 |];
      [| 8; 7; 7; 8; 8; 7; 7; 8; 8; 7; 7; 8; 8; 7; 7; 8 |];
      [| 9; 9; 8; 8; 9; 9; 8; 8; 8; 8; 9; 9; 8; 8; 9; 9 |];
      [| 9; 8; 8; 6; 9; 9; 9; 6; 9; 8; 9; 6; 9; 9; 11; 8 |];      
    ]
    in
    List.iter
      (fun v ->
	 Util.output_array Util.output_int stdout v; print_newline ();
	 match find_lincomb v gadgets with
	     None -> ()
	   | Some (cost, costvecs, gadgets) ->
	       Printf.printf "costs: %d\n" cost;
	       List.iter
		 (fun costvec ->
		    Printf.printf "%a\n" (Util.output_array Util.output_int) costvec)
		 costvecs;
	       print_newline ();
      ) vv;
    *)
    exit 0;
;;
*)
  
let () =
  Arg.parse specs (fun _ -> Arg.usage specs usage_msg) usage_msg;
  let g, vertex_numbers, vertex_names = Ulp.input stdin in
  let m = ELGraph.fold_edges (fun m _ _ { Ulp.eq = eq; Ulp.ne = ne } -> m + eq + ne) g 0 in
(*   Ulp.output stdout g; *)
  let start = Util.timer () in
  let colors = Solve.solve g in
(*   Printf.eprintf "result: %a\n%!" (IntMap.output Util.output_bool) colors; *)
  let stop = Util.timer () in
  let k = Ulp.coloring_cost g colors
  in
    if !stats_only      
    then
      Printf.printf "%5d %6d %5d %10.2f\n"
	(ELGraph.num_vertices g) m k (stop -. start)
    else
      ELGraph.iter_edges
	(fun i j { Ulp.eq = eq; Ulp.ne = ne } ->
	   if IntMap.get colors i = IntMap.get colors j
	   then for l = 1 to ne do
	     Printf.printf "%s %s 1\n" (IntMap.get vertex_names i) (IntMap.get vertex_names j)
	   done
	   else for l = 1 to eq do
	     Printf.printf "%s %s 0\n" (IntMap.get vertex_names i) (IntMap.get vertex_names j)
	   done)
	g
;;
