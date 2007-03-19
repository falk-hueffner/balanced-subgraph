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

let fold_pairs f s1 s2 accu =
  IntSet.fold
    (fun accu x1 -> IntSet.fold (fun accu x2 -> f accu x1 x2) s2 accu) s1 accu
;;

let add_gadget gadgets g c_set cost =
  let colorings = Solve.solve_all_colorings g c_set in
  let edges = ELGraph.fold_edges (fun r i j l -> (i, j, l) :: r) g [] in
  let costs = Array.init
    (IntMap.size colorings)
    (fun i ->  Bsg.coloring_cost g (IntMap.get colorings i)) in
  let rec test = function
      [] -> true
    | (cost', costs', edges') :: gadgets ->
	if costs = costs' &&
	  (cost > cost' || (cost = cost' && List.length edges > List.length edges'))
	then false
	else test gadgets in
    if test gadgets    
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
    (fun (i, j, {Bsg.eq = eq; Scs.ne = ne}) ->
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
	   add_gadget gadgets (ELGraph.connect g i j {Bsg.eq = 1; Scs.ne = 0} ) c_set 0 in
	 let gadgets =
	   add_gadget gadgets (ELGraph.connect g i j {Bsg.eq = 0; Scs.ne = 1} ) c_set 0
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
    let count = ref 0 in
    let rec loop gadgets =
      if not (bump ()) then gadgets
      else
	let g, _ = List.fold_left
	  (fun (g, i) (v, w) ->
	     if l.(i) > 0
	     then ELGraph.connect g v w { Bsg.eq = l.(i); Scs.ne = 0 }, i + 1
	     else if l.(i) < 0
	     then ELGraph.connect g v w { Bsg.eq = 0; Scs.ne = -l.(i) }, i + 1
	     else g, i + 1)
	  (g, 0)
	  edges in
	Printf.eprintf "s_size = %d gadget %d%!\n" s_size !count;
        incr count;
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

let () =
  let gadgets = [] in
  let gadgets = single_edge_gadgets gadgets 3 in
  let gadgets = extra_vertices_gadgets gadgets 3 1 in
    List.iter
      (fun (cost, costs, edges) -> print_gadget costs cost edges)
      (List.sort compare gadgets);
;;
