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

(*
let find_gadgets c_size s_size max_mult =
  let g = Util.fold_n ELGraph.add_vertex (c_size + s_size) ELGraph.empty in
(*   ELGraph.output stdout output_edge g;  *)
  let fold_pairs f n a =
    let rec loop a i j =
      if i >= n then a
      else if j >= i then loop a (i + 1) 0
      else loop (f a i j) i (j + 1)
    in
      loop a 0 0 in
  let n = ELGraph.num_vertices g in
  let edges = fold_pairs (fun l i j -> (i, j) :: l) n [] in
  let m = List.length edges in
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
      loop 0 in
  let c_set = Util.fold_n IntSet.add c_size IntSet.empty in
  let s_set = Util.fold_n (fun s i -> IntSet.add s (i + c_size)) s_size IntSet.empty
  in
(*     Printf.printf "c = %a s = %a\n" IntSet.output c_set IntSet.output s_set; *)
    l.(0) <- l_min - 1;
    while bump () do
(*       Printf.printf "l = %a\n" (Util.output_array Util.output_int) l; *)
      let g, _ = List.fold_left
	(fun (g, i) (v, w) ->
	   if l.(i) > 0
	   then ELGraph.connect g v w { Ulp.eq = l.(i); Ulp.ne = 0 }, i + 1
	   else if l.(i) < 0
	   then ELGraph.connect g v w { Ulp.eq = 0; Ulp.ne = -l.(i) }, i + 1
	   else g, i + 1)
	(g, 0)
	edges in
(*  	ELGraph.output stdout output_edge g; *)
      let colorings = Ulp.solve_all_colorings g c_set in
      let costs = IntMap.map (fun _ coloring -> Ulp.coloring_cost g coloring) colorings
      in
(* 	  Util.output_list stdout Util.output_int r; *)
(*  	let m = List.fold_left min max_int r in *)
(*  	let r = List.map (fun i -> i - m) r in *)
(* 	  Util.output_list stdout Util.output_int r; *)
(* 	  ELGraph.output stdout output_edge g; *)
	print_char '[';
 	IntMap.iter (fun i cost ->
		       if i > 0 then print_string ", ";
		       Printf.printf "%d" cost) costs;
	print_string "], [";	 
	ELGraph.iter_edges (fun i j l -> Printf.printf "(%d, %d, %d)"
			      i j (if l.Ulp.eq > 0 then 0 else 1)) g;
	print_string "]\n";	 
	  (*
	  if GadgetMap.has_key r !gadgets
	  then 
	  gadgets := GadgetMap.add r (Array.copy l) !gadgets;
	  *)	
    done;
;;

let () =
  find_gadgets 3 1 1;
  exit 0;
;;
*)

let () =
  Arg.parse specs (fun _ -> Arg.usage specs usage_msg) usage_msg;
  let g, vertex_numbers, vertex_names = Ulp.input stdin in
  let m = ELGraph.fold_edges (fun m _ _ { Ulp.eq = eq; Ulp.ne = ne } -> m + eq + ne) g 0 in
(*   Ulp.output stdout g; *)
  let start = Util.timer () in
  let colors = Ulp.solve g in
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
