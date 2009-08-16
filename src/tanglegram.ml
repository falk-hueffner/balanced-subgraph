(* bsg -- solve the balanced subgraph problem
   Copyright (C) 2009  Falk Hüffner

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

(** Solve Tanglegram Layout by reduction to Balanced Subgraph.  *)

type 'a tree = Leaf of 'a | Node of 'a tree list;;

let rec fold_dfs f accu = function
    Leaf x -> f accu x
  | Node [t1; t2] ->
      let accu = fold_dfs f accu t1 in
      let accu = fold_dfs f accu t2 in
	accu
;;

type token = Lparen | Rparen | Comma | String of string;;

let rec find_first_of s pat p0 =
  if p0 >= String.length s || String.contains pat s.[p0]
  then p0
  else find_first_of s pat (p0 + 1)
;;

let lex s =
  let pos = ref 0 in
  let rec next_token pos =
    if !pos >= String.length s
    then None
    else match s.[!pos] with
	' ' | '\t' -> incr pos; next_token pos
      | '(' -> incr pos; Some Lparen
      | ')' -> incr pos; Some Rparen
      | ',' -> incr pos; Some Comma
      | _ -> let pos2 = find_first_of s "(), \t" !pos in
	let str = String.sub s !pos (pos2 - !pos) in
	  pos := pos2;
	  Some (String str)
  in
    Stream.from (fun count -> next_token pos)
;;

let rec parse_tree = parser
    [< 'Lparen; t = parse_tuple; 'Rparen >] -> t
  | [< 'String s >] -> Leaf s
and parse_tuple = parser
    [< t1 = parse_tree; 'Comma; t2 = parse_tree; >] -> Node [t1; t2]
;;

let rec output_tree printer channel = function
    Leaf s -> printer channel s
  | Node [t1; t2] ->
      Printf.fprintf channel "(%a,%a)" (output_tree printer) t1 (output_tree printer) t2
;;

module StringMap = Map.Make(String);;

let rec fold_pairs f accu = function
    [] -> accu
  | x :: xs ->
      let accu = List.fold_left (fun accu y -> f accu x y) accu xs in
	fold_pairs f accu xs
;;

let label_tree s =
  let t = parse_tree (lex (s)) in
  let name_of_leaf = IntMap.empty in
  let leaf_of_name = StringMap.empty in
  let rec loop name_of_leaf leaf_of_name = function
      Leaf s ->
	if StringMap.mem s leaf_of_name then failwith "Duplicate leaf label" else
	let i = IntMap.size name_of_leaf
	in
	  ((IntMap.add name_of_leaf i s),
	   (StringMap.add s i leaf_of_name),
	   Leaf i)
    | Node [t1; t2] ->
	let name_of_leaf, leaf_of_name, t1 = loop name_of_leaf leaf_of_name t1 in
	let name_of_leaf, leaf_of_name, t2 = loop name_of_leaf leaf_of_name t2 in
	  name_of_leaf, leaf_of_name, Node [t1; t2] in
  let name_of_leaf, leaf_of_name, t = loop name_of_leaf leaf_of_name t in
    t, name_of_leaf, leaf_of_name
;;

let read_tree_file channel =
  let rec loop trees edges =
    try
      let l = input_line channel in
      let l = if String.contains l '#' then String.sub l 0 (String.index l '#') else l in
      let l = Util.strip l in
	if l = "TCG 2.0" || l = "" then loop trees edges
	else if List.length trees < 2
	then loop (l :: trees) edges
	else loop trees (l :: edges)
    with End_of_file -> trees, edges
  in
    match loop [] [] with
	[l2; l1], edges -> l1, l2, edges
      | _ -> failwith "Need exactly two trees"
;;

let tanglegram_to_bsg tl tr edges =
  let rec loop path i parents = function
      Leaf x ->
	let parents = IntMap.add parents x (List.rev path) in
	  parents, i
    | Node [l; r] ->
	let path = i :: path in
	let i = i + 1 in
	let parents, i = loop path i parents l in
	let parents, i = loop path i parents r in
	  parents, i in
  let tl_parents, _ = loop [] 0 IntMap.empty tl in
  let tr_parents, _ = loop [] 0 IntMap.empty tr in
  let tl_pos = fold_dfs (fun pos x -> IntMap.add pos x (IntMap.size pos)) IntMap.empty tl in
  let tr_pos = fold_dfs (fun pos x -> IntMap.add pos x (IntMap.size pos)) IntMap.empty tr in
  let rec last_common_elt l1 l2 =
    let rec loop result = function
	x :: xs, y :: ys when x = y -> loop x (xs, ys)
      | _ -> result
    in
      match l1, l2 with
	  x :: xs, y :: ys when x = y -> loop x (xs, ys)
	| _ -> failwith "last_common_elt" in
  let nl = IntMap.size tl_parents - 1 in
  let nr = IntMap.size tr_parents - 1 in
  let g = Util.fold_n (fun g i -> ELGraph.add_vertex g i) (nl + nr) ELGraph.empty
  in
    fold_pairs
      (fun g (l1, r1) (l2, r2) ->
	 if l1 = l2 || r1 = r2 then g else
	   let crosses = (IntMap.get tl_pos l1 < IntMap.get tl_pos l2)
	              <> (IntMap.get tr_pos r1 < IntMap.get tr_pos r2) in
	   let lca_tl = last_common_elt (IntMap.get tl_parents l1) (IntMap.get tl_parents l2) in
	   let lca_tr = last_common_elt (IntMap.get tr_parents r1) (IntMap.get tr_parents r2) in
	     ELGraph.modify_label_default
	       (fun label ->
		  if not crosses
		  then { label with Bsg.eq = label.Bsg.eq + 1}
		  else { label with Bsg.ne = label.Bsg.ne + 1})
	       g lca_tl (nl + lca_tr) { Bsg.eq = 0; Bsg.ne = 0 })
      g edges
;;

let num_crossings tl tr edges =
  let tl_pos = fold_dfs (fun pos x -> IntMap.add pos x (IntMap.size pos)) IntMap.empty tl in
  let tr_pos = fold_dfs (fun pos x -> IntMap.add pos x (IntMap.size pos)) IntMap.empty tr
  in
    fold_pairs
      (fun num_crossings (l1, r1) (l2, r2) ->
	 let crosses = l1 <> l2 && r1 <> r2 &&
	               (IntMap.get tl_pos l1 < IntMap.get tl_pos l2)
	            <> (IntMap.get tr_pos r1 < IntMap.get tr_pos r2) in
	   if crosses then num_crossings + 1 else num_crossings)
      0 edges;
;;

let usage_msg = "Find optimal tanglegrams";;

let stats_only = ref false;;

let specs = [
  ("-c", Arg.Set_int(Util.max_cut_size),
         "Set maximum cut size for data reduction (0..4)");
  ("-u", Arg.Clear(Util.downward_compress),
         "Use induction for iterative compression");
  ("-s", Arg.Set(stats_only),
         "Print statistics only");
  ("-v", Arg.Set(Util.verbose),
         "Print progress to stderr");
];;

let () =
  Util.downward_compress := true;
  Arg.parse specs (fun _ -> Arg.usage specs usage_msg) usage_msg;
  if !Util.max_cut_size < 0 || !Util.max_cut_size > 4 then begin
    Printf.eprintf "maximum cut size must be 0..4\n";
    exit 1;
  end;
  let s1, s2, edges = read_tree_file stdin in
  let t1, name_of_leaf_t1, leaf_of_name_t1 = label_tree s1 in
  let t2, name_of_leaf_t2, leaf_of_name_t2 = label_tree s2 in
  let edges = List.map
    (fun l -> match Util.split_string l with
	 [s1; s2] -> StringMap.find s1 leaf_of_name_t1, StringMap.find s2 leaf_of_name_t2
       | _ -> failwith "Bad edge syntax") edges in
  let start = Util.timer () in
  let edges = IntMap.fold
    (fun edges x name_x ->
       if StringMap.mem name_x leaf_of_name_t2
       then (x, (StringMap.find name_x leaf_of_name_t2)) :: edges
       else edges)
    name_of_leaf_t1 edges in
  let g = tanglegram_to_bsg t1 t2 edges in
  if !Util.verbose then Printf.eprintf "Initial number of crossings: %d\n" (num_crossings t1 t2 edges);
  if false then begin
    ELGraph.iter_edges
      (fun i j l ->
	 for k = 1 to l.Bsg.eq - min l.Bsg.eq l.Bsg.ne do	
	   Printf.printf "%d %d 0\n" i j
	 done;
	 for k = 1 to l.Bsg.ne - min l.Bsg.eq l.Bsg.ne do	
	   Printf.printf "%d %d 1\n" i j
	 done)
      g;
    exit 0;
  end;
  if !stats_only
  then
    Printf.printf "%5d %6d %!" (ELGraph.num_vertices g) (Bsg.num_edges g);
  let colors = Solve.solve g in
  let stop = Util.timer () in
  let k = Bsg.coloring_cost g colors in
  let n = IntMap.size name_of_leaf_t1 - 1 in
  let rec loop i = function
      Node [l; r] ->
	let swap = IntMap.get colors i in
	let i = i + 1 in
	let l, i = loop i l in
	let r, i = loop i r in
	  if swap
	  then Node [r; l], i
	  else Node [l; r], i
    | leaf -> leaf, i in
  let t1', _ = loop 0 t1 in
  let t2', _ = loop n t2
  in
    assert (num_crossings t1' t2' edges = k);
    if !stats_only      
    then
      Printf.printf "%5d %10.2f %3d\n" k (stop -. start) !Util.max_unreducible_size
    else begin
      output_tree (fun chan i -> output_string chan (IntMap.get name_of_leaf_t1 i)) stdout t1';
      print_newline ();
      output_tree (fun chan i -> output_string chan (IntMap.get name_of_leaf_t2 i)) stdout t2';
      print_newline ();
    end
;;
