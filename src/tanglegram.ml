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

type 'a tree = Leaf of 'a | Node of 'a tree * 'a tree;;

let rec fold_dfs f accu = function
    Leaf x -> f accu x
  | Node (t1, t2) ->
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
    [< t1 = parse_tree; 'Comma; t2 = parse_tree; >] -> Node (t1, t2)
;;

let rec output_tree printer channel = function
    Leaf s -> printer channel s
  | Node (t1, t2) ->
      Printf.fprintf channel "(%a,%a)" (output_tree printer) t1 (output_tree printer) t2
;;

module StringMap = Map.Make(String);;

let parse_trees s1 s2 =
  let t1 = parse_tree (lex (s1)) in
  let t2 = parse_tree (lex (s2)) in
  let name_of_leaf = IntMap.empty in
  let leaf_of_name = StringMap.empty in
  let rec loop name_of_leaf leaf_of_name = function
      Leaf s ->
	let i = IntMap.size name_of_leaf
	in
	  ((IntMap.add name_of_leaf i s),
	   (StringMap.add s i leaf_of_name),
	   Leaf i)
    | Node (t1, t2) ->
	let name_of_leaf, leaf_of_name, t1 = loop name_of_leaf leaf_of_name t1 in
	let name_of_leaf, leaf_of_name, t2 = loop name_of_leaf leaf_of_name t2 in
	  name_of_leaf, leaf_of_name, Node (t1, t2) in
  let name_of_leaf, leaf_of_name, t1 = loop name_of_leaf leaf_of_name t1 in
  let rec loop = function
      Leaf s -> Leaf (StringMap.find s leaf_of_name)
    | Node (t1, t2) -> Node (loop t1, loop t2) in
  let t2 = loop t2 in
    t1, t2, name_of_leaf
;;

let read_tree_file channel =
  let rec loop trees =
    try
      let l = input_line channel in
      let l = if String.contains l '#' then String.sub l 0 (String.index l '#') else l in
      let l = Util.strip l in
	if l = "TCG 2.0" || l = "" then loop trees
	else loop (l :: trees)
    with End_of_file -> trees
  in
    match loop [] with
	[l1; l2] -> l1, l2
      | _ -> failwith "Need exactly two trees"
;;

let tanglegram_to_bsg t1 t2 =
  let rec loop path i parents = function
      Leaf x ->
	let parents = IntMap.add parents x (List.rev path) in
	  parents, i
    | Node (l, r) ->
	let path = i :: path in
	let i = i + 1 in
	let parents, i = loop path i parents l in
	let parents, i = loop path i parents r in
	  parents, i in
  let t1_parents, _ = loop [] 0 IntMap.empty t1 in
  let t2_parents, _ = loop [] 0 IntMap.empty t2 in
  let leaves_t1 = List.rev (fold_dfs (fun ls l -> l :: ls) [] t1) in
  let leaves_t2 = List.rev (fold_dfs (fun ls l -> l :: ls) [] t2) in
  let rec list_pos x = function
      [] -> failwith "list_pos"
    | y :: _ when y = x -> 0
    | y :: ys -> 1 + list_pos x ys in
  let rec fold_pairs f accu = function
      [] -> accu
    | x :: xs ->
	let accu = List.fold_left (fun accu y -> f accu x y) accu xs in
	  fold_pairs f accu xs in
  let rec last_common_elt l1 l2 =
    let rec loop result = function
	x :: xs, y :: ys when x = y -> loop x (xs, ys)
      | _ -> result
    in
      match (l1, l2) with
	  x :: xs, y :: ys when x = y -> loop x (xs, ys)
	| _ -> failwith "last_common_elt" in
  let n = IntMap.size t1_parents - 1 in
  let g = Util.fold_n (fun g i -> ELGraph.add_vertex g i) (2 * n) ELGraph.empty in
  let g =
    fold_pairs
      (fun g x y ->	 
	 let crosses = list_pos x leaves_t2 > list_pos y leaves_t2 in
	 let lca_t1 = last_common_elt (IntMap.get t1_parents x) (IntMap.get t1_parents y) in
	 let lca_t2 = last_common_elt (IntMap.get t2_parents x) (IntMap.get t2_parents y) in
	   ELGraph.modify_label_default
	     (fun label ->
		if not crosses
		then { label with Bsg.eq = label.Bsg.eq + 1}
		else { label with Bsg.ne = label.Bsg.ne + 1})
	     g lca_t1 (n + lca_t2) { Bsg.eq = 0; Bsg.ne = 0 })
      g leaves_t1
  in
    g      
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
  let s1, s2 = read_tree_file stdin in
  let t1, t2, name_of_leaf = parse_trees s1 s2 in
  let start = Util.timer () in
  let g = tanglegram_to_bsg t1 t2 in
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
  let n = IntMap.size colors / 2
  in
    if !stats_only      
    then
      Printf.printf "%5d %10.2f %3d\n" k (stop -. start) !Util.max_unreducible_size
    else
      let rec loop i = function
	  Node (l, r) ->
	    let swap = IntMap.get colors i in
	    let i = i + 1 in
	    let l, i = loop i l in
	    let r, i = loop i r in
	      if swap
	      then Node (r, l), i
	      else Node (l, r), i
	| leaf -> leaf, i in
      let t1', _ = loop 0 t1 in
      let t2', _ = loop n t2 in
	output_tree (fun chan i -> output_string chan (IntMap.get name_of_leaf i)) stdout t1';
	print_newline ();
	output_tree (fun chan i -> output_string chan (IntMap.get name_of_leaf i)) stdout t2';
	print_newline ();
;;
