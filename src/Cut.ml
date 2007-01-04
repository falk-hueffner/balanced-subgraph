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

let vertex_cut_transform g =
  let d = (Graph.max_vertex g) + 1 in
  let entry i = i in
  let outlet i = i + d in
  let g' = Graph.fold_vertices
    (fun g' i _ ->
       let g' = Digraph.add_vertex g' (entry i) in
       let g' = Digraph.add_vertex g' (outlet i) in
       let g' = Digraph.connect g' (entry i) (outlet i) in
	 g')
    g
    Digraph.empty in
  Graph.fold_edges
    (fun g' i j ->
       let g' = Digraph.connect g' (outlet i) (entry j) in
       let g' = Digraph.connect g' (outlet j) (entry i) in
	 g')
    g
    g'
;;

let is_n_connected g n v w =
  let g' = vertex_cut_transform g in
  let d = (Graph.max_vertex g) + 1 in
  let outlet_v = v + d in
  let entry_w = w in
  let flow = Diflow.make g' (fun _ _ -> 1) in
  let rec loop i flow =
    if i >= n || Diflow.influx flow outlet_v < i
    then flow
    else loop (i + 1) (Diflow.augment flow outlet_v entry_w) in
  let flow = loop 0 flow in
    Diflow.influx flow outlet_v >= n
;;

let n_connected_components g n =
  UnionFind.equivalence_classes (Graph.vertex_set g) (is_n_connected g n)
;;

(*
let connected_components g =
  let rec dfs v seen =
    let seen = IntSet.add seen v in
      IntSet.fold
	(fun seen w -> if not (IntSet.contains seen w) then dfs w seen else seen)
	(Graph.neighbors g v)
	seen in
  let rec loop components fresh =
    if IntSet.is_empty fresh
    then components
    else
      let v = IntSet.choose fresh in
      let component = dfs v IntSet.empty in
	loop (component :: components) (IntSet.minus fresh component)
  in
    loop [] (Graph.vertex_set g)
;;
*)

(* Closely following H. Gabow: "Path-based depth-first search for
   strong and biconnected components", Inf. Proc. Lett. 2000 *)
let biconnected_components g =
  let n = Graph.max_vertex g in
  let s = Array.make (n+1) 0 and top_s = ref (-1) in
  let b = Array.make (2 *(n+1)) 0 and top_b = ref (-1) in
  let push_s x = incr top_s; s.(!top_s) <- x in
  let push_b x = incr top_b; b.(!top_b) <- x in
  let pop_s () = let r = s.(!top_s) in decr top_s; r in
  let pop_b () = let r = b.(!top_b) in decr top_b; r in
  let i = Array.make (n+1) (-1) in
  let c = ref n in
  let rec dfs v =
    push_s v;
    i.(v) <- !top_s;
    if i.(v) > 0 then push_b i.(v);
    Graph.iter_neighbors
      (fun w ->
	 if i.(w) = -1 then begin
	   push_b i.(v);
	   dfs w;
	 end else
	   while i.(v) > 0 && i.(w) < b.(!top_b - 1) do
	     ignore (pop_b ());
	     ignore (pop_b ());
	   done) g v;
    if i.(v) = 0
    then i.(pop_s ()) <- !c
    else if i.(v) = b.(!top_b) then begin
      ignore (pop_b ());
      ignore (pop_b ());
      incr c;
      while i.(v) <= !top_s do
 	i.(pop_s ()) <- !c
      done
    end
  in
    Graph.iter_vertices
      (fun v _ ->
	 if i.(v) = -1 && not (Graph.is_deg0 g v) then dfs v)
      g;
    let components = 
      Graph.fold_edges
	(fun components v w ->
	   let c = min i.(v) i.(w) in
	     IntMap.modify_default
	       (fun component -> IntSet.put (IntSet.put component v) w)
	       components c IntSet.empty)
	g
	IntMap.empty
    in
      IntMap.fold (fun components _ component -> component :: components) components []
;;

let cut_corner g =
  let better s1 c1 s2 c2 =
    if IntSet.size s2 < IntSet.size c2 - 1
    then
      if not (IntSet.size s1 < IntSet.size s1 - 1)
      then s1, c1
      else if (IntSet.size c1 - IntSet.size s1 < IntSet.size c2 - IntSet.size s2)
      then s1, c1
      else s2, c2
    else
      if IntSet.size c1 < IntSet.size c2
	|| (IntSet.size c1 = IntSet.size c2 && IntSet.size s1 < IntSet.size s2)
      then s1, c1
      else s2, c2 in
  let rec grow s c best_s best_c =
(*     Printf.eprintf "s = %a c = %a\n" IntSet.output s IntSet.output c; *)
(*     Printf.eprintf "bests = %a bestc = %a\n" IntSet.output best_s IntSet.output best_c; *)
    if IntSet.size s > Graph.num_vertices g / 2
    then best_s, best_c
    else
      let best_v, best_c_size =
	IntSet.fold
	  (fun (best_v, best_c_size) v ->
	     let v_c_size = IntSet.size (IntSet.minus (Graph.neighbors g v) c) in
	       if v_c_size < best_c_size then v, v_c_size else best_v, best_c_size)
	  c (0, max_int) in
      let s = IntSet.add s best_v in
      let c = IntSet.delete c best_v in
      let new_c = IntSet.minus (Graph.neighbors g best_v) s in
      let c = IntSet.union c new_c in
(* 	Printf.eprintf "s' = %a c' = %a\n" IntSet.output s IntSet.output c; *)
      let best_s, best_c = better s c best_s best_c in
	grow s c best_s best_c
  in
    Graph.fold_vertices
      (fun (best_s, best_c) v neighbors_v ->
	 let s, c =
	   grow (IntSet.singleton v) neighbors_v (IntSet.singleton v) neighbors_v
	 in
(* 	   if IntSet.size c <= 3 then *)
(* 	     Printf.eprintf "s = %a c = %a\n" IntSet.output s IntSet.output c; *)
	   better s c best_s best_c)
      g
      (IntSet.singleton (Graph.max_vertex g), Graph.neighbors g (Graph.max_vertex g))
;;

(*

let cut_corner g =
  let rec grow s neighbors best_s best_neighbors =
(*     Printf.eprintf "s = %a neighbors = %a\n%!" IntSet.output s IntSet.output neighbors; *)
    if IntSet.size s >= 64 || IntSet.size s > Graph.num_vertices g / 2
    then best_s, best_neighbors
    else
      let best, best_new =
	IntSet.fold
	  (fun (best, best_size) v ->
	     let v_size = IntSet.size (IntSet.minus (Graph.neighbors g v) neighbors) in
	       if v_size < best_size then v, v_size else best, best_size)
	  neighbors
	  (0, max_int)
      in
	let s = IntSet.add s best in
(* 	Printf.eprintf "about to delete %d from %a\n%!" best IntSet.output neighbors; *)
	let neighbors = IntSet.delete neighbors best in
(* 	Printf.eprintf "Graph.neighbors g best = %a s = %a\n" *)
(* 	  IntSet.output (Graph.neighbors g best) IntSet.output s; *)
	let new_neighbors = IntSet.minus (Graph.neighbors g best) s in
	let neighbors = IntSet.union neighbors new_neighbors in
	  if (*IntSet.size s > IntSet.size neighbors &&*)
(* 	    IntSet.size neighbors <= IntSet.size best_neighbors *)
	    IntSet.size neighbors < IntSet.size best_neighbors
	  then grow s neighbors s neighbors
	  else grow s neighbors best_s best_neighbors
  in
    Graph.fold_vertices
      (fun l v neighbors_v ->
	 let s, neighbors =
	   grow (IntSet.singleton v) neighbors_v (IntSet.singleton v) neighbors_v
	 in
	   if IntSet.is_empty s
	   then l
	   else (IntSet.union s neighbors, neighbors) :: l)
      g
      []
;;
*)
