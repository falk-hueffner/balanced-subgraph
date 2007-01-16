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
  let rec grow sc s c =
(*     Printf.eprintf "s = %a c = %a\n" IntSet.output s IntSet.output c; *)
    if IntSet.size s + IntSet.size c >= Graph.num_vertices g / 2
    then sc
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
(*  	Printf.eprintf "s' = %a c' = %a\n" IntSet.output s IntSet.output c; *)
      let sc = if IntSet.size c <= !Util.max_cut_size then (s, c) :: sc else sc in
	grow sc s c
  in
    Graph.fold_vertices
      (fun sc v neighbors_v ->
	 let sc =
	   if IntSet.size neighbors_v <= !Util.max_cut_size
	   then (IntSet.singleton v, neighbors_v) :: sc else sc
	 in
	   grow sc (IntSet.singleton v) neighbors_v)
      g
      []
;;
