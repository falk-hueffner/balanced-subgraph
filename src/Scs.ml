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

module StringMap = Map.Make(String);;

type edge = {
  eq: int;
  ne: int;
};;

type t = edge ELGraph.t;;

let num_edges g = ELGraph.fold_edges (fun m _ _ { eq = eq; ne = ne } -> m + eq + ne) g 0;;

let strip_comment s =
  if String.contains s '#' then String.sub s 0 (String.index s '#') else s
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

let input channel =
  let rec loop lines lineno =
    try
      let line = input_line channel in
      if line = "# Graph Name\013" then begin
	let l = ref "" in
	  while !l <> "# Edges\013" do
	    l := input_line channel;
	  done;
      end;
      let line = strip_comment line in
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
	   if not (is_int v) && not (StringMap.mem v vertex_numbers)
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

exception Not_sign_consistent;;

let color g =
  let rec dfs v color colors =
    match IntMap.get_opt colors v with
	Some c -> if c = color then colors else raise Not_sign_consistent
      | None ->
	  let colors = IntMap.set colors v color in
	    ELGraph.fold_neighbors
	      (fun colors w e ->
		 if e.eq > 0 && e.ne > 0
		 then raise Not_sign_consistent
		 else if e.eq > 0
		 then dfs w color colors
		 else dfs w (not color) colors)
	      g v colors
  in
    ELGraph.fold_vertices
      (fun colors v _ ->
	 if IntMap.has_key colors v then colors
	 else dfs v false colors)
      g
      IntMap.empty
;;

let coloring_cost g colors =
  ELGraph.fold_edges
    (fun k i j { eq = eq; ne = ne } ->
       if IntMap.get colors i = IntMap.get colors j
       then k + ne
       else k + eq)
    g 0
;;

let is_sign_consistent g =
  try
    ignore (color g);
    true
  with
      Not_sign_consistent -> false
;;
