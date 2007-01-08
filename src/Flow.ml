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

type flow_edge = {
  cap: int;
  flow: int;				(* for s < t, positive: s->t, negative: t->s  *)
}

type t = flow_edge ELGraph.t;;

let empty = ELGraph.empty;;

let new_vertex = ELGraph.new_vertex;;
let connect g i j cap =
  let g = ELGraph.set_vertex g i in
  let g = ELGraph.set_vertex g j in
    ELGraph.connect g i j {cap = cap; flow = 0}
;;
let disconnect = ELGraph.disconnect;;
let capacity g i j = (ELGraph.get_label g i j).cap;;

let augment g s t =
  let residual i j =
    let {cap = cap; flow = flow} = ELGraph.get_label g i j in
    let flow = if i < j then flow else -flow in
      cap - flow in
  let push g i j =
    ELGraph.modify_label
      (fun {cap = cap; flow = flow} ->
	 if i < j
	 then {cap = cap; flow = flow + 1}
	 else {cap = cap; flow = flow - 1}) g i j in
  let q = IntSet.fold Queue.push s Queue.empty in
    match
      let rec loop q preds =
	if Queue.is_empty q
	then preds, None
	else
          let i, q = Queue.pop q in
	  let rec scan neighbors q preds =
	    if IntMap.is_empty neighbors
	    then loop q preds
	    else
	      let j,  {cap = cap; flow = flow}, neighbors = IntMap.pop neighbors in
	      let flow = if i < j then flow else -flow in
		if cap - flow <= 0 || IntMap.has_key preds j
		then scan neighbors q preds
		else
		  let preds = IntMap.add preds j i in
		    if IntSet.contains t j
		    then preds, Some j
		    else
		      let q = Queue.push q j in
			scan neighbors q preds
	  in
	    scan (ELGraph.neighbors g i) q preds
      in
	loop q IntMap.empty
    with
	_, None -> g, false
      | preds, Some j ->
	  let rec loop g i j =
	    let g = push g i j in
	      if IntSet.contains s i
	      then g, true
	      else loop g (IntMap.get preds i) i
	  in
	    loop g (IntMap.get preds j) j
;;

let cut g s =
  let seen =
    let q = IntSet.fold Queue.push s Queue.empty in
    let rec loop q seen =
      if Queue.is_empty q
	then seen
      else
	let i, q = Queue.pop q in
	let rec scan neighbors q seen =
	  if IntMap.is_empty neighbors
	  then loop q seen
	  else
	    let j,  {cap = cap; flow = flow}, neighbors = IntMap.pop neighbors in
	    let flow = if i < j then flow else -flow in
	      if cap - flow <= 0 || IntSet.contains seen j
	      then scan neighbors q seen
	      else
		  let seen = IntSet.add seen j in
		  let q = Queue.push q j in
		    scan neighbors q seen
	  in
	    scan (ELGraph.neighbors g i) q seen
    in
      loop q s
  in
    ELGraph.fold_edges
      (fun cut i j _ -> if IntSet.contains seen i <> IntSet.contains seen j then (i, j) :: cut else cut)
      g []
;;

let output_edge channel e =
  Printf.fprintf channel "(%d/%d)" e.flow e.cap
;;

let output channel = ELGraph.output channel output_edge;;
