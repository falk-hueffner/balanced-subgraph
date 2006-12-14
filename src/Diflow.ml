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

type t = (int * int) ELDigraph.t;;	(* capacity, flow *)

let make g cap = ELDigraph.make g (fun i j -> (cap i j), 0);;

let augment g s t =
  let preds =
    let rec loop q preds =
      if Queue.is_empty q
      then preds
      else
	let i, q = Queue.pop q in
	let p, s = ELDigraph.neighbors g i in
	let q, preds =
	  IntMap.fold
	    (fun (q, preds) j (cap, flow) ->
	       if not (IntMap.has_key preds j) && flow < cap
	       then Queue.push q j, IntMap.add preds j i
	       else q, preds)
	    s
	    (q, preds) in
	let q, preds =
	  IntMap.fold
	    (fun (q, preds) j (cap, flow) ->
	       if not (IntMap.has_key preds j) && flow > 0
	       then Queue.push q j, IntMap.add preds j i
	       else q, preds)
	    p
	    (q, preds) in
	  if IntMap.has_key preds t
	  then preds
	  else loop q preds
    in
      loop (Queue.make s) IntMap.empty
  in
    if not (IntMap.has_key preds t)
    then g
    else
      let rec loop i g =
	if i = s
	then g
	else
	  let p = IntMap.get preds i in
	    if ELDigraph.has_arc g p i
	    then
	      let cap, flow = ELDigraph.get_label g p i
	      in
		assert (flow + 1 <= cap);
		loop p (ELDigraph.relabel g p i (cap, flow + 1))
	    else
	      let cap, flow = ELDigraph.get_label g i p
	      in
		assert (flow > 0);
		loop p (ELDigraph.relabel g i p (cap, flow - 1))
      in
	loop t g
;;

let output channel g =
  ELDigraph.output channel (fun channel (cap, flow) -> Printf.fprintf channel "(%d/%d)" flow cap) g
;;
