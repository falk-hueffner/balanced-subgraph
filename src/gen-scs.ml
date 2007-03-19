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

let () =
  if Array.length Sys.argv < 4 || Array.length Sys.argv > 5
  then begin
    Printf.eprintf "usage: %s vertices edges k [seed]\n" Sys.argv.(0);
    exit 1;
  end;
  let n = int_of_string Sys.argv.(1) in
  let m = int_of_string Sys.argv.(2) in
  let k = int_of_string Sys.argv.(3) in
  if Array.length Sys.argv = 5
  then Random.init (int_of_string Sys.argv.(4))
  else Random.self_init ();
  let g = Util.fold_n (fun g i -> ELGraph.add_vertex g i) n ELGraph.empty in
  let c = Util.fold_n (fun c i -> IntMap.add c i (Random.bool ())) n IntMap.empty in
  let g =
    let rec loop g m =
      if m = 0 then g
      else
	let i = Random.int n and j = Random.int n in
	  if i = j then loop g m
	  else
	    let g =
	      ELGraph.modify_label_default
		(fun label ->
		   if IntMap.get c i = IntMap.get c j
		   then { label with Bsg.eq = label.Scs.eq + 1}
		   else { label with Bsg.ne = label.Scs.ne + 1})
		g i j { Bsg.eq = 0; Scs.ne = 0 }
	    in
	      loop g (m - 1)
    in
      loop g m in
  let g =
    let rec loop g k =
      if k = 0 then g
      else
	let i = Random.int n and j = Random.int n in
	  if i = j then loop g k
	  else
	    let { Bsg.eq = eq; Scs.ne = ne } =
	      ELGraph.get_label_default g i j { Bsg.eq = 0; Scs.ne = 0 }
	    in
	      if IntMap.get c i = IntMap.get c j
	      then
		if eq > 0
		then loop (ELGraph.set_label g i j { Bsg.eq = eq - 1; Scs.ne = ne + 1}) (k - 1)
		else loop g k
	      else
		if ne > 0
		then loop (ELGraph.set_label g i j { Bsg.eq = eq + 1; Scs.ne = ne - 1}) (k - 1)
		else loop g k
    in
      loop g k
  in
    ELGraph.iter_edges
      (fun i j { Bsg.eq = eq; Scs.ne = ne } ->
	 for l = 1 to eq do
	   Printf.printf "%3d %3d  0\n" i j
	 done;
	 for l = 1 to ne do
	   Printf.printf "%3d %3d  1\n" i j
	 done)
      g
;;
