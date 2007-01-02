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

let () =
  Arg.parse specs (fun _ -> Arg.usage specs usage_msg) usage_msg;
  let g = Ulp.input stdin in
(*   Ulp.output stdout g; *)
  let start = Util.timer () in
  let edges = Ulp.solve g in
  let stop = Util.timer () in
  let k = List.fold_left
    (fun k (i, j, sign) ->
       let { Ulp.eq = eq; Ulp.ne = ne } = ELGraph.get_label g i j in
	 if sign = Ulp.Eq then k + eq else k + ne)
    0 edges
  in
    if !stats_only      
    then
      Printf.printf "%4d %5d %4d %8.2f\n"
	(ELGraph.num_vertices g) (ELGraph.num_edges g) k (stop -. start)
    else
      List.iter
	(fun (i, j, sign) ->
	   let { Ulp.eq = eq; Ulp.ne = ne } = ELGraph.get_label g i j in
	     for l = 1 to (if sign = Ulp.Eq then eq else ne) do
	       Printf.printf "%d %d %d\n" i j (if sign = Ulp.Eq then 0 else 1)
	     done)
	edges
;;
