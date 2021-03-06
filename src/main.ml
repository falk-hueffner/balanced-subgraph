(* bsg -- solve the balanced subgraph problem
   Copyright (C) 2006  Falk H?ffner

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

let usage_msg = "Find maximum balanced subgraphs";;

let stats_only = ref false;;

let specs = [
  ("-c", Arg.Set_int(Util.max_cut_size),
         "Set maximum cut size for data reduction (0..4)");
  ("-d", Arg.Set(Util.downward_compress),
         "Start iterative compression with heuristic solution");
  ("-m", Arg.Set(Util.maxcut),
         "MaxCut mode (parse edge weights, print cut size in statistics)");
  ("-s", Arg.Set(stats_only),
         "Print statistics only");
  ("-v", Arg.Set(Util.verbose),
         "Print progress to stderr");
];;

let () =
  Arg.parse specs (fun _ -> Arg.usage specs usage_msg) usage_msg;
  if !Util.max_cut_size < 0 || !Util.max_cut_size > 4 then begin
    Printf.eprintf "maximum cut size must be 0..4\n";
    exit 1;
  end;
  let g, vertex_numbers, vertex_names = Bsg.input stdin in
  if !stats_only      
  then
    Printf.printf "%5d %6d %!" (ELGraph.num_vertices g) (Bsg.num_edges g);
  let start = Util.timer () in
  let colors = Solve.solve g in
  let stop = Util.timer () in
  let k = if not !Util.maxcut then Bsg.coloring_cost g colors else
    ELGraph.fold_edges
      (fun k i j { Bsg.eq = eq; Bsg.ne = ne } ->
	 if IntMap.get colors i <> IntMap.get colors j
	 then k + ne - eq else k)
      g 0
  in
    if !stats_only      
    then
      Printf.printf "%8d %10.2f %3d\n" k (stop -. start) !Util.max_unreducible_size
    else
      ELGraph.iter_edges
	(fun i j { Bsg.eq = eq; Bsg.ne = ne } ->
	   if IntMap.get colors i = IntMap.get colors j
	   then for l = 1 to ne do
	     Printf.printf "%s %s 1\n" (IntMap.get vertex_names i) (IntMap.get vertex_names j)
	   done
	   else for l = 1 to eq do
	     Printf.printf "%s %s 0\n" (IntMap.get vertex_names i) (IntMap.get vertex_names j)
	   done)
	g
;;
