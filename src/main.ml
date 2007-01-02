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

let () =
  let g = Ulp.input stdin in
(*   Ulp.output stdout g; *)
  let edges = Ulp.solve g in
    List.iter
      (fun (i, j, sign) ->
	 let { Ulp.eq = eq; Ulp.ne = ne } = ELGraph.get_label g i j in
	   for l = 1 to (if sign = Ulp.Eq then eq else ne) do
	     Printf.printf "%d %d %d\n" i j (if sign = Ulp.Eq then 0 else 1)
	   done)
      edges
;;
