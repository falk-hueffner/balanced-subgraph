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

(* Horribly slow, but simple implementation.  *)

type t = int IntMap.t;;

let make s =
  IntSet.fold
    (fun m i -> IntMap.add m i i)
    s
    IntMap.empty
;;

let join m i j =
  let old_col = IntMap.get m i in
  let new_col = IntMap.get m j in
    IntMap.fold
      (fun m i ci -> if ci = old_col then IntMap.update m i new_col else m)
      m
      m
;;

let classes m =
  let classes =
    IntMap.fold
      (fun classes i ci ->
	 let cl = IntMap.get_default classes ci IntSet.empty in
	 let cl = IntSet.add cl i in
	   IntMap.set classes ci cl)
      m
      IntMap.empty
  in
    IntMap.fold
      (fun list i set -> set::list)
      classes
      []
;;
