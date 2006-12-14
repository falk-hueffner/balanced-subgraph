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

type 'a t = 'a list * 'a list;;

exception Empty

let empty = [], [];;
let make x = [x], [];;
  
let is_empty (f, r) = f = [];;

let checkf (f, r as q) = if f = [] then List.rev r, f else q

let push (f, r) x = checkf (f, x :: r);;

let pop = function
    [], _ -> raise Empty
  | x :: f, r -> x, checkf (f, r)
;;

