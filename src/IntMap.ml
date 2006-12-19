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

(** The map is represented as big-endian patricia tree.  *)
type 'a t =
    Empty
  | Leaf of int * 'a
  | Branch of int * int * int * 'a t * 'a t
    (** [Branch (p, m, c, l, r)]:
        [p] is the largest common prefix for all the keys in this tree
        [m] is the branching bit mask
            ([m] is a power of 2, only the bits above [m] are valid in [p])
        [c] is the number of leaves of the tree (number of elements)
        [l] contains all the keys with a 0 in the branching bit
        [r] contains all the keys with a 1 in the branching bit  *)
;;

exception Already_present;;

let empty = Empty;;

let is_empty s = (s = Empty);;

let size = function
    Empty -> 0
  | Leaf _ -> 1
  | Branch (_, _, c, _, _) -> c
;;

let rec has_key s i =
  if i < 0 then invalid_arg "IntMap.has_key: negative key";
  match s with
      Empty -> false
    | Leaf (j, _) -> j = i
    | Branch (p, _, _, l, r) ->
	if i <= p
	then has_key l i
	else has_key r i
;;

let rec max_key s = match s with
    Empty -> raise Not_found
  | Leaf (i, _) -> i
  | Branch (_, _, _, _, r) -> max_key r
;;

let rec get s i =
  if i < 0 then invalid_arg "IntMap.get: negative key";
  match s with
  | Leaf (j, x) when j = i -> x
  | Branch (p, _, _, l, r) ->
      if i <= p
      then get l i
      else get r i
  | _  -> raise Not_found
;;

let rec get_default s i x =
  if i < 0 then invalid_arg "IntMap.get_default: negative key";
  match s with
      Empty -> x
    | Leaf (j, x) when j = i -> x
    | Leaf _ -> x
    | Branch (p, _, _, l, r) ->
	if i <= p
	then get_default l i x
	else get_default r i x
;;

let rec choose = function
  | Empty -> raise Not_found
  | Leaf (i, x) -> i, x
  | Branch (_, _, _, l, _) -> choose l
;;

(* Return an integer where only the highest bit that was set in [x] is
   still set.  *)
let rec highest_bit x =
  let x' = x land (x - 1) in
    if x' = 0
    then x
    else highest_bit x'
;;

let branching_bit m p1 p2 =
  let x = p1 lxor p2 in                 (* remove common prefix *)
  let x = x land lnot (m - 1) in        (* remove invalid suffix *)
    highest_bit x;;

(* In [i], clear the 1-bit mask [m], and set all bits below [m]'s bit to one.  *)
let mask i m = (i lor (m - 1 + m)) - m;;

let prefix_matches i p m = (mask i m = p);;

let join m p1 s1 p2 s2 c =
  let m = branching_bit m p1 p2 in
  let p = mask p1 m
  in
    if p1 < p2
    then Branch (p, m, c, s1, s2)
    else Branch (p, m, c, s2, s1)
;;
  
let branch p m l r = Branch (p, m, size l + size r, l, r);;

let rec set s i x =
  if i < 0 then invalid_arg "IntMap.set: negative key";
  match s with
      Empty -> Leaf (i, x)
    | Leaf (j, _) when j = i -> Leaf (i, x)
    | Leaf (j, _) -> join 1 i (Leaf (i, x)) j s 2
    | Branch (p, m, c, l, r) ->
	if prefix_matches i p m then
          if i <= p
          then branch p m (set l i x) r
          else branch p m l (set r i x)
	else
          join (m lsl 1) i (Leaf (i, x)) p s (c + 1)
;;

let rec add s i x =
  if i < 0 then invalid_arg "IntMap.add: negative key";
  match s with
      Empty -> Leaf (i, x)
    | Leaf (j, _) when j = i -> raise Already_present
    | Leaf (j, _) -> join 1 i (Leaf (i, x)) j s 2
    | Branch (p, m, c, l, r) ->
	if prefix_matches i p m then
          if i <= p
          then branch p m (add l i x) r
          else branch p m l (add r i x)
	else
          join (m lsl 1) i (Leaf (i, x)) p s (c + 1)
;;

let rec remove s i =
  let branch = function
    | (_, _, Empty, s) -> s
    | (_, _, s, Empty) -> s
    | (p, m, l, r) -> Branch (p, m, size l + size r, l, r)
  in
    match s with
	Leaf (j, _) when j = i -> Empty
      | Branch (p, m, c, l, r) ->
          if prefix_matches i p m then
            if i <= p
            then branch (p, m, remove l i, r)
            else branch (p, m, l, remove r i)
          else
            s
      | _ -> raise Not_found
;;

let rec update s i x =
  if i < 0 then invalid_arg "IntMap.set: negative key";
  match s with
    | Leaf (j, _) when j = i -> Leaf (i, x)
    | Branch (p, m, _, l, r) ->
        if i <= p
        then branch p m (update l i x) r
        else branch p m l (update r i x)
    | _ -> raise Not_found
;;

let rec modify f s i =
  if i < 0 then invalid_arg "IntMap.update: negative key";
  match s with
      Leaf (j, x) when j = i -> Leaf (i, f x)
    | Branch (p, m, _, l, r) ->
        if i <= p
        then branch p m (modify f l i) r
        else branch p m l (modify f r i)
    | _ -> raise Not_found
;;

let rec modify_default f s i x =
  if i < 0 then invalid_arg "IntMap.set: negative key";
  match s with
      Empty -> Leaf (i, x)
    | Leaf (j, y) when j = i -> Leaf (i, f y)
    | Leaf (j, _) -> join 1 i (Leaf (i, f x)) j s 2
    | Branch (p, m, c, l, r) ->
	if prefix_matches i p m then
          if i <= p
          then branch p m (modify_default f l i x) r
          else branch p m l (modify_default f r i x)
	else
          join (m lsl 1) i (Leaf (i, f x)) p s (c + 1)
;;

let rec fold f s accu = match s with
    Empty -> accu
  | Leaf (i, x) -> f accu i x
  | Branch (_, _, _, l, r) -> fold f r (fold f l accu)
;;

let iter f s = fold (fun () k x -> f k x) s ();;

let output channel p m =
  Printf.fprintf channel "{[%d] " (size m);
  ignore (fold
    (fun first i x ->
       if not first then Printf.fprintf channel ", ";
       Printf.fprintf channel "%d:%a" i p x;
       false)
    m
    true);
  Printf.fprintf channel "}";
;;
