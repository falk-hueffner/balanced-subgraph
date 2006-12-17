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

(** The set is represented as big-endian patricia tree.	 *)
type t =
    Empty
  | Leaf of int
  | Branch of int * int * int * t * t
    (* Branch (p, m, c, l, r):
       [p] is the largest common prefix for all the keys in this tree
       [m] is the branching bit mask
	   ([m] is a power of 2, only the bits above [m] are valid in [p])
       [c] is the number of leaves of the tree (number of elements)
       [l] contains all the keys with a 0 in the branching bit
       [r] contains all the keys with a 1 in the branching bit  *)
;;

exception Already_present;;

let empty = Empty;;

let is_empty s = s = Empty;;

let size = function
    Empty -> 0
  | Leaf _ -> 1
  | Branch (_, _, c, _, _) -> c
;;

let rec contains s i = match s with
    Empty -> false
  | Leaf j -> j = i
  | Branch (p, _, _, l, r) ->
      if i <= p
      then contains l i
      else contains r i
;;

let rec choose = function
  | Empty -> raise Not_found
  | Leaf i -> i
  | Branch (_, _, _, l,_) -> choose l
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
  let x = p1 lxor p2 in			(* remove common prefix *)
  let x = x land lnot (m - 1) in	(* remove invalid suffix *)
    highest_bit x;;

(* In [i], clear the 1-bit mask [m], and set all bits below [m]'s bit to one.  *)
let mask i m = (i lor (m - 1 + m)) - m;;

let prefix_matches i p m = (mask i m = p)

(* Combine two trees with prefixes P1 and P2, where P1 and P2 are
   known to disagree.  *)
let join m p1 s1 p2 s2 c =
  let m = branching_bit m p1 p2 in
  let p = mask p1 m
  in
    if p1 < p2
    then Branch (p, m, c, s1, s2)
    else Branch (p, m, c, s2, s1)
;;

let rec add s i = match s with
    Empty -> Leaf i
  | Leaf j when j = i -> raise Already_present
  | Leaf j -> join 1 i (Leaf i) j s 2
  | Branch (p, m, c, l, r) ->
      if prefix_matches i p m then
	if i <= p
	then Branch (p, m, c + 1, (add l i), r)
	else Branch (p, m, c + 1, l, (add r i))
      else
	join (m lsl 1) i (Leaf i) p s (c + 1)
;;

let put s i = try add s i with Already_present -> s;;

let rec delete s i =
  let branch = function
    | (_, _, _, Empty, s) -> s
    | (_, _, _, s, Empty) -> s
    | (p, m, c, l, r) -> Branch (p, m, c, l, r)
  in
    match s with
	Leaf j when j = i -> Empty
      | Branch (p, m, c, l, r) ->
	  if i <= p
	  then branch (p, m, c - 1, (delete l i), r)
	  else branch (p, m, c - 1, l, (delete r i))
      | _ -> raise Not_found
;;

let remove s i = try delete s i with Not_found -> s;;

let rec fold f s accu = match s with
    Empty -> accu
  | Leaf i -> f accu i
  | Branch (_, _, _, l, r) -> fold f r (fold f l accu)
;;

let iter f s = fold (fun () i -> f i) s ();;

let branch p m l r = Branch (p, m, size l + size r, l, r);;

let rec union s1 s2 = match s1, s2 with
    Empty, t  -> t
  | t, Empty  -> t
  | Leaf k, t -> add t k
  | t, Leaf k -> add t k
  | Branch (p1, m1, c1, l1, r1), Branch (p2, m2, c2, l2, r2) ->
      if m1 = m2 && p1 = p2 then
	(* The trees have the same prefix. Merge the subtrees.  *)
	branch p1 m1 (union l1 l2) (union r1 r2)
      else if m1 > m2 && prefix_matches p2 p1 m1 then
	(* [p2] contains [p1]. Merge [s2] with a subtree of [s1].  *)
	if p2 <= p1
	then branch p1 m1 (union l1 s2) r1
	else branch p1 m1 l1 (union r1 s2)
      else if m1 < m2 && prefix_matches p1 p2 m2 then
	(* [p1] contains [p2]. Merge [s1] with a subtree of [s2].  *)
	if p1 <= p2
	then branch p2 m2 (union s1 l2) r2
	else branch p2 m2 l2 (union s1 r2)
      else
	(* The prefixes disagree.  *)
	join (m1 lsl 1) p1 s1 p2 s2 (c1 + c2)
;;

let rec minus s1 s2 = match s1, s2 with
    Empty, _ -> Empty
  | _, Empty -> s1
  | Leaf i, _ -> if contains s2 i then Empty else s1
  | _, Leaf i -> remove s1 i
  | Branch (p1, m1, _, l1, r1), Branch (p2, m2, _, l2, r2) ->
      if m1 = m2 && p1 = p2 then
	union (minus l1 l2) (minus r1 r2)
      else if m1 > m2 && prefix_matches p2 p1 m1 then
	if p2 <= p1
	then union (minus l1 s2) r1
	else union l1 (minus r1 s2)
      else if m1 < m2 && prefix_matches p1 p2 m2 then
	if p1 <= p2
	then minus s1 l2
	else minus s1 r2
      else
	s1
;;

let rec intersection s1 s2 = match s1, s2 with
    Empty, _
  | _, Empty -> Empty
  | Leaf i, _ -> if contains s2 i then s1 else Empty
  | _, Leaf i -> if contains s1 i then s2 else Empty
  | Branch (p1, m1, _, l1, r1), Branch (p2, m2, _, l2, r2) ->
      if m1 = m2 && p1 = p2 then
        union (intersection l1 l2) (intersection r1 r2)
      else if m1 > m2 && prefix_matches p2 p1 m1 then
        intersection (if p2 <= p1 then l1 else r1) s2
      else if m1 < m2 && prefix_matches p1 p2 m2 then
        intersection s1 (if p1 <= p2 then l2 else r2)
      else
        Empty
;;

let output channel s =
  Printf.fprintf channel "{[%d] " (size s);
  ignore (fold
    (fun first i ->
       if not first then Printf.fprintf channel ", ";
       Printf.fprintf channel "%d" i;
       false)
    s
    true);
  Printf.fprintf channel "}";
;;
