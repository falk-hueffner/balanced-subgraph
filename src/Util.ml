(* ulp -- solve the undirected labeling problem
   Copyright (C) 2006  Falk H�ffner

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

let is_whitespace = function
    ' ' | '\t' | '\r' | '\n' -> true
  | _ -> false
;;

let split_string s =
  let rec loop p word_start words =
    if p > String.length s then words
    else match word_start, p = String.length s || is_whitespace s.[p] with
        None,    true  -> loop (p + 1) None words
      | None,    false -> loop (p + 1) (Some p) words
      | Some p0, true  -> loop (p + 1) None ((String.sub s p0 (p - p0)) :: words)
      | Some _,  false -> loop (p + 1) word_start words
  in
    List.rev (loop 0 None [])
;;
