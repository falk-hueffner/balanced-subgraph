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

let verbose = ref false;;
let max_cut_size = ref 4;;
let max_unreducible_size = ref 0;;
let downward_compress = ref false;;

let timer () =
  let stamp = Unix.times () in
    stamp.Unix.tms_utime;;

let fold_n f n accu =
  let rec loop accu i =
    if i >= n
    then accu
    else loop (f accu i) (succ i)
  in
    loop accu 0
;;

let rec list_contains l x =
  match l with
      [] -> false
    | y :: r when x = y -> true
    | y :: r -> list_contains r x
;;

let output_int channel i = Printf.fprintf channel "%d" i;;
let output_bool channel b = output_char channel (if b then '1' else '0');;

let output_list p channel l =
  Printf.fprintf channel "[(%d) " (List.length l);
  ignore (List.fold_left
    (fun first x ->
       if not first then Printf.fprintf channel ", ";
       Printf.fprintf channel "%a" p x;
       false)
    true
    l);
  Printf.fprintf channel "]";
;;

let output_array p channel l =
  Printf.fprintf channel "(%d)[|" (Array.length l);
  ignore (Array.fold_left
    (fun first x ->
       if not first then Printf.fprintf channel "; ";
       Printf.fprintf channel "%a" p x;
       false)
    true
    l);
  Printf.fprintf channel "|]";
;;


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
