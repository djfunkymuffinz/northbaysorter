#load "str.cma"
open Printf

type student =
   |  None
   |  Student of string
   |  Leader of string

let student_to_string s =
   match s with
   |  Student s -> s
   |  Leader l -> l
   |  _ -> ""

let rec student_list_to_string lst =
   let str_lst = List.rev (List.fold_left (fun acc s -> (student_to_string s)::acc) [] lst )in
   let str = String.concat ", " str_lst in
   String.concat "" ["[ ";str;" ]"]

let read_file filename =
   let lines = ref [] in
   let chan = open_in filename in
   try
      while true; do
         lines := input_line chan :: !lines
      done; !lines
   with End_of_file ->
      close_in chan;
      List.rev !lines

let lines_to_array lines =
   let reg_comma = Str.regexp "," in
   List.map (fun m_line -> List.tl (Str.split reg_comma m_line)) lines

let make_edges file =
   let lines = lines_to_array (read_file file) in
   List.map (fun m_l -> 
      let reg = Str.regexp "\"" in
      let s = Student (String.lowercase ((Str.global_replace reg "" (String.trim (List.nth m_l 3)))^" "^(Str.global_replace reg "" (String.trim (List.nth m_l 2))))) in
      let f1 = Student (String.lowercase ((Str.global_replace reg "" (String.trim (List.nth m_l 5)))^" "^(Str.global_replace reg "" (String.trim (List.nth m_l 4))))) in
      let f2 = Student (String.lowercase ((Str.global_replace reg "" (String.trim (List.nth m_l 7)))^" "^(Str.global_replace reg "" (String.trim (List.nth m_l 6))))) in
      let f3 = Student (String.lowercase ((Str.global_replace reg "" (String.trim (List.nth m_l 9)))^" "^(Str.global_replace reg "" (String.trim (List.nth m_l 8))))) in
      (s,[f1;f2;f3])
   ) lines

let make_graph file =
   let edges = make_edges file in
   (List.map (fun (a,b) -> a) edges, edges)

let get_friends student g =
   let (v,e) = g in
   let (s,f) = List.find (fun (s,f) -> s = student) e in
   f

let eval_cabin cabin g =
   List.fold_left (
      fun acc s -> 
         acc && (
            List.fold_left (
               fun acc f -> (
                  acc || (List.mem f cabin)
               )
            ) false (get_friends s g)
         )
   ) true cabin

let rec make_loop loop g =
   if (eval_cabin loop g) then
      loop
   else (
      let friends = get_friends (List.hd loop) g in
      let a = print_string (student_list_to_string friends) in
      let a = print_string "\n" in
      let friends_in_loop = List.map (
         fun f ->
            (f, (List.filter (fun f -> List.mem f loop) (get_friends f g)))
      ) friends in
      let friends_in_loop = List.filter (fun (s,f) -> not(f = [])) friends_in_loop in
      let a = List.iter (fun (s,f) ->
         let a = print_string (student_to_string s) in
         let a = print_string (student_list_to_string f) in
         let a = print_string "\n" in
         a
      ) friends_in_loop in
      if friends_in_loop = [] then
         make_loop ((List.hd friends)::loop) g
      else (
         let (s,_) = List.hd friends_in_loop in
         make_loop (s::loop) g
      )
   )

let rec fill_cabin loop size g =
   let (v,_) = g in
   if (List.length loop) < size then (
      let valid_students = List.filter (fun s -> (eval_cabin (s::loop) g) && not(List.mem s loop)) v in
      try fill_cabin ((List.hd valid_students)::loop) size g with
      |  hd ->
            let valid_students = List.tl valid_students in
            fill_cabin ((List.hd valid_students)::loop) size g
   ) else
      loop
