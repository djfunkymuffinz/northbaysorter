#load "str.cma"
open Printf

type student =
   |  None
   |  Student of string
   |  Leader of string

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
      let s = Student (String.lowercase ((Str.global_replace reg "" (String.trim (List.nth m_l 2)))^" "^(Str.global_replace reg "" (String.trim (List.nth m_l 1))))) in
      let f1 = Student (String.lowercase ((Str.global_replace reg "" (String.trim (List.nth m_l 4)))^" "^(Str.global_replace reg "" (String.trim (List.nth m_l 3))))) in
      let f2 = Student (String.lowercase ((Str.global_replace reg "" (String.trim (List.nth m_l 6)))^" "^(Str.global_replace reg "" (String.trim (List.nth m_l 5))))) in
      let f3 = Student (String.lowercase ((Str.global_replace reg "" (String.trim (List.nth m_l 8)))^" "^(Str.global_replace reg "" (String.trim (List.nth m_l 7))))) in
      (s,[f1;f2;f3])
   ) lines

let make_graph file =
   let edges = make_edges file in
   (List.map (fun (a,b) -> a) edges, edges)

let get_name student = 
   match student with
      |  None -> ""
      |  Student s -> s
      |  Leader l -> l

let g_to_json g =
   let (v,e) = g in
   let str = List.fold_left (fun acc f -> acc^"{\"id\": \""^(get_name f)^"\", \"group\": 1},\n") "{\n\"nodes\": [\n" v in
   let str = (String.sub str 0 ((String.length str) - 2))^"\n],\n\"links\": [\n" in
   let str = List.fold_left (
      fun acc (a,b) -> 
         acc^(List.fold_left (fun acc s -> acc^"{\"source\": \""^(get_name a)^"\", \"target\": \""^(get_name s)^"\", \"value\": 1},\n") "" b)
   ) str e in
   let str = (String.sub str 0 ((String.length str) - 2))^"\n]\n}" in
   let oc = open_out "graph.json" in
   fprintf oc "%s\n" str;
   close_out oc
