#load "str.cma"

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

let csv_parse file =
   let lines = lines_to_array (read_file file) in
   List.map (fun m_l -> 
      let s = Student (String.trim (List.nth m_l 2)^" "^String.trim (List.nth m_l 1)) in
      let f1 = Student (String.trim (List.nth m_l 4)^" "^String.trim (List.nth m_l 3)) in
      let f2 = Student (String.trim (List.nth m_l 6)^" "^String.trim (List.nth m_l 5)) in
      let f3 = Student (String.trim (List.nth m_l 8)^" "^String.trim (List.nth m_l 7)) in
      (s,[f1;f2;f3])
   ) lines
