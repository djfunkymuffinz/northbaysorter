#load "str.cma"

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
