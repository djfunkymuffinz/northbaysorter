#use "csv_parse.ml"
#use "students.ml"

let csv_to_students file_name =
   let str_list = lines_to_array (read_file file_name) in
   let s_list = List.map (fun m_str -> (
      let name = List.nth m_str 0 in
      let gender = String.get (List.nth m_str 1) 0 in
      let chap = ((List.nth m_str 5) = "y") in
      let f1 = (List.nth m_str 2) in
      let f2 = (List.nth m_str 3) in
      let f3 = (List.nth m_str 4) in
      let std = Student.cons chap name gender in
      Student.add_friends std (f1::f2::[f3])
   )) (List.tl str_list) in
   s_list
