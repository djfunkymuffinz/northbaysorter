#use "nbs.ml";;

let s1 = Student.cons true "Matt" 'M';;
let s2 = Student.cons false "Steve" 'M';;
let s3 = Student.cons false "Nick" 'M';;
let s4 = Student.cons false "Nate" 'M';;
let s5 = Student.cons false "Andy" 'M';;

Student.add_friends s1 ["Steve";"Nick";"Nate"];;
Student.add_friends s2 ["Matt";"Nick";"Nate"];;
Student.add_friends s3 ["Matt";"Steve";"Nate"];;
Student.add_friends s4 ["Matt"];;

let s_list = s1::s2::s3::s4::[s5]
