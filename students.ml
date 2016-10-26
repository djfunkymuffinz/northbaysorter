module type STUDENT =
   sig
      type student

      val hasChap : student -> bool

      val name : student -> string

      val gender : student -> char

      val friends : student -> string list

      val cons : bool -> string -> char -> student

      val add_friend : student -> string -> student

      val add_friends : student -> string list -> student
   end

module StudentImpl =
struct

   (*
    * A student has four properties 
    * hasChap is a boolean that tells if a parent is in the cabin
    *    (dictates a cabin leader)
    * name is a string that has a unique name of a student
    * gender is a char either m or f showing the gender
    * friends is a string list ref showing the names of a students
    *    friends
    *)

   type student = { hasChap : bool; name : string; gender : char; friends : string list ref }
   
   let hasChap s = s.hasChap
   
   let name s = s.name
   
   let gender s = s.gender
   
   let friends s = !(s.friends)
   
   let cons hc n g = { hasChap = hc; name = n; gender = g; friends = ref [] }
   
   let add_friend s0 name = 
      s0.friends := name::!(s0.friends);
      s0
   
   let add_friends s0 name_list =
      s0.friends := name_list@(!(s0.friends));
      s0

end

module Student : STUDENT = StudentImpl
