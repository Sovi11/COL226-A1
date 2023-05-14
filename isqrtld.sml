fun int_to_char(a : int ) = 
if a=0 then #"0" 
else if a=1 then #"1"
else if a=2 then #"2"
else if a=3 then #"3"
else if a=4 then #"4"
else if a=5 then #"5"
else if a=6 then #"6"
else if a=7 then #"7"
else if a=8 then #"8"
else #"9"
 
fun char_to_int(a : char) = 
if a= #"0" then 0 
else if a= #"1" then 1 
else if a= #"2" then 2 
else if a= #"3" then 3 
else if a= #"4" then 4 
else if a= #"5" then 5 
else if a= #"6" then 6 
else if a= #"7" then 7 
else if a= #"8" then 8 
else 9

fun list_addition(l1 : char list , l2 : char list , carry : char) =
if null l1 andalso null l2 then carry::[]
else 
if null l1 andalso carry = #"0" then l2 
else 
if null l2 andalso carry = #"0" then l1 
else 
if null l1 then list_addition(l2 , [carry ] , #"0" ) 
else 
if null l2 then list_addition(l1, [carry] , #"0" )
else 
let val rr1 = char_to_int(hd(l1))
val rr2 = char_to_int(hd(l2))
val rr3 = char_to_int(carry)
val temp = rr1 + rr2 + rr3 
val carry2 =int_to_char(temp div 10) 
val temp2 = int_to_char(temp mod 10)
in
temp2::list_addition(tl(l1),tl(l2),carry2)
end  

fun list_multiplication(l1 : char, l2 : char list ) =
if l1 = #"0" then [#"0" ] 
else 
let val n1= char_to_int(l1) -1
val n2 = int_to_char(n1) 
in
list_addition(l2, list_multiplication(n2, l2),#"0")  
end

fun size( l) = 
if null l then 0 
else 
1 + size(tl(l))
 
fun subtract_list( l1 : char list , l2 : char list , sub : char ) =
if null l1 then sub::[]
else 
if null l2 andalso sub = #"0" then l1
else
if null l2 then subtract_list(l1, [sub] , #"0" )
else
let val tt1 = char_to_int(hd(l1))
val tt2 = char_to_int(hd(l2)) 
val tt3 = char_to_int(sub)
in
if (tt1 >= tt2 + tt3) 
then 
let val temp1 = int_to_char((tt1 - tt2) -tt3)
in 
temp1 :: subtract_list(tl(l1),tl(l2), #"0")
end
else
let val temp2 = int_to_char((((tt1 + 10) - tt2) - tt3))
in 
temp2 :: subtract_list(tl(l1),tl(l2),#"1")
end 
end

fun greater_than(l1 : char list , l2 : char list, preffered : bool )= 
if null l1 andalso null l2 then (preffered) 
else if null l2 andalso hd(l1) <> #"0" then true
else if null l2 then greater_than(tl(l1), l2 , preffered)   
else if null l1 andalso hd(l2) <> #"0" then false
else if null l1 then greater_than(l1 , tl(l2) , preffered)  
else 
let val nn1 = char_to_int (hd(l1))
val nn2 = char_to_int (hd(l2))
in
if (nn1 > nn2) then greater_than( tl(l1) , tl(l2) , true) 
else if (nn2 > nn1) then greater_than(tl(l1) , tl(l2) , false ) 
else greater_than(tl(l1) , tl(l2) ,preffered)
end 


fun gt(l1 ,l2)=
greater_than(l1,l2,false) ; 

fun lt(l1,l2) =
let val temp = not (greater_than(l1,l2,true))
in 
temp 
end  ;

fun eq(l1,l2)=
not (gt(l1,l2) orelse lt(l1,l2)) ;


fun konsa_x(l1 : char list , l2 : char list , n : char )= 
if gt(list_multiplication(n, n::l1), l2) then 
let val temp1 = int_to_char(char_to_int(n) - 1)
in
konsa_x(l1, l2, temp1)
end
else
n

fun ldpd(l1,l2)=
konsa_x(l1,l2,#"9")


fun ind_ex(l1,i)=
if i=0 then hd(l1)
else ind_ex(tl(l1),i-1) 

fun rem_lead_0_ff(l : char list)=
if null l then l
else if ((hd(l) = #"0") andalso (null (tl(l)))) then l
else if hd(l) <> #"0" then l 
else 
rem_lead_0_ff(tl(l))


fun final_wl(l1 : char list , runn_quo : char list , rem : char list, i :int)=
let val temp1  = list_multiplication(#"2", runn_quo)
val new_rem = ind_ex(l1,i+1)::ind_ex(l1,i)::rem
val temp2 = ldpd(temp1, new_rem)
val new_quo = temp2::runn_quo
val temp3 = list_multiplication(temp2,temp2::temp1)
val temp4 = subtract_list(new_rem,temp3 , #"0") 
val n = size(l1)  
in
if i=n-2 then (new_quo, temp4)
else final_wl(l1, new_quo, temp4 , i+2) 
end 


fun new_final_wl(l1)=
final_wl(l1, [], [] ,0)  


fun final_function(s : string)= 
let val lmao  = String.explode s ;
in
if ((size (lmao)) mod 2 = 0) then new_final_wl(lmao)  
else 
let val lmao2 = #"0" :: lmao 
in 
new_final_wl(lmao2) 
end 
end

fun isqrtld(s)=
let val final_val = final_function(s) 
val nice = rem_lead_0_ff(rev ( #1 final_val ))
val nice2 = rem_lead_0_ff(rev( #2 final_val ))   
val vokay = String.implode nice
val vokay2 = String.implode nice2  
in 
(vokay, vokay2)
end 
