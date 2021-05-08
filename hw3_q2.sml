fun to_binary 0 = [] |
to_binary n= (n mod 2)::(to_binary (n div 2));

local 
    fun num_of_ones [] = 0 | num_of_ones (x::xs)= if(x=1) then 1+num_of_ones(xs) else num_of_ones(xs);
    fun num_of_zeros [] = 0 | num_of_zeros (x::xs)=if(x=0) then 1+num_of_zeros(xs) else num_of_zeros(xs);
    fun get_value ([],index) = 2|
    get_value ((x::xs),index) = if(index=0) then x else get_value((xs),index-1);
    fun change_list ([],index) = []|
    change_list ((x::xs),index) = if(index=0) then (if(x=1) then (0::xs) else (1::xs)) else x::change_list((xs),index-1);
    fun encode_aux ([],index)=[] | 
    encode_aux ((x::xs),index) = if(num_of_ones(x::xs) = num_of_zeros(x::xs)) then ((x::xs)@to_binary (index))
    else if(get_value((x::xs),index)=1) then encode_aux(change_list((x::xs),index),(index+1)) 
    else encode_aux(change_list((x::xs),index),index+1);
in 
    fun encode list = encode_aux (list,0)
end;


local
    fun from_binary [] = 0 |
    from_binary (x::xs)= x+2*from_binary(xs);
    fun get_number ([],index)=0 | 
    get_number ((x::xs),index) = if(index=0) then (from_binary (x::xs)) else get_number ((xs),index-1);
    fun change_list ([],index)=[] | change_list ((x::xs),0)=[]|
    change_list ((x::xs),index)= if(x=1) then 0::change_list(xs,index-1) else 1::change_list(xs,index-1);
    fun get_from_list([],index)=[]|get_from_list((x::xs),0)=[]|
    get_from_list ((x::xs),index)= x::get_from_list(xs,index-1);
    fun second_list ([],i,j,index)= []|
    second_list ((x::xs),i,j,index)= if(index=i) then get_from_list((x::xs),j-i) else second_list ((xs),i,j,index+1);
in
    fun decode (list,index) = (change_list(list,(get_number (list,index))) @ (second_list(list,(get_number (list,index)),index,0)));
end;

