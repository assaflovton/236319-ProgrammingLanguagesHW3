datatype 'a HashTableEntry = Empty
    | Deleted
    | Value of int * 'a;

type 'a HashTable = 'a HashTableEntry list;
fun create 0=[] | create n = Empty::create(n-1);

local
fun get_key_f Deleted = 0|get_key_f Empty = 0 | get_key_f (Value(k,v))= k;
fun is_right_key ( [] ,i , (k,v) ) = true | is_right_key ( h::hs  ,i , (k,v) ) = 
    if (i=0) then ( 
        if (h=Empty) orelse (h=Deleted) then true 
        else if (get_key_f(h) = i) then true  else false )
    else is_right_key ( (hs) , (i-1) , (k,v));
    
fun update_hash ([] , i , (k,v) ) = nil| update_hash ((h::hs) , i , (k,v) ) = 
    if (i=0) then Value((k,v))::hs
    else h::update_hash( hs, (i-1), (k,v) );
    
fun calculate_key_until_inseret ( list, hash_func, cur_key, (k,v)) = 
if is_right_key (list, cur_key, (k,v) ) then update_hash(list, cur_key, (k,v)) 
else calculate_key_until_inseret((list), (hash_func), (hash_func(cur_key)), (k,v));

in
fun insert hash_func [] (k,v) = nil | insert hash_func (h::hs) (k,v) =
update_hash( h::hs , hash_func(k), (k,v) );
end;

local 
fun get_tuple (Value(k,v))=(k,v);
fun get_key_f Deleted = 0|get_key_f Empty = 0 | get_key_f (Value(k,v))= k;
fun is_identical_key ( []  ,i , (k) )= true | is_identical_key ( h::hs  ,i , (k) ) = 
    if (i=0) then 
        if get_key_f(h) = k then true else false
    else is_identical_key (hs, (i-1) , (k));

fun get_arg_by_key ( []  ,i , (k) ) = Empty | get_arg_by_key ( h::hs  ,i , (k) ) = 
    if(i=0) then h else get_arg_by_key(hs, (i-1), (k) );

fun calculate_key_until_getting ( list,  hash_func, cur_key, k ) = 
    if is_identical_key( list , cur_key , (k) ) then get_arg_by_key (list, cur_key, (k))
    else calculate_key_until_getting((list) ,(hash_func),(hash_func(cur_key)), (k));
    
in
fun get hash_func (list) (k) = (get_tuple(calculate_key_until_getting ((list) ,
(hash_func), (hash_func(k)), (k) )));
end;


local
fun get_key_f Deleted = 0|get_key_f Empty = 0 | get_key_f (Value(k,v))= k;
fun is_right_key ( [] ,i , (k) ) = true | is_right_key ( h::hs  ,i , (k) ) = 
    if (i=0) then ( 
        if (h=Empty) orelse (h=Deleted) then true 
        else if (get_key_f(h) = i) then true  else false )
    else is_right_key ( (hs) , (i-1) , (k));
    
fun update_hash ([] , i , (k) ) = nil| update_hash ((h::hs) , i , (k) ) = 
    if (i=0) then Deleted::hs
    else h::update_hash( hs, (i-1), (k));
    
fun calculate_key_until_inseret ( list, hash_func, cur_key, (k)) = 
if is_right_key (list, cur_key, (k) ) then update_hash(list, cur_key, (k)) 
else calculate_key_until_inseret((list), (hash_func), (hash_func(cur_key)), (k));

in
fun remove hash_func (ht) (k) =update_hash(ht , hash_func( k:int ), k );
end;

