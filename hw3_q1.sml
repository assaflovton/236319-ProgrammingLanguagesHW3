datatype ('a,'b) hetrolist = nil | ::: of 'a * ('b,'a) hetrolist;
infixr 5 ::: ;


fun build4 (x,one,y,two) = x:::one:::y:::two:::nil;

local
fun unzip_first_aux (x:::y:::nil) = [x]| unzip_first_aux (x:::nil) = [x]| unzip_first_aux (nil) = []|
unzip_first_aux (x:::y:::xs)=x :: (unzip_first_aux xs);

fun unzip_second_aux (x:::y:::nil) = [y]| unzip_second_aux (x:::nil) = [] | unzip_second_aux (nil) = []|
unzip_second_aux (x:::y:::xs)=y :: (unzip_second_aux xs);
in
fun unzip  h = (unzip_first_aux h,unzip_second_aux h);
end;
fun zip ([],[]) = nil | zip ([],_) = raise Empty |  zip (_,[]) = raise Empty|
zip ( l1 , l2 ) = hd(l1):::hd(l2)::: zip(tl(l1),tl(l2));
