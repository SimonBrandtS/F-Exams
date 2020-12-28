(*I hereby declare that I myself have created this exam hand-in in its entirety without help from
anybody else.



Simon Brandt Sørensen
sims@itu.dk

Re-exam Functional Programming August 2020

*)


(*Question 1*)


type item = {Id: int;
             Name: string;
             Price: float}
type pizzareg = item list

//1.1

//1.1.1

//Below are declared 4 items with the id, name, and price as is specified by schema
let item1 = {Id = 1; Name = "Vesuvio"; Price = 55.0};;
let item2 = {Id = 2; Name = "Pepperoni"; Price = 50.0};;
let item3 = {Id = 3; Name = "Italiana"; Price = 59.0};;
let item4 = {Id = 4; Name = "Capricossa"; Price = 62.0};;

//1.1.2:

let reg = [item1;item2;item3;item4];; //item list containing the 4 items above

//1.1.3:

let regDub = [item1;item2;item3;item4; {Id = 4; Name = "Norrebro Special"; Price = 62.0}];; //contains id=4 twice


//1.1.4:

let emptyReg() = [];;

//1.1.5:

let size (r:pizzareg) = List.length r //using List.length to find length of r

size reg //val it : int = 4




//1.2

//1.2.1
let isEmpty (r:pizzareg) = List.isEmpty r;; //using List.isEmpty to see if list is empty

isEmpty reg //val it : bool = false


//1.2.2
let pId i item = 
    match item with
    |{Id = x; Name = _; Price = _} when i = x -> true //compare Id of item with i, and return true if they are equal
    |_                                         -> false;; //else, return false

pId 2 item1;; //val it : bool = false

pId 2 item2;; //val it : bool = true

let pName n item = 
    match item with
    |{Id = _; Name = x; Price = _} when n = x -> true //compare Name of item with i, and return true if they are equal
    |_                                         -> false;;//else, return false

pName "Pepperoni" item1;; //val it : bool = false

pName "Pepperoni" item2;;// val it : bool = true

//1.2.3:

let tryFind p (r:pizzareg) = 
    match List.tryFind p r with //try to find p in r, and match thhe result with
    |None -> None               // if there's nothing, return none
    |Some item -> Some item;;     //If we manage to find some item, return said item


tryFind (pName "Pepperoni") reg;; //Returns {ID=2;Name="Pepperoni";Price=50.0}

tryFind (pId 30) reg;; //Returns none

//1.2.4:


let rec isUniqueById (r:pizzareg) = 
       match r with //match r with
       |[] -> true //if we find nothing when the list is empty then return true
       |{Id=x;Name = _; Price = _}::r-> match (tryFind (pId x) r) with //else, take out the first element, and match on id with tryFind
                                                |None -> isUniqueById r //if nothing is returned, then call isUniqueById with r without first element
                                                |Some item -> false;; //if something matches, return false

isUniqueById reg //true

isUniqueById regDub //false


//1.3

type order = (int * int) list;;



let order1:order = [(2,3);(3,4)];;

let order2:order = [(2,3);(1,2);(1,3);(2,4)];;

let  collectById (o:order) =
    let m = Map.empty //create an empty map
    let rec inner o m = 
        match o with //match order with
        |[] -> m //if it's empty, return m
        |(k,z) :: o -> match Map.tryFind k m with //else, take the first tuple out of o, and try to find k in map m
                       |None -> inner o (Map.add k z m) //if it doesn't exist, add key and value to map
                       |Some v -> inner o (Map.add k (z+v) m) // else, update with the sum of the two values
    let (o2:order) = Map.toList(inner o m) //convert the map returned from calling inner to a list, and cast it to an order
    o2;;

collectById order1 //val it : order = [(2, 3); (3, 4)]

collectById order2 //val it : order = [(1, 5); (2, 7)]



//=================================================================================================================

(*Question 2*)

let rec f i = function
    [] -> []
  | x::xs -> (x+i) :: g (i+1) xs
  | x1::x2::xs -> (x1+i) :: g (i+1) (x2::xs)
and g i = function
    [] -> [i]
  | x::xs -> (x-i) :: f (i+1) xs

f 10 [1..10]

//2.1
//2.1.1
//The warning occurs, as the algorithm will always take the first element when there is one, and therefore will never 
//get to the point of trying to take out multiple element from the list


let rec fFix i = function
    [] -> []
  | x::xs when xs = [x]-> (x+i) :: gFix (i+1) xs //insert a "when" clause, which compares xs to a list with only one element
  | x1::x2::xs -> (x1+i) :: gFix (i+1) (x2::xs)
and gFix i = function
    [] -> [i]
  | x::xs -> (x-i) :: fFix (i+1) xs

fFix 10 [1..10] //val it : int list = [11; -9; 15; -9; 19; -9; 23; -9; 27; -9]

//2.1.2:

let sum xs = 
    match xs with
    |[] -> [] //if xs is empty, return an empty list
    |[x] -> [] //if there's one element in the list, return an empty list
    |_  -> let rec inner xs xs' i = //else, make a recuresive inner function
                match xs with //match xs with
                |[] -> xs' //if it's empty,  return xs'
                |x::xs when xs = [x] -> xs'@[i+x] //if there's only one element in the lsit, append it to xs' added together with the previous element in the list
                |x1::x2::xs -> inner ([x2]@xs) (xs'@[x1+x2]) x2 // When more than one element, recursively call inner with xs where x2 is added back
                                                                // and xs' where x1 and x2 has been added together, and finally element x2
                |_ -> xs' //if anything else happens, return xs'
           inner xs [] 0;; //instanciate with xs, an empty list, and the value 0

let test = f 10 [1..10]

sum test //val it : int list = [2; 6; 6; 10; 10; 14; 14; 18; 18]

//The result above is as expected, as the list sonists of some negative integers, and thus e.g. (-9)+19=10

//2.2

(*2.2.1:
g and f are not tail recursive, as the call to x1+i occours outside the function call, thus being stored seperately in the stack
*)


//2.2.2

let rec fA i acc = function 
        [] -> [] //if the list is empty, return an empty list
      | x::xs when xs = [x]-> gA (i+1) xs ((x+i)::acc) //call gA with (x+i) conned onto the accumulator
      | x1::x2::xs -> gA (i+1) (x2::xs) ((x1+i)::acc) //same, but with x1+i
and gA i acc = function 
    |[] -> [i]
    |x::xs -> fA (i+1) xs ((x-i)::acc);; //call fA with x-i conned onto the accumulator

fA 10 [1..10] [] //it's empty, something is not working


//2.2.3:



//=================================================================================================================

(*Question 3*)
type name = string;;
type FamilyTree = Family of name * name * Children list
and Children = Single of name
             | Couple of FamilyTree;;

let fam1 = Family ("Hanne", "Peter", [])
let fam2 = Family ("Kurt", "Pia", [Single "Henrik"]);;


//3.1
(*3.1.1:
The type of fam1 is a FamilyTree. This can be deduced, as we declare that fam1 contains a Family of name,name, children list, which is exactly what a 
FamilyTree consits of
*)

//3.1.2:

let fam3 = Family("Charlotte", "Oliver", [Couple fam1; Couple fam2]);; //Family consisting of charlotte and oliver, with a list of kids, their status, and their family
                                                                       //tree

//3.2

//3.2.1
let rec numPerFam (ft:FamilyTree) = function
    |Family(_,_,[]) -> 2 //if the family has no children, return 2
    |Family(_,_,ch) ->  match ch with //if they do have children
                            |[l] -> 2 + (numPerChildren l) //if they only have one kid, add 2 to numPerChildren of that element
                            |xs   -> 2 + (List.map(fun f -> numPerChildren f) xs) //else, apply numPerChildren to all the kids, and plus that together with 2              

and numPerChildren (ch:Children) = 
    match ch with
    |Single x -> 1 //if the child is single, add one
    |Couple x -> 2 + (numPerFam x);; //else, add 2.

//NOTE numPerFam doesn't work, neither does numPer children. However, I can't get it to work probably, but I believe I'm close to a working answer


//3.2.2:

let rec toListFam (ft:FamilyTree) =


and toListChildren (ch:Children) = 
    match ch with
    |Single x -> 






//3.3

//No, as single's can only contain a string, and nothing else. Thus, there is no way to associate the person with a new familiy tree, where
//Children could exist.

//=================================================================================================================

(*Question 4*)

//4.1

//4.1.1


let rec F n =
    match n with
    |0 -> 1
    |n when n>0 -> n-M(F(n-1))//if n is greater than 0, do n-M(F(n-1))
    |_ -> failwith "Invalid input" //if n is less than zero, fail with warning
and M n =
    match n with
    |0 -> 0
    |n when n>0 -> n-F(M(n-1)) //Same, but calling n-F(M(n-1)) instead
    |_ -> failwith "Invalid input"

List.map F [0..10];; //[1; 1; 2; 2; 3; 3; 4; 5; 5; 6; 6]

List.map M [0..10];; //[0; 0; 1; 2; 2; 3; 4; 4; 5; 6; 6]


//4.1.2:
let  combineFM n = ((F n), (M n)) //call F n and M n, and combine to at tuple

combineFM 4 //val it : int * int = (3, 2)

//4.2

//4.2.1:

let Fseq = Seq.initInfinite(fun i -> F i);; //Initiate an infinite function, where i is the integer in the sequence, and then appply F on that i

Seq.item 5 Fseq //val it : int = 3

//4.2.2:

let FseqCache = Seq.cache(Fseq);; //Use the higher order function Se.cache, to make a cached version of Fseq

Seq.item 5 FseqCache //val it : int = 3

(*The two functions has the same type, as they do the exact same thing. The only difference is, that the Fseq would be called immidiatel when executed, while
    FSeqCache is only executed when we actually use it (i.e. it is cached*)

let combineFMSeq = seq{for i in 0..1000 do yield combineFM i};; //create a sequence, in this case with 1001 elements
                                                                //it was unclear what the size had to be, so I made the decision to make it size 10001.
                                                                //Then yield the tuple returned from combineFM on i

Seq.take 4 combineFMSeq //val it : seq<int * int> = seq [(1, 0); (1, 0); (2, 1); (2, 2)]


//=================================================================================================================