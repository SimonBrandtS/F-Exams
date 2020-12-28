(*I hereby declare that I myself have created this exam hand-in in its entirety without help from
anybody else.*)


(*Simon Brandt Sørensen*)


type item = {id : int;
            name : string;
            price : float};;
type register = item list;;

(*1.1*)

let reg =[{id=1; name= "Milk"; price = 8.75};
               {id=2; name ="Juice";price=16.25};
               {id=3; name = "Rye Bread"; price=25.00};
               {id=4; name = "White Bread"; price=18.50}];;
//1.2
exception Register of string;;

let rec getItemById i r =
    match r with
    |[] -> failwith "item doesn't exist"
    |z::r -> match z with
               |{id=y;name = m;price = n} -> if y=i then m else getItemById i r;;

getItemById 1 reg;;

//1.3:

let nextId r =
   let rec inner r i =
        match r with
        |[] -> 1
        |x::r -> match x with
                    |{id=y;name = m;price = n} -> inner r (i+1)+1
   inner r 0;;

nextId reg;


let addItem n p r =
    r@[{id=(nextId r);name=n;price = p}];;

addItem "sugar" 15.0 reg;;

let deleteItemById i r = 
    let rec inner i r acc=
        match r with
        |[] -> acc@r
        |x::r -> match x with
                 |{id=y;name = m;price = n} when y <> i -> inner i r (acc@[{id=y;name = m;price = n}])
                 |{id=y;name = m;price = n} when y = i -> inner i r acc
                 |_ -> failwith "Something went wrong"
    inner i r []

deleteItemById 1 reg;;

let rec uniqueRegister r =
    match r with
    |[] -> true
    |x::r   -> match x with
                    |{id=y;name = m;price = n} -> if List.exists (fun elem -> elem={id=y;name = m;price = n}) r then false else uniqueRegister r

uniqueRegister reg;;

let reg2 =[{id=1; name= "Milk"; price = 8.75};
               {id=1; name= "Milk"; price = 8.75};
               {id=2; name ="Juice";price=16.25};
               {id=3; name = "Rye Bread"; price=25.00};
               {id=4; name = "White Bread"; price=18.50}];;


uniqueRegister reg2;;


let itemsInPriceRange p d r=
    let rec inner p d r acc =
        match r with
        |[]    -> acc
        |x::r -> match x with
                    |{id=y;name = m;price = n} -> if n<p+d && n> p-d then inner p d r ([{id=y;name = m;price = n}]@acc) else inner p d r acc
    inner p d r [];;

itemsInPriceRange 16.0 2.0 reg;;


let rec f n m =
    if m=0 then n
    else n*f (n+1) (m-1)

//the type of f is int -> int -> int.
// f computes n times what is returned from recursive call to n+1 and m-1 until m reaches 0, which then returns n

f 2 0

f 2 2

let f' n m =
    let rec inner n m acc =
        if m=0 then n*acc
        else inner(n+1)(m-1) n*acc
    inner n m 1;;

f' 2 2//

//Q2.3

let rec z xs ys =
    match (xs, ys) with
    ([],[]) -> []
    | (x::xs,[]) -> (x,x) :: (z xs ys)
    | ([],y::ys) -> (y,y) :: (z xs ys)
    | (x::xs,y::ys) -> (x,y)::(z xs ys);;

//Question 3

type Latex<'a> =
    Section of string*'a*Latex<'a>
    | Subsection of string*'a*Latex<'a>
    | Text of string*Latex<'a>
    | End;;


let text1 = Section ("Introduction", None,
                Text ("This is an introduction to ...",
                    Subsection ("A subsection", None,
                        Text ("As laid out in the introduction we ...",
                            End))));;

//the type of tex1 is Latex<'a option>

let addSecNumbers (Latex<'a> text) = 
    let rec inner text i = function
        |Section(Subsection,_,rest)    -> Section(text,(i+1).ToString(),inner (i+1) 0 rest)
        |Subsection(Text,_,rest)       -> Subsection(text,i.ToString() + "." + (subSec+1).ToString(), addSecNumbers' i (subSec+1) rest)


//Question 4.1

let mySeq = Seq.initInfinite (fun i -> if i % 2 = 0 then -i else i)

Seq.take 10 mySeq  //The type of running Seq.take 10 mySeq is seq<int>, and it returns the sequence [0; 1; -2; 3; ...]

//Question 4.2

let finSeq n M = seq{for i in 0 .. 2 .. (2*M) do yield n+i}

finSeq 5 10;;

//Question 4.3

type X = A of int | B of int | C of int*int;;

let rec zX xs ys =
    match (xs,ys) with
    (A a::aS,B b::bS) -> C(a,b) :: zX aS bS
    | ([],[]) -> []
    | _ -> failwith "Error";;

