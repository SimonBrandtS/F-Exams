(*Question 1:*)

type Heap<'a when 'a: equality> =
   | EmptyHP
   | HP of 'a*Heap<'a>*Heap<'a>;;


let ex3 = HP(1,HP(2,HP(3,EmptyHP,EmptyHP),HP(5,EmptyHP,EmptyHP)),HP(4,EmptyHP,EmptyHP));;

ex3;; (*
       val it : Heap<int> =
       HP
         (1,HP (2,HP (3,EmptyHP,EmptyHP),HP (5,EmptyHP,EmptyHP)),
            HP (4,EmptyHP,EmptyHP))
*)

//The Type of ex3 is a Heap<int>

let empty = EmptyHP;; //val empty : Heap<'a> when 'a : equality

exception HeapError of string;; //An exception HeapError carrying a string

(*Question 1.2:*)

let isEmpty = function
   |xs when xs = empty -> true //match input with empty
   |_                  -> false;;

isEmpty empty;; //val it : bool = true

isEmpty ex3;;   //val it : bool = false

let rec size h = 
      match h with
      |EmptyHP -> 0 //if heap is empty, return 0
      |HP(_,rx,lx) -> 1 + (size rx) + (size lx);; //when theres something in left and right tree, add 1 to the two recursive calls on left and right.

size empty //val it : int = 0

size ex3 //val it : int = 5


(*let find h = function
   |HP(x,_,_) -> x*)

let find h =
   match h with
   |HP(x,_,_) -> x     //take the first element we see, and return x
   |_                -> failwith "something went wrong";; //if for some reason we did not find the first element, fail


 find ex3;; //val it : int = 1




let rec chkHeapProperty (h:Heap<'a>) =
   match h with
   |HP(_,EmptyHP,EmptyHP) -> true //If we're at a leaf, return true
   |HP(x,lt,rt)      -> match x with //match the value with
                        |x when x > find lt -> false //if x iis greater than the first value in lt, return false
                        |x when x > find rt -> false //same, but with right tree
                        |x when x < find lt -> chkHeapProperty lt //else, run recursively on left tree
                        |x when x < find rt -> chkHeapProperty rt //and right tree
                        |_                  -> false //if there's anything equal in the children, return falls
   |_          -> true;; //anything else returns true

chkHeapProperty ex3;;

let ex4 = HP(1,HP(8,HP(3,EmptyHP,EmptyHP),HP(5,EmptyHP,EmptyHP)),HP(4,EmptyHP,EmptyHP));;

chkHeapProperty ex4;;

(*Question 1.3:*)

let rec map f h = //Maps the function to all values from left to right
   match h with
   |HP(x,EmptyHP,EmptyHP) -> HP(f x, EmptyHP, EmptyHP) //If we're at a leaf, apply the function to x and collect it in a new node
   |HP(x,lt,rt)           -> HP(f x, map f lt, map f rt) //else, apply f to x, and recursively call map on lt and rt to, then map the 
                                                         //result to a new node in the heap
   |_                     -> empty;;                     //If it's empty, return an empty heap.


 map ((+)1) ex3;; (*val it : Heap<int> =
                    HP
                      (2,HP (3,HP (4,EmptyHP,EmptyHP),HP (6,EmptyHP,EmptyHP)),
                       HP (5,EmptyHP,EmptyHP))
                       
                       Returns a new heap with 1 added to all values from left to right
                       *)

let f x = x*(-x);; 

map f ex3;; //Is not valid, as the top node has a higher value than it's children.

(*Question 2.1*)

let genRandoms n = 
     let rnd = System.Random() //initiate a System.Random function
     Array.init n (fun x -> rnd.Next(1,10000));; //Initiate an array of size n, where elements are generated randomly between 1 and 10000

//val it : int [] = [|5813; 5017; 8886; 6843; 7524; 66; 9904; 6920; 9849; 8529|] is one example of what could be generated

let genRandomsP n =
    let rnd = System.Random() //initiate a System.Random function
    Array.Parallel.init n (fun x -> rnd.Next(1,10000));; //Array.Parallel makes the same function from before run in parallel.


(*Question 2.2:*)
let split xs =
  let n = List.length xs
  xs.[0..n/2-1],xs.[n/2..n-1] //initialise two lists xs in a tuple with everything from 0..n/2-1 in one list, and n/2..n-1 in the other

split [1;2;3;4;5;6];; //val it : int list * int list = ([1; 2; 3], [4; 5; 6])
                     //Creates two equal size lists, and puts them into tuple (l1,l2).

split [1;2;3;4;5];; //val it : int list * int list = ([1; 2], [3; 4; 5])
                    //This splits the list into two lists, one with 2 integers, and one with 3 where the big one is on the right site of the tuple
split [1];; //val it : int list * int list = ([], [1]) 
            //When splitting a list with only one element, the left tuple becomes an empty list, while the right one contains the element

let indivisible xs = //checks if list contains zero, one, or more elements
   match xs with
   |[] -> true
   |[_]-> true
   |_ -> false;;

indivisible [1;2;3;4;5;6];; //val it : bool = false

indivisible [1;2;3;4;5];; //val it : bool = false

let merge (xs,ys) =
   let rec inner (xs,ys) =
      match (xs,ys) with
      |(xs,[]) -> xs
      |([],ys) -> ys     
      |(x::xs,y::ys) -> if x<y || x=y then //takes the first  element from xs and ys and compares them to eachother
                          let xs' = x::[y] //make a xs' of x::[y]
                          xs'@inner (xs,ys) //appends xs' to the recursive call of xs and ys without the first elements
                        else
                        let xs' = y::[x]
                        xs'@inner(xs,ys)
   inner (xs,ys)


merge ([1;2;3;4;5;6],[1;2;3;4;5]);; //val it : int list = [1; 1; 2; 2; 3; 3; 4; 4; 5; 5; 6]

merge ([],[1;2]);; //One empty and one with element, concatenates into one list with only elements from second list

merge ([],[]);; //Fails, don't know why.

(*Question 2.3*)

let test = [15;7;22;1002;9];;


let divideAndConquer split merge indivisible (p:List<'a>) =
   let rec dc p =
      if indivisible p
      then p
      else merge(dc(fst(split p)),dc(snd(split p))) //merge whatever we get from calling dc in the fst (first) element in the tuple that is derived from
                                                    //Splitting p with the snd (second) element.
   dc p;;

 divideAndConquer split merge indivisible test;; //val it : List<int> = [7; 9; 15; 22; 1002]

(*Question 3.1*)

let triNum = Seq.initInfinite(fun i -> (i*(i+1))/2);;

let triNumC = Seq.cache(Seq.initInfinite(fun i -> (i*(i+1))/2));;

(*Question 3.2:*)

let rec filterOddIndex s = 
   Seq.append (Seq.singleton (Seq.item 0 s))
      (filterOddIndex (Seq.skip 2 s));;

let myFilterOddIndex s = Seq.filter(fun i -> (s |> Seq.findIndex (fun x -> x =i ))%2=0) s //Filter all the elements at odd indexes, by checking if the elements
                                                                                          //index%2 =0. If so, we keep it


(*Question 3.3:*)

let rec zipSeq s1 s2 =
     Seq.map2(fun x y -> x,y) s1 s2;;
 

let se1 = seq{1;2;3};;

let se2 = seq{5;6;7};;

zipSeq se1 se2;;

(*Question 4.1:*)

exception FigError of string;;

type Point = P of double*double;;

type Fig =
   | Circle of Point*double
   | Line of Point*Point
   | Move of double*double*Fig
   | Combine of Fig list
   | Label of string*Fig
   | Ref of string

let figEx01 = Combine [Circle(P(1.0,1.0),2.0);Line(P(0.0,0.0),P(1.0,1.0))];;


//rectEx is the comibation of 4 lines, each consisting of 2 points.
let rectEx = Combine [Line(P(-1.0,1.0),P(1.0,1.0));Line(P(1.0,1.0),P(1.0,-1.0));Line(P(1.0,-1.0),P(-1.0,-1.0));Line(P(-1.0,-1.0),P(-1.0,1.0))];;


//function does the same as rectEx, however with doubles subsituted by x1,x2,y1,y2
let rect (x1,y1) (x2,y2) = Combine [Line(P(x1,y1),P(x2,y1));Line(P(x2,y1),P(x2,y2));Line(P(x2,y2),P(x1,y2));Line(P(x1,-y2),P(x1,y1))];;


Combine [Line(P(-2.0,1.0),P(1.0,1.0));Line(P(1.0,1.0),P(1.0,-1.0));
         Line(P(1.0,-1.0),P(-2.0,-1.0));Line(P(-2.0,-1.0),P(-2.0,1.0))];;

//val it : Fig =
//  Combine
//    [Line (P (-2.0,1.0),P (1.0,1.0)); Line (P (1.0,1.0),P (1.0,-1.0));
//     Line (P (1.0,-1.0),P (-2.0,-1.0)); Line (P (-2.0,-1.0),P (-2.0,1.0))]

let figEx02 =
   Combine [Label("c",Circle(P(0.0,0.0),1.0));
            Move(1.0,1.0,Ref "c");
            Move(2.0,2.0,Ref "c")];;


let buildEnv2 fig = 
    let m = Map.empty
    let rec inner fig m=
      match fig with
      |Label(z,y) -> Map.add z y m
      |Combine (x::xs) -> inner x m
      |_          -> m
    inner fig m


let envEx02 = buildEnv2 figEx02

(*let substFigRefs env fig = function

   |Combine -> let rec inner fig = function
               |Label(z,y) ->*)
