(*I hereby declare that I myself have created this exam hand-in in its entirety without help from
anybody else.*)


(*Simon Brandt Sørensen*)


type OrderedList<'a when 'a : equality> =
    {front: 'a list;
     rear: 'a list}

let ex = {front = ['x']; rear = ['z';'y']}


exception OrderedError of string;;


(*Question 1.1*)

let ol1 = {front = ["Hans"; "Brian"; "Gudrun"]; rear = []};;

let ol2 = {front = ["Hans"; "Brian"]; rear = ["Gudrun"]};;

let ol3 = {front = ["Hans"]; rear = ["Gudrun"; "Brian"]};;

(*There exists 4 possible lists (the final one being where front is empty*)

//1.2.1:

let rec reverse list =
    match list with
    |[] -> []
    |[x] -> [x]
    | head::tail -> reverse tail @ [head];;

let canonical ol = 
    match ol with
    |{front = ls; rear = []} -> ol //if rear is empty, return ol
    |{front = ls; rear = ys} -> let ys' = List.rev ys
                                let ol' = {front = ls@ys'; rear = []}
                                ol';;
                               
canonical ol3 //{ front = ["Hans"; "Brian"; "Gudrun"]
//                               rear = [] }


//1.2.2                                 
let toList ol =
    let ol' = canonical ol
    match ol' with
        |{front = ls; rear = []} -> ls;;

toList ol3 //val it : string list = ["Hans"; "Brian"; "Gudrun"]

toList ex //val it : char list = ['x'; 'y'; 'z']

(*Question 1.3:*)
//1.3.1
let newOl() ={front = []; rear = []};; //ordered list where fron and rear are both empty
//1.3.2
let isEmpty ol = ol = {front = []; rear = []};;

let emptyList = {front = []; rear = []}

isEmpty ex;; //false

isEmpty emptyList;;  //true

//1.4.1:

let addFront e ol=
    let xs = toList ol
    {front = e::xs; rear = []};;

addFront 'w' ex;; // OrderedList<char> = { front = ['w'; 'x'; 'y'; 'z']
 //                              rear = [] }


 let removeFront ol =
    let xs = toList ol
    match xs with
    |x::xs ->   (x,{front = xs; rear = []})
    |_      -> failwith "no elements in list";;

removeFront ex;; //val it : char * OrderedList<char> = ('x', { front = ['y'; 'z']
 //                                           rear = [] })


let peekFront ol =
    let xs = toList ol
    let y = List.head xs
    y;;

peekFront ex //val it : char = 'x'

let append ol1 ol2 =
    let xs = toList ol1
    let ys = List.rev (toList ol2)
    {front = xs; rear = ys};;
    
append ex ex;;


let fold f a ol = List.fold f a (toList ol);;

fold(fun acc e->acc+e.ToString())"" ex;;


let multiplicity ol =
    let xs = toList ol
    let m = Map.empty
    let rec inner xs m =
        match xs with
        |[] -> m
        |x::xs -> match Map.tryFind x m with
                    |None -> Map.add x 1 m
                    |Some v -> Map.add x (v+1) m
                               inner xs m
    inner xs m

multiplicity (addFront 'x' ex)

let rec f i = function
    [] -> [i]
    | x::xs -> i+x :: f (i+1) xs;;

 f 10 [0;1;2;3];;

//2.1.1
 //f computes a list, where if the list is empty, then return the list with i, else, take out the first element of the list given
 //, add i to it, and append that to what is reaturned from the recursive call to f with i+1 and the new xs without element x

//2.1.2:
//The list can never be empty, as it wil alway contain a list with element i, i.e. if you call f 0 [], you will receive a list of [0]


//2.1.3:

//Tehcnically, the loop would be infinite if the list given to it had a size of infinite. However, in reality, this is not the case.

//2.2.1:

let fA i xs=
    let rec inner (i,xs,acc) =
        match xs with
        |[] -> acc@[i]
        |x::xs -> inner((i+1),xs,(acc)@([i+x]))
    inner(i,xs,[]);;

f 10 [0;1;2;3];;
fA 10 [0;1;2;3];;


//2.3:


(*Qustion 3.1:*)

//3.1.1:
let myFinSeq n M = Seq.map (fun m -> n+n * m) [0..M];;
myFinSeq 5 5

let x = Seq.iter(fun x -> printf "%d " x) (myFinSeq 5 5) //to print sequences

x

//myFinSeq n M is a sequence fof length m+1, where each element has been multiplied with (n+n)

//3.2.1:

let mySeq n = Seq.initInfinite(fun i -> n+n*i);; //initiate an infinite sequence, where i is the index. Then use function n+n*i on each of those

//3.3:

let multTable N M =
    let s1 = seq{for i in 0..N do yield i} //creates a sequence from elements 0 to N
    let s2 = seq{for i in 0..M do yield i} //same up to N
    let s3 = seq{for i in 0..((Seq.length s1)-1) do yield Seq.item i s1 * Seq.item i s2} //using Seq.length and Seq.ite, I multiply elements in s1 at index i with the
                                                                                   //corresponing int in s2
    Seq.zip3 s1 s2 s3;; //using Seq.zip3 to make triples out of each element in the three sequences.



multTable 5 5;; //   seq [(0, 0, 0); (1, 1, 1); (2, 2, 4); (3, 3, 9); ...]


let ppMultTable N M = Seq.map (fun (n,m,nm) -> sprintf "%d * %d is %d" n m (n*m)) (multTable N M)

(*

let ppMultTable N M = 
    let s1 = seq{for i in 0..N do yield i} //creates a sequence from elements 0 to N
    let s2 = seq{for i in 0..M do yield i}
    let s3 = seq{for i in 0..((Seq.length s1)-1) do yield Seq.item i s1 * Seq.item i s2}
    let s4 = seq{for i in 0..((Seq.length s1)-1) do yield "Item: %A" Seq.item i s1}
    s4*)



type opr = MovePenUp
            | MovePenDown
            | TurnEast
            | TurnWest
            | TurnNorth
            | TurnSouth
            | Step;;

type plot = Opr of opr
            | Seq of plot*plot;;

let side = Seq(Opr MovePenDown, Seq(Opr Step, Seq(Opr Step, Opr Step)))
let rect = Seq(Seq(Opr TurnEast, side),
                Seq(Opr TurnNorth, Seq(side,
                     Seq(Opr TurnWest, Seq(side,
                            Seq(Opr TurnSouth, side))))));;


//4.1.1
let ppOpr x =  //matches the input with all possible operations, then returns the string
    match x with
      | MovePenUp   -> "MovePenUp"
      | MovePenDown -> "MovePenDown"
      | TurnEast    -> "TurnEast"
      | TurnWest    -> "TurnWest"
      | TurnNorth   -> "TurnNorth"
      | TurnSouth   -> "TurnSouth"
      | Step        -> "Step";;

ppOpr MovePenUp;; //val it : string = "MovePenUp"


//4.1.2

let s1 = seq{1;2;3;4}
let s2 = seq{4;5;6;7;8}

let x1 = Seq.zip s1 s2

let x2 =Seq.allPairs s1 s2

let x3 = Seq.pairwise s1

let lol = Seq.iter(fun x -> printf "%A " x) x1

let lol2 = Seq.iter(fun x -> printf "%A " x) x2

let lol3 = Seq.iter(fun x -> printf "%A " x) x3


