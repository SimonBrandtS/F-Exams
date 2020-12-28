//Q1.1

type Multiset<'a when 'a: comparison> = MSet of Map<'a, int>;;

let ex = MSet (Map.ofList [("a",1);("b",2);("c",1)]);;


let wrong = MSet (Map.ofList [("a",0);("b",2);("c",1)]);;

//Q1.1.1

let diceSet = MSet(Map.ofList[(1,2);(2,1);(3,5);(5,2);(6,2)]);;


//Q1.1.2
//The type of diceSet is Multiset<int>

//Q1.1.3

//Q1.2.1
let newMultiset() = MSet(Map.empty)


let isEmpty xs =
    match xs with //matches xs with
    |xs when xs = newMultiset() -> true //if xs is equal to the empty MultiSet, return true
    |_ -> false;; //Otherwise, return false

isEmpty diceSet;; //val it : bool = false

(*let x = Map.empty.Add("1","2")
                 .Add("14","400")*)

(*Q1.3.1*)


let add k (MSet ms) = //Start with casting ms to MSet, which turns it into a map we can work on
     match ms with //match the map with
        |ms when Map.isEmpty ms -> MSet(Map.add k 1 ms) //If map is empty, add key k and value 1 to the map (i.e. it has one occurence)
        |_                  -> match Map.tryFind k ms with //Else, match whatever is returned from try find key k on map ms with
                               |None -> MSet(Map.add k 1 ms) //If nothing is returned, i.e. if map does not contain k, add k to the map, and convert
                                                             //to MSet
                               |Some v -> MSet(Map.add k (v+1) ms);; //If there is some value associated with key k, add the key to the map again (maps can only 
                                                                     //contain a key once), and map it to the value +1.


add "a" ex;; //val it : Multiset<string> = MSet (map [("a", 2); ("b", 2); ("c", 1)])
let test = MSet(Map.empty);;
add "a" test;;

(*below function does almost exactly as before, but with minor modifications*)
let del k (MSet ms) =
    match ms with
    |ms when Map.isEmpty ms -> MSet(ms) //If the map is empty, just return the empty map
    |_                      -> match Map.tryFind k ms with
                                |None -> MSet(ms) //If the key doesn't exist, return the map converted to MSet.
                                |Some v -> let x = v-1 //subtract 1 from the value
                                           if x>0 then MSet(Map.add k x ms) //check if the new value is greater than zero. If so, map the new value to k
                                           else MSet(Map.remove k ms);; //if the enw value is 0 or less, remove the key from map.
                                         

del "c" ex;; // Multiset<string> = MSet (map [("a", 1); ("b", 2)])
del "b" ex;; //val it : Multiset<string> = MSet (map [("a", 1); ("b", 1); ("c", 1)])

(*1.4.1*)

let toList (MSet ms) = 
    let xs = Map.toList ms //convert the map to a list
    let rec inner xs = 
        match xs with
        |[] -> [] //if the list is empty, return an empty list
        |x::xs -> match x with //take out first element of xs and match with
                      |(y,z) -> [for i in 0..(z-1) -> y]@inner xs //will always contain a tuble. Then add y to a list z times, and append it onto the list returned
                                                                  //From calling inner recursively on xs (where first element is removed)       
    inner xs;; 


toList ex;; //val it : string list = ["a"; "b"; "b"; "c"]

(*1.4.2*)

let fromList xs =
    let ms = MSet(Map.empty) //create a new emoty MSet
    let rec inner ms xs = //create a inner recursive function which takes ms and a list xs
        match xs with 
        |[] -> ms//if the list is empty, return ms
        |x::xs -> let ms' = add x ms //Else, take the first element, add it to ms, and save it as a new MSet ms'
                  inner ms' xs //call inner with ms' and the list without the first element
    inner ms xs //instansiate inner with ms and xs

fromList ["a";"a";"b"];; //val it : Multiset<string> = MSet (map [("a", 2); ("b", 1)])       

fromList ["a";"a";"a";"a";"b";"b";] //val it : Multiset<string> = MSet (map [("a", 4); ("b", 2)])

let fromList2 xs = List.fold(fun acc x -> add x acc) (newMultiset()) xs

fromList2 ["a";"a";"b"];; //val it : Multiset<string> = MSet (map [("a", 2); ("b", 1)])       

fromList2 ["a";"a";"a";"a";"b";"b";]

(*1.5.1*)

let map f ms =
    fromList (List.map f (toList ms));;

map (fun (c:string) -> c.ToUpper()) ex;; //val it : Multiset<string> = MSet (map [("A", 1); ("B", 2); ("C", 1)])


let fold f a ms = List.fold f a (toList ms);;

 fold (fun acc e-> acc+e) "" ex

 (*1.6.1*)

 let union ms1 ms2 =
    let xs = (toList ms1@toList ms2) //convert each Multiset to a list, and append them.
    fromList xs;; //convert back to Multiset.

union ex ex;; // val it : Multiset<string> = MSet (map [("a", 2); ("b", 4); ("c", 2)])

(*1.6.2*)

let minus ms1 ms2 =
    let xs = toList ms2
    let rec inner xs ms1 =
        match xs with
        |[] -> ms1
        |x::xs -> inner xs (del x ms1)
    inner xs ms1;;

minus ex ex;;

(*2.1.1*)

let rec f n =
    if n < 10 then "f" + g (n+1) else "f"
and g n =
    if n < 10 then "g" + f (n+1) else "g"


(*All n >= 10 will generate strings starting and ending with f. So will all even number*)

(*2.1.2*)

g 4 //val it : string = "gfgfgfg"

(*2.1.3:

No, as it will always reach 10, which will end the computation.

*)


(*Question 2.2*)
let fA n =
    let rec inner (acc:string) n =
        if n< 11 then gA(acc + "f") (n+1)
        else acc
    and gA acc n =
        if n < 11 then inner(acc + "g") (n+1)
        else acc
    inner "" n;;

 let gA n =
    let rec inner (acc:string) n =
        if n< 11 then fA(acc + "g") (n+1)
        else acc
    and fA acc n =
        if n < 11 then inner(acc + "f") (n+1)
        else acc
    inner "" n;;

    

fA 1;; //val it : string = "fgfgfgfgfg"
gA 1;; //"gfgfgfgfgf"

(*Question 3.1.1:*)


let myFinSeq (n,m) = seq { for i in [0 .. n] do
                                yield! seq { for j in [0 .. m] do yield j }};;

myFinSeq(1,2);;

(*myFinSeq returns a sequence containing everything from a sequence containing the numbers 0 to m, n times.*)

(*Question 3.2.1:*)

//let myFinSeq2 (n,m) = seq {for i in [0 .. n] do yield (i,seq{for x in 0..m do yield x})}
let myFinSeq2 (n,m) = seq {for i in [0 .. n] do yield (i,seq{for j in [0..m] do yield j})}


myFinSeq2 (0,0);; //seq<int * seq<int>> = seq [(0, seq [0])]
myFinSeq2 (1,1);; //val it : seq<int * seq<int>> = seq [(0, seq [0; 1]); (1, seq [0; 1])]

type Row = int
type Col = char
type CellAddr = Row*Col
type ArithOp = Add | Sub | Mul | Div
type RangeOp = Sum | Count
type CellDef =
    FCst of float
    | SCst of string
    | Ref of CellAddr
    | RangeOp of CellAddr*CellAddr*RangeOp
    | ArithOp of CellDef*ArithOp*CellDef
type CellValue =
    S of string
    | F of float
type Sheet = Map<CellAddr,CellDef>

