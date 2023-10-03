module library.BST

type BST<'a when 'a: comparison> = 
   | Empty                   
   | Node of BST<'a>*'a*BST<'a>
   
let empty = Empty

let private comparer elem = (fun e -> if (e < elem) then -1 elif (e > elem) then 1 else 0)

let rec insert elem bst =
   match bst with
   | Empty                        -> Node(Empty, elem, Empty)
   | Node (l, a, r) when a < elem -> Node(l, a, insert elem r)
   | Node (l, a, r) when a > elem -> Node(insert elem l, a, r)
   | _                            -> bst


let rec contains elem = function
   | Empty                        -> false
   | Node (_, a, r) when a < elem -> contains elem r
   | Node (l, a, _) when a > elem -> contains elem l
   | _                            -> true
   
let rec private inOrderSuccessor elem = function
   | Empty -> elem
   | Node(Empty, a, Empty) -> a
   | Node(Empty, _, r) -> inOrderSuccessor elem r
   | Node(l, _, Empty) -> inOrderSuccessor elem l
   | _ -> failwith "Can this ever happen"

let rec remove elem = function
   | Empty                        -> Empty
   | Node (l, a, r) when a < elem -> Node(l, a, remove elem r)
   | Node (l, a, r) when a > elem -> Node(remove elem l, a, r)
   | Node(l, a, r)                ->
      match (l, r) with
      | (Empty, Empty)            -> Empty
      | (Empty, Node(l', a', r')) -> Node(l', a', r')
      | (Node(l', a', r'), Empty) -> Node(l', a', r')
      | (_, _)                    ->
         let successor = inOrderSuccessor a r
         let r' = remove successor r
         Node(l, successor, r')