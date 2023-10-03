module Tests

open FsCheck
open FsCheck.Xunit
open Swensen.Unquote

module Math =
    open library.Math

    [<Property>]
    let ``fact(n) should be equal to n*fact(n-1)`` (i: int) =
        i > 1 ==> (lazy test <@ fact(i) = fact(i-1) * i @>)
        
    [<Property>]
    let ``fact(1) should be an identity 'element'`` (i: int) =
        i > 1 ==> (lazy test <@ fact(i) = fact(i)*fact(1) @>)

module BST =
    open library.BST
    
    [<Property>]
    let ``insert and remove should yield an empty tree`` (i: int) =
        test <@ remove i (insert i empty) = empty @>
        
    [<Property>]
    let ``insert the same element again, should yield the same three`` (i: int) =
        let l = insert i empty
        test <@ insert i l = l @>
        
    [<Property>]
    let ``insert a number of elements should a tree where you can find them all`` (l: int list) =
        let bst = List.fold (fun a i -> insert i a) empty l
        test <@ List.forall (fun i -> contains i bst) l = true @>
        
    [<Property>]
    let ``insert a number of elements should a tree where you can remove them all`` (l: int list) =
        let l' = l |> Seq.distinct |> Seq.toList
        let bst = List.fold (fun a i -> insert i a) empty l'
        let bst' = List.fold (fun a i -> remove i a) bst l'
        test <@ bst' = empty @>
    
    [<Property>]
    let ``should include numbers not inserted`` (l: int list) =
        List.length l > 0 ==> lazy (
            let bst = List.fold (fun a i -> insert i a) empty l
            let notInL = List.filter (fun i -> not (List.contains i l))[0..(List.max l+1)]
            for i in notInL do
                test <@ contains i bst = false @>
        )
