module Tournament.Client.Model.Tests
#if FABLE_COMPILER
open Fable.Mocha
#else
open Expecto
#endif

open Tournament.Client.Model.MergeSort

let createTests isGreaterThan sort =
    [
        testCase "[10; 3; 2; 20; 1; -10] -> [-10; 1; 2; 3; 10; 20]" <| fun () ->
            Expect.equal
                (sort isGreaterThan [10; 3; 2; 20; 1; -10])
                [-10; 1; 2; 3; 10; 20]
                ""
        testCase "[-2; 1; -10; 8; 3] -> [-10; -2; 1; 3; 8]" <| fun () ->
            Expect.equal
                (sort isGreaterThan [-2; 1; -10; 8; 3])
                [-10; -2; 1; 3; 8]
                ""
        testCase "[-6; 5; 2; -10; -10; 9; 3; -4; -2; -3] -> [-10; -10; -6; -4; -3; -2; 2; 3; 5; 9]" <| fun () ->
            Expect.equal
                (sort isGreaterThan [-6; 5; 2; -10; -10; 9; 3; -4; -2; -3])
                [-10; -10; -6; -4; -3; -2; 2; 3; 5; 9]
                ""
    ]

#if !FABLE_COMPILER
[<Tests>]
#endif
let ``Tournament.Client.Model.qsort`` =
    let isGreaterThan x y =
        x > y
    testList "Tournament.Client.Model.qsort" [
        yield! createTests isGreaterThan qsort
    ]

#if !FABLE_COMPILER
[<Tests>]
#endif
let ``Tournament.Client.Model.mergeSort`` =
    let isGreaterThan x y =
        x > y
    testList "Tournament.Client.Model.mergeSort" [
        yield! createTests isGreaterThan MergeSort.start
    ]

#if !FABLE_COMPILER
[<Tests>]
#endif
let ``MergeSort.joinTwoSortedLists`` =
    testList "MergeSort.joinTwoSortedLists" [
        let joinTwoSortedLists isGreaterThan sorted1 sorted2 =
            let rec loop = function
                | Choice1Of2 x ->
                    match x with
                    | JoinTwoSortedLists.Main.IsGreaterThanReq ((x, y), data) ->
                        data
                        |> JoinTwoSortedLists.IsGreaterThan.exec (isGreaterThan x y)
                        |> joinTwoSortedListsInterp
                        |> loop
                    | x ->
                        failwithf "%A Not Implemented" x
                | Choice2Of2 result ->
                    result
            JoinTwoSortedLists.start sorted1 sorted2
            |> joinTwoSortedListsInterp
            |> loop

        let isGreaterThan x y =
            x > y
        let inline create (sorted1, sorted2) exp =
            testCase (sprintf "%A, %A -> %A" sorted1 sorted2 exp) <| fun () ->
                Expect.equal
                    (joinTwoSortedLists isGreaterThan sorted1 sorted2)
                    exp
                    ""
        create ([], [2; 20]) [2; 20]
        create ([2; 20], []) [2; 20]
        create ([3; 10], [2; 20]) [2; 3; 10; 20]
        create ([3; 10], [2; 20; 30]) [2; 3; 10; 20; 30]
        create ([2; 20; 30], [3; 10]) [2; 3; 10; 20; 30]
    ]

#if !FABLE_COMPILER
[<Tests>]
#endif
let ``MergeSort.joinSortedLists`` =
    testList "MergeSort.joinSortedLists" [
        let isGreaterThan x y =
            x > y
        let inline create xss exp =
            testCase (sprintf "%A -> %A" xss exp) <| fun () ->
                Expect.equal
                    (MergeSort.joinSortedLists isGreaterThan xss)
                    exp
                    ""
        create [[3; 10]; [2; 20]; [-10; 1]; [5]] [[2; 3; 10; 20]; [-10; 1; 5]]
        create [[2; 3; 10; 20]; [-10; 1; 5]] [[-10; 1; 2; 3; 5; 10; 20]]
    ]
