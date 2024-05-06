module Tournament.Client.Model.MergeSort.Tests
#if FABLE_COMPILER
open Fable.Mocha
#else
open Expecto
#endif

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
let start =
    let start isGreaterThan (xs: 'a list) =
        let rec loop = function
            | Start.Main.JoinSortedLists x ->
                match x with
                | JoinSortedLists.Main.JoinTwoSortedListsReq (joinTwoSortedListsCmd, data) ->
                    match joinTwoSortedListsCmd with
                    | JoinTwoSortedLists.Main.IsGreaterThanReq ((x, y), data') ->
                        let result =
                            data'
                            |> JoinTwoSortedLists.IsGreaterThan.exec (isGreaterThan x y)
                        JoinSortedLists.Main.JoinTwoSortedListsReq (result, data)
                        |> Start.Main.JoinSortedLists
                        |> startInterp
                        |> loop
                    | x ->
                        failwithf "%A Not Implemented" x
                | x ->
                    failwithf "%A Not Implemented" x
            | Start.Main.Result sortedList ->
                sortedList
        xs
        |> Start.start
        |> startInterp
        |> loop

    let isGreaterThan x y =
        x > y
    testList "start" [
        yield! createTests isGreaterThan start
    ]

#if !FABLE_COMPILER
[<Tests>]
#endif
let joinTwoSortedLists =
    testList "joinTwoSortedLists" [
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
let joinSortedLists =
    testList "joinSortedLists" [
        let joinSortedLists isGreaterThan xss =
            let rec loop = function
                | Choice1Of2 x ->
                    match x with
                    | JoinSortedLists.Main.JoinTwoSortedListsReq (joinTwoSortedListsCmd, data) ->
                        match joinTwoSortedListsCmd with
                        | JoinTwoSortedLists.Main.IsGreaterThanReq ((x, y), data') ->
                            let result =
                                data'
                                |> JoinTwoSortedLists.IsGreaterThan.exec (isGreaterThan x y)
                            JoinSortedLists.Main.JoinTwoSortedListsReq (result, data)
                            |> joinSortedListsInterp
                            |> loop
                        | x ->
                            failwithf "%A Not Implemented" x
                    | x ->
                        failwithf "%A Not Implemented" x
                | Choice2Of2 result ->
                    result
            JoinSortedLists.start xss
            |> joinSortedListsInterp
            |> loop

        let isGreaterThan x y =
            x > y
        let inline create xss exp =
            testCase (sprintf "%A -> %A" xss exp) <| fun () ->
                Expect.equal
                    (joinSortedLists isGreaterThan xss)
                    exp
                    ""
        create [[3; 10]; [2; 20]; [-10; 1]; [5]] [[2; 3; 10; 20]; [-10; 1; 5]]
        create [[2; 3; 10; 20]; [-10; 1; 5]] [[-10; 1; 2; 3; 5; 10; 20]]
    ]
