module Tournament.Client.Model.Tests
#if FABLE_COMPILER
open Fable.Mocha
#else
open Expecto
#endif

let createTests sort =
    [
        testCase "[10; 3; 2; 20; 1; -10] -> [-10; 1; 2; 3; 10; 20]" <| fun () ->
            Expect.equal
                (sort [10; 3; 2; 20; 1; -10])
                [-10; 1; 2; 3; 10; 20]
                ""
        testCase "[-2; 1; -10; 8; 3] -> [-10; -2; 1; 3; 8]" <| fun () ->
            Expect.equal
                (sort [-2; 1; -10; 8; 3])
                [-10; -2; 1; 3; 8]
                ""
        testCase "[-6; 5; 2; -10; -10; 9; 3; -4; -2; -3] -> [-10; -10; -6; -4; -3; -2; 2; 3; 5; 9]" <| fun () ->
            Expect.equal
                (sort [-6; 5; 2; -10; -10; 9; 3; -4; -2; -3])
                [-10; -10; -6; -4; -3; -2; 2; 3; 5; 9]
                ""
    ]

#if !FABLE_COMPILER
[<Tests>]
#endif
let ``Tournament.Client.Model.qsort`` =
    testList "Tournament.Client.Model.qsort" [
        yield! createTests qsort
    ]

#if !FABLE_COMPILER
[<Tests>]
#endif
let ``Tournament.Client.Model.mergeSort`` =
    testList "Tournament.Client.Model.mergeSort" [
        yield! createTests mergeSort
    ]
