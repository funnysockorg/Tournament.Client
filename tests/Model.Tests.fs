module Tournament.Client.Model.Tests
#if FABLE_COMPILER
open Fable.Mocha
#else
open Expecto
#endif

#if !FABLE_COMPILER
[<Tests>]
#endif
let ``Tournament.Client.Model.qsort`` =
    testList "Tournament.Client.Model.qsort" [
        testCase "[10; 3; 2; 20; 1; -10] -> [-10; 1; 2; 3; 10; 20]" <| fun () ->
            Expect.equal
                (qsort [10; 3; 2; 20; 1; -10])
                [-10; 1; 2; 3; 10; 20]
                ""
        testCase "[-2; 1; -10; 8; 3] -> [-10; -2; 1; 3; 8]" <| fun () ->
            Expect.equal
                (qsort [-2; 1; -10; 8; 3])
                [-10; -2; 1; 3; 8]
                ""
        testCase "[-6; 5; 2; -10; -10; 9; 3; -4; -2; -3] -> [-10; -10; -6; -4; -3; -2; 2; 3; 5; 9]" <| fun () ->
            Expect.equal
                (qsort [-6; 5; 2; -10; -10; 9; 3; -4; -2; -3])
                [-10; -10; -6; -4; -3; -2; 2; 3; 5; 9]
                ""
    ]
