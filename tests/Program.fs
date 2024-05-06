module Tournament.Client.Tests.Program
#if FABLE_COMPILER
open Fable.Mocha
#else
open Expecto
#endif

#if FABLE_COMPILER
let all =
    testList "All" [
        Tournament.Client.Model.MergeSort.Tests.start
        Tournament.Client.Model.MergeSort.Tests.joinTwoSortedLists
        Tournament.Client.Model.MergeSort.Tests.joinSortedLists
    ]
#endif

[<EntryPoint>]
let main args =
    #if FABLE_COMPILER
    Mocha.runTests all
    #else
    runTestsInAssembly defaultConfig args
    #endif
