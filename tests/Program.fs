module Tournament.Client.Tests.Program
#if FABLE_COMPILER
open Fable.Mocha
#else
open Expecto
#endif

#if FABLE_COMPILER
let all =
    testList "All" [
        Tournament.Client.Model.Tests.``Tournament.Client.Model.mergeSort``
        Tournament.Client.Model.Tests.``MergeSort.joinTwoSortedLists``
        Tournament.Client.Model.Tests.``MergeSort.joinSortedLists``
    ]
#endif

[<EntryPoint>]
let main args =
    #if FABLE_COMPILER
    Mocha.runTests all
    #else
    runTestsInAssembly defaultConfig args
    #endif
