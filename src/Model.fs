module Tournament.Client.Model

module MergeSort =
    type Func<'Args, 'Result, 'Next> = 'Args * ((unit -> 'Result) -> 'Next)

    module JoinTwoSortedLists =
        type Loop<'a> =
            {
                Acc: 'a list
                TwoLists: 'a list * 'a list
            }

        type IsGreaterThan<'a> =
            {
                Xs: 'a * 'a list
                Ys: 'a * 'a list
                Acc: 'a list
            }

        [<RequireQualifiedAccess>]
        type Main<'a> =
            | Loop of Loop<'a>
            | IsGreaterThanReq of ('a * 'a) * IsGreaterThan<'a>
            | Result of 'a list

        module IsGreaterThan =
            let exec isGreaterThan (data: IsGreaterThan<'a>) =
                let { Xs = x, xs; Ys = y, ys; Acc = acc } = data
                if isGreaterThan then
                    Main.Loop {
                        Acc = y::acc
                        TwoLists = x::xs, ys
                    }
                else
                    Main.Loop {
                        Acc = x::acc
                        TwoLists = xs, y::ys
                    }

        module Loop =
            let exec { Acc = acc; TwoLists = twoLists } =
                match twoLists with
                | x::xs, y::ys ->
                    Main.IsGreaterThanReq (
                        (x, y),
                        {
                            Xs = x, xs
                            Ys = y, ys
                            Acc = acc
                        }
                    )
                | [], ys ->
                    Main.Result (
                        List.fold (fun st y -> y::st) acc ys
                        |> List.rev
                    )
                | xs, [] ->
                    Main.Result (
                        List.fold (fun st x -> x::st) acc xs
                        |> List.rev
                    )

        let start (sorted1: 'a list) (sorted2: 'a list) =
            Loop.exec {
                Acc = []
                TwoLists = sorted1, sorted2
            }

    module JoinSortedLists =
        type 'a Loop =
            {
                Xss: 'a list list
                Acc: 'a list list
            }

        [<RequireQualifiedAccess>]
        type Main<'a> =
            | JoinTwoSortedListsReq of JoinTwoSortedLists.Main<'a> * 'a Loop
            | Loop of Loop<'a>
            | Result of 'a list list

        module Loop =
            let exec { Acc = acc; Xss = xss } =
                match xss with
                | xs::ys::xss ->
                    Main.JoinTwoSortedListsReq (
                        JoinTwoSortedLists.start xs ys,
                        {
                            Acc = acc
                            Xss = xss
                        }
                    )
                | [xs] ->
                    Main.Result (
                        List.rev (xs::acc)
                    )
                | [] ->
                    Main.Result (
                        List.rev acc
                    )

        module JoinTwoSortedListsReq =
            let exec zs { Acc = acc; Xss = xss } =
                Main.Loop {
                    Acc = zs::acc
                    Xss = xss
                }

        let start (xss: 'a list list) =
            Loop.exec {
                Acc = []
                Xss = xss
            }

    let joinTwoSortedListsInterp isGreaterThan cmd =
        let rec interp = function
            | JoinTwoSortedLists.Main.IsGreaterThanReq ((x, y), data) ->
                data
                |> JoinTwoSortedLists.IsGreaterThan.exec (isGreaterThan x y)
                |> interp
            | JoinTwoSortedLists.Main.Loop loopArgs ->
                JoinTwoSortedLists.Loop.exec loopArgs
                |> interp
            | JoinTwoSortedLists.Main.Result result ->
                result
        interp cmd

    let joinTwoSortedLists isGreaterThan sorted1 sorted2 =
        joinTwoSortedListsInterp
            isGreaterThan
            (JoinTwoSortedLists.start sorted1 sorted2)

    let joinSortedLists isGreaterThan xss =
        let rec interp = function
            | JoinSortedLists.Main.Loop loop ->
                JoinSortedLists.Loop.exec loop
                |> interp
            | JoinSortedLists.Main.JoinTwoSortedListsReq (joinTwoSortedListsCmd, data) ->
                let result =
                    joinTwoSortedListsInterp
                        isGreaterThan
                        joinTwoSortedListsCmd
                data
                |> JoinSortedLists.JoinTwoSortedListsReq.exec result
                |> interp
            | JoinSortedLists.Main.Result xss ->
                xss
        xss
        |> JoinSortedLists.start
        |> interp

    let start (xs: 'a list) =
        let isGreaterThan x y =
            x > y

        let rec loop = function
            | [xs] -> xs
            | [] -> []
            | xss -> loop (joinSortedLists isGreaterThan xss)

        loop (List.map (fun x -> [x]) xs)

let rec qsort (xs: _ list) =
    match xs with
    | x::xs ->
        let lowers, greaters = List.partition (fun y -> y < x) xs
        [
            yield! qsort lowers
            yield x
            yield! qsort greaters
        ]
    | [] -> []
