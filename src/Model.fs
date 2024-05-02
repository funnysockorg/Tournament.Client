module Tournament.Client.Model

module MergeSort =
    type Func<'Args, 'Result, 'Next> = 'Args * ((unit -> 'Result) -> 'Next)

    type JoinTwoSortedListsLoopArgs<'a> =
        {
            Acc: 'a list
            TwoLists: 'a list * 'a list
        }

    type JoinTwoSortedListsIsGreaterThanData<'a> =
        {
            Xs: 'a * 'a list
            Ys: 'a * 'a list
            Acc: 'a list
        }

    [<RequireQualifiedAccess>]
    type JoinTwoSortedLists<'a> =
        | Loop of JoinTwoSortedListsLoopArgs<'a>
        | IsGreaterThanReq of ('a * 'a) * JoinTwoSortedListsIsGreaterThanData<'a>
        | Result of 'a list
    module JoinTwoSortedLists =
        let isGreaterThanContinue isGreaterThan (data: JoinTwoSortedListsIsGreaterThanData<'a>) =
            let { Xs = x, xs; Ys = y, ys; Acc = acc } = data
            if isGreaterThan then
                JoinTwoSortedLists.Loop {
                    Acc = y::acc
                    TwoLists = x::xs, ys
                }
            else
                JoinTwoSortedLists.Loop {
                    Acc = x::acc
                    TwoLists = xs, y::ys
                }

        let loop { Acc = acc; TwoLists = twoLists } =
            match twoLists with
            | x::xs, y::ys ->
                JoinTwoSortedLists.IsGreaterThanReq (
                    (x, y),
                    {
                        Xs = x, xs
                        Ys = y, ys
                        Acc = acc
                    }
                )
            | [], ys ->
                JoinTwoSortedLists.Result (
                    List.fold (fun st y -> y::st) acc ys
                    |> List.rev
                )
            | xs, [] ->
                JoinTwoSortedLists.Result (
                    List.fold (fun st x -> x::st) acc xs
                    |> List.rev
                )

        let start (sorted1: 'a list) (sorted2: 'a list) =
            loop {
                Acc = []
                TwoLists = sorted1, sorted2
            }

    let joinTwoSortedLists isGreaterThan sorted1 sorted2 =
        let rec interp = function
            | JoinTwoSortedLists.IsGreaterThanReq ((x, y), data) ->
                data
                |> JoinTwoSortedLists.isGreaterThanContinue (isGreaterThan x y)
                |> interp
            | JoinTwoSortedLists.Loop loopArgs ->
                JoinTwoSortedLists.loop loopArgs
                |> interp
            | JoinTwoSortedLists.Result result ->
                result
        interp (JoinTwoSortedLists.start sorted1 sorted2)

    let joinSortedLists isGreaterThan (xs: 'a list list) =
        // xs
        // |> List.chunkBySize 2
        // |> List.map (function
        //     | [xs; ys] ->
        //         joinTwoSortedLists isGreaterThan xs ys
        //     | [xs] -> xs
        //     | [] -> []
        //     | xs -> failwithf "%A length is greater than 2!" xs
        // )
        let rec loop acc = function
            | xs::ys::xss ->
                let ys =
                    joinTwoSortedLists isGreaterThan xs ys
                loop (ys::acc) xss
            | [xs] -> xs::acc
            | [] -> acc
        loop [] xs |> List.rev

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
