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

    let joinTwoSortedLists isGreaterThan sorted1 sorted2 =
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
