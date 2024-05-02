module Tournament.Client.Model

module MergeSort =
    type Func<'Args, 'Result, 'Next> = 'Args * ((unit -> 'Result) -> 'Next)

    [<RequireQualifiedAccess>]
    type JoinTwoSortedLists<'a> =
        | IsGreaterThan of Func<('a * 'a), bool, JoinTwoSortedLists<'a>>
        | Result of 'a list
    module JoinTwoSortedLists =
        let start (sorted1: 'a list) (sorted2: 'a list) =
            let rec loop acc = function
                | x::xs, y::ys ->
                    JoinTwoSortedLists.IsGreaterThan ((x, y), fun isGreaterThan ->
                        if isGreaterThan () then
                            loop (y::acc) (x::xs, ys)
                        else
                            loop (x::acc) (xs, y::ys)
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
            loop [] (sorted1, sorted2)

    let joinTwoSortedLists isGreaterThan sorted1 sorted2 =
        let rec interp = function
            | JoinTwoSortedLists.IsGreaterThan ((x, y), getIsGreaterThan) ->
                getIsGreaterThan (fun () ->
                    isGreaterThan x y
                )
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
