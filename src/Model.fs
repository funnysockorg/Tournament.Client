module Tournament.Client.Model

module MergeSort =
    let joinTwoSortedLists isGreaterThan (sorted1: 'a list) (sorted2: 'a list) : 'a list =
        let rec loop acc = function
            | x::xs, y::ys ->
                if isGreaterThan x y then
                    loop (y::acc) (x::xs, ys)
                else
                    loop (x::acc) (xs, y::ys)
            | [], ys ->
                List.fold (fun st y -> y::st) acc ys
            | xs, [] ->
                List.fold (fun st x -> x::st) acc xs
        loop [] (sorted1, sorted2) |> List.rev

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
