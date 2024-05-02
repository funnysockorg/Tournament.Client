module Tournament.Client.Model

let mergeSort (xs: 'a list) =
    let isGreaterThan x y =
        x > y

    let splitTwo xs =
        let rec loop acc = function
            | x::y::xs ->
                let ys =
                    if isGreaterThan x y then
                        [y; x]
                    else
                        [x; y]
                loop (ys::acc) xs
            | [x] -> [x]::acc
            | [] -> acc
        loop [] xs |> List.rev

    let joinTwo (sorted1: 'a list, sorted2: 'a list) : 'a list =
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

    let join (xs: 'a list list) =
        let rec loop acc = function
            | xs::ys::xss ->
                let ys =
                    joinTwo (xs, ys)
                loop (ys::acc) xss
            | [xs] -> xs::acc
            | [] -> acc
        loop [] xs |> List.rev

    let rec loop = function
        | [xs] -> xs
        | [] -> []
        | xss -> loop (join xss)

    loop (splitTwo xs)

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
