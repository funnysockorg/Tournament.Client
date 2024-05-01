module Tournament.Client.Model

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
