module Tournament.Client.Model

let rec qsort (xs: _ list) =
    match xs with
    | x::xs ->
        [
            yield! List.filter (fun y -> y < x) xs |> qsort
            yield x
            yield! List.filter (fun y -> y > x) xs |> qsort
        ]
    | [] -> []
