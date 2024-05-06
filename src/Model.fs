namespace Tournament.Client.Model

type User =
    {
        Id: string
        AvatarUrl: string
        Nickname: string
    }

module User =
    let mock : User =
        {
            Id = "123456"
            Nickname = "Guest"
            AvatarUrl = ""
        }

type Participant =
    {
        Name: string
    }

module ParticipantsStorage =
    let mocks : Participant list =
        [ 10; 20; 1; 4; 5; 3 ]
        |> List.map (fun n ->
            { Name = sprintf "%d" n }
        )

module MergeSort =
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

    module Start =
        [<RequireQualifiedAccess>]
        type Main<'a> =
            | JoinSortedLists of JoinSortedLists.Main<'a>
            | Result of 'a list

        let loop = function
            | [xs] ->
                Main.Result xs
            | [] ->
                Main.Result []
            | xss ->
                Main.JoinSortedLists (
                    JoinSortedLists.start xss
                )

        let start (xs: 'a list) =
            loop (List.map (fun x -> [x]) xs)

    let joinTwoSortedListsInterp cmd =
        let rec interp = function
            | JoinTwoSortedLists.Main.IsGreaterThanReq ((x, y), data) ->
                JoinTwoSortedLists.Main.IsGreaterThanReq ((x, y), data)
                |> Choice1Of2
            | JoinTwoSortedLists.Main.Loop loopArgs ->
                JoinTwoSortedLists.Loop.exec loopArgs
                |> interp
            | JoinTwoSortedLists.Main.Result result ->
                Choice2Of2 result
        interp cmd

    let joinSortedListsInterp xss =
        let rec interp = function
            | JoinSortedLists.Main.Loop loop ->
                JoinSortedLists.Loop.exec loop
                |> interp
            | JoinSortedLists.Main.JoinTwoSortedListsReq (joinTwoSortedListsCmd, data) ->
                match joinTwoSortedListsInterp joinTwoSortedListsCmd with
                | Choice1Of2 joinTwoSortedListsCmd ->
                    JoinSortedLists.Main.JoinTwoSortedListsReq (joinTwoSortedListsCmd, data)
                    |> Choice1Of2
                | Choice2Of2 result ->
                    data
                    |> JoinSortedLists.JoinTwoSortedListsReq.exec result
                    |> interp
            | JoinSortedLists.Main.Result xss ->
                Choice2Of2 xss
        interp xss

    let startInterp x =
        let rec interp = function
            | Start.Main.JoinSortedLists xss ->
                match joinSortedListsInterp xss with
                | Choice1Of2 msg ->
                    Start.Main.JoinSortedLists msg
                | Choice2Of2 result ->
                    result
                    |> Start.loop
                    |> interp
            | Start.Main.Result sortedList ->
                Start.Main.Result sortedList
        interp x

    [<RequireQualifiedAccess>]
    type Choice =
        | Left
        | Right

    let create xs =
        Start.start xs

    let choice choiceArg state =
        match state with
        | Start.Main.JoinSortedLists x ->
            match x with
            | JoinSortedLists.Main.JoinTwoSortedListsReq (joinTwoSortedListsCmd, loop) ->
                match joinTwoSortedListsCmd with
                | JoinTwoSortedLists.Main.IsGreaterThanReq ((x, y), cmd) ->
                    let isGreaterThan =
                        match choiceArg with
                        | Choice.Left -> true
                        | Choice.Right -> false
                    let joinTwoSortedListsCmd =
                        cmd
                        |> JoinTwoSortedLists.IsGreaterThan.exec isGreaterThan
                    JoinSortedLists.Main.JoinTwoSortedListsReq (joinTwoSortedListsCmd, loop)
                    |> Start.Main.JoinSortedLists
                    |> startInterp
                | _ ->
                    startInterp state
            | _ ->
                startInterp state
        | _ ->
            startInterp state

    let getCurrentCandidates = function
        | Start.Main.JoinSortedLists x ->
            match x with
            | JoinSortedLists.Main.JoinTwoSortedListsReq (joinTwoSortedListsCmd, _) ->
                match joinTwoSortedListsCmd with
                | JoinTwoSortedLists.Main.IsGreaterThanReq ((x, y), _) ->
                    Some (x, y)
                | _ ->
                    None
            | _ ->
                None
        | _ ->
            None

    let getResult = function
        | Start.Main.Result xs ->
            Some xs
        | _ ->
            None
