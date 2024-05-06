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
