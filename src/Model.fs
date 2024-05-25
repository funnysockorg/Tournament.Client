namespace Tournament.Client.Model

type User =
    {
        Id: string
        /// the user's avatar hash, through which you can get a link to your avatar:
        ///
        /// ```bash
        /// https://cdn.discordapp.com/avatars/$user_id/$user_avatar.png
        /// ```
        Avatar: string
        Nickname: string
    }

module User =
    open Thoth.Json

    let mock : User =
        {
            Id = "123456"
            Nickname = "Guest"
            Avatar = ""
        }

    let decoder : Decoder<User> =
        Decode.Auto.generateDecoder(CaseStrategy.CamelCase)

    let decode rawJson =
        Decode.fromString decoder rawJson

    let encoder : Encoder<User> =
        Encode.Auto.generateEncoder(CaseStrategy.CamelCase)

    let encode space (items: User) =
        encoder items
        |> Encode.toString space

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
