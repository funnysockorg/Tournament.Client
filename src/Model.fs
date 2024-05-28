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

type ParticipantAvatar =
    {
        Src: string
        X: float
        Y: float
        /// Assume:
        ///
        /// * Source image size 498x498
        /// * Destination viewport size 48x48
        ///
        /// Then the scale is calculated as follows:
        ///
        /// ```text
        /// 1 / 498 = x / 48
        /// x = 48 / 498
        /// x = 0.0963855421686747
        /// ```
        Scale: float
    }

type Participant =
    {
        Name: string
        Avatar: ParticipantAvatar option
    }

module ParticipantsStorage =
    let mocks : Participant list =
        let unicornSocks =
            {
                Name = "unicornSocks"
                Avatar =
                    Some {
                        Src = "https://c.tenor.com/cFlKcN77kQMAAAAC/tenor.gif"
                        X = 0
                        Y = 0
                        // size = 498x498
                        Scale = 0.0963855421686747
                    }
            }
        let clawsSocks =
            {
                Name = "clawsSocks"
                Avatar =
                    let w, h = 398, 498
                    let scale = 48. / float h
                    Some {
                        Src = "https://c.tenor.com/2Pg7sAD2TBMAAAAC/tenor.gif"
                        X = 48. / 2. - (float w * scale) / 2.
                        Y = 0
                        Scale = scale
                    }
            }
        let withoutAvatars =
            [ 20; 1; 4; 5; 3 ]
            |> List.map (fun n ->
                {
                    Name = sprintf "%d" n
                    Avatar = None
                }
            )
        clawsSocks :: unicornSocks :: withoutAvatars
