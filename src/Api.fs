module Tournament.Client.Api
open Fable.Core
open Fetch.Types

open Tournament.Client.Model

let clientId = "1237078496866734120"
let apiHost = "http://localhost:4010/api"
let redirectUri = sprintf "%s/discord/callback" apiHost

let discordSignInUrl =
    let query =
        [
            "client_id", clientId
            "response_type", "code"
            "redirect_uri", redirectUri
            "scope", "identify"
        ]
        |> List.map (fun (name, value) ->
            sprintf "%s=%s" name (JS.encodeURIComponent value)
        )
        |> String.concat "&"
    sprintf "https://discord.com/oauth2/authorize?%s" query

type ApiAuthToken = string
type StatusCode = int
type FetchError =
    {
        StatusCode: StatusCode
        Body: string
    }

module AuthCallback =
    type DiscordAuthCode = string

    type Response =
        Result<ApiAuthToken, FetchError>

    let request (code: DiscordAuthCode) : JS.Promise<Response> =
        promise {
            let! result =
                Fetch.fetchUnsafe (sprintf "%s/discord/auth/callback?code=%s" apiHost code) [
                    Method HttpMethod.GET
                ]
            match result.Status with
            | 200 ->
                let! body = result.text()
                return Ok body
            | statusCode ->
                let! body = result.text()
                return Error {
                    StatusCode = statusCode
                    Body = body
                }
        }

[<RequireQualifiedAccess>]
type ResponseError =
    | ParseError of string
    | FetchError of FetchError

let getCurrentUser (authToken: ApiAuthToken) : JS.Promise<_> =
    promise {
        let! result =
            Fetch.fetchUnsafe (sprintf "%s/discord/get-user-data" apiHost) [
                Method HttpMethod.GET
                Fetch.requestHeaders  [
                    Authorization authToken
                ]
            ]
        match result.Status with
        | 200 ->
            let! body = result.text()
            return
                User.decode body
                |> Result.mapError ResponseError.ParseError
        | statusCode ->
            let! body = result.text()
            return
                ResponseError.FetchError {
                    StatusCode = statusCode
                    Body = body
                }
                |> Error
    }
