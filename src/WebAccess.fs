module WebAccess


open Fable.SimpleHttp
open Domain

let getResourceGroups token =
    async {
        let! response =
            Http.request "https://management.azure.com/subscriptions/77d9206d-4aa9-4ebb-a46c-6859ab85a638/resourcegroups?$filter=tagName eq 'fancy-tag'&api-version=2019-10-01"
            |> Http.method GET
            |> Http.header (Headers.authorization ("Bearer " + token))
            |> Http.header (Headers.contentType "application/json")
            |> Http.send

        if response.statusCode = 200 then
            let result = 
                response.responseText
                |> Thoth.Json.Decode.fromString (AzureResult.Decoder ResourceGroup.Decoder)
            return result
        else
            let errorMessage = sprintf "error getting resource groups: %i - %s" response.statusCode response.responseText
            return Error errorMessage
    }

        
let deleteResourceGroup token id =
    async {
        let! response =
            Http.request (sprintf "https://management.azure.com%s?api-version=2019-10-01" id)
            |> Http.method DELETE
            |> Http.header (Headers.authorization ("Bearer " + token))
            |> Http.header (Headers.contentType "application/json")
            |> Http.send

        if response.statusCode = 202 then
            return Ok ()
        else
            let errorMessage = sprintf "error delete resource groups: %i - %s" response.statusCode response.responseText
            return Error errorMessage
    }

