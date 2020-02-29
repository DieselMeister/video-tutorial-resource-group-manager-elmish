module Domain


open Thoth.Json

type AzureResult<'a> = 
    {
        Value: 'a list
    }
    static member Decoder innerDecoder : Decoder<AzureResult<'a>> =
        Decode.object(fun get ->
            {
                Value = get.Required.Field "value" (Decode.list innerDecoder)
            }
        )


type ResourceGroup = 
    {
        Id:string
        Name:string
        Location:string
        Tags:Map<string,string>
    }
    static member Decoder : Decoder<ResourceGroup> =
        Decode.object(fun get ->
            {
                Id = get.Required.Field "id" Decode.string
                Name = (get.Optional.Field "name" Decode.string) |> Option.defaultValue ""
                Location = get.Required.Field "location" Decode.string
                Tags = 
                    (get.Optional.Field "tags" (Decode.keyValuePairs Decode.string)) // (string * string) list option
                    |> Option.defaultValue [] 
                    |> Map
            }
        )



type ResoureGroupResult = AzureResult<ResourceGroup>

