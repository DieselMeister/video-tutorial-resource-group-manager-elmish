module DeleteDialog

open Domain
open Elmish


type Model = {
    ResourceGroup:ResourceGroup
    DeleteConfirmationName:string
}


type Msg =
    | ChangeDeleteConfirmationText of string
    | TriggerResourceGroupDelete of ResourceGroup
    | ResourceGroupDeletionTriggered of string
    | CloseDeleteDialog
    | OnError of string
    



module Commands =

    let deleteResourceGroupCmd resourceGroup =
        async {
            try
                let account = Authentication.userAgent.getAccount()
                let authParams = Authentication.getAuthenticationParameters(Some account)
                let! authResponse = Authentication.aquireToken(authParams)
                let! result = WebAccess.deleteResourceGroup authResponse.accessToken resourceGroup.Id
                match result with
                | Ok result ->
                    return (ResourceGroupDeletionTriggered <| sprintf "resource '%s' group deleted!" resourceGroup.Name)
                | Error error ->
                    return (OnError error)
            with
            | _ as ex ->
                return (OnError ex.Message)
        }
        |> Cmd.OfAsync.result



let init resourceGroup =
    {
        ResourceGroup = resourceGroup
        DeleteConfirmationName = ""
    }, Cmd.none


let update msg model =
    match msg with
    | ChangeDeleteConfirmationText text ->
        { model with DeleteConfirmationName = text}, Cmd.none
    | TriggerResourceGroupDelete resourceGroup ->
        model, Commands.deleteResourceGroupCmd resourceGroup
    | ResourceGroupDeletionTriggered message ->
        model, Cmd.none
    | CloseDeleteDialog ->
        model, Cmd.none
    | OnError _ ->
        model, Cmd.none


open Feliz
open Feliz.Bulma

let renderFullColumn content =
    Bulma.columns [
        Bulma.column [
            column.isFull
            prop.children [
                yield! content
            ]
        ]
    ]


let render model dispatch =
    Html.div [
        Bulma.modal [
            modal.isActive
            prop.children [
                Bulma.modalBackground [ ]
                Bulma.modalContent [
                    Bulma.message [
                        message.isDanger
                        prop.children [
                            Bulma.messageHeader [
                                Html.p (sprintf "Delete Resource Group '%s'!" model.ResourceGroup.Name)
                                Bulma.delete [
                                    prop.onClick (fun _ -> dispatch CloseDeleteDialog)
                                ]
                            ]

                            Bulma.messageBody [
                                renderFullColumn [
                                    Html.p "to delete the resource group, you have to enter the name."
                                ]
                                renderFullColumn [
                                    Bulma.textInput [
                                        prop.onTextChange (fun t -> dispatch <| ChangeDeleteConfirmationText t)
                                            
                                    ]
                                ]
                                renderFullColumn [
                                    Bulma.button [
                                        helpers.isPulledRight
                                        button.isDanger
                                        prop.text "Delete"
                                        prop.disabled (model.ResourceGroup.Name <> model.DeleteConfirmationName)
                                        prop.onClick (fun _ -> dispatch <| TriggerResourceGroupDelete model.ResourceGroup)
                                    ]

                                    Bulma.button [
                                        helpers.isPulledLeft
                                        prop.text "Cancel"
                                        prop.onClick (fun _ -> dispatch CloseDeleteDialog)
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]
    
    ]

