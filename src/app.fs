
module App

(**
 - title: Counter
 - tagline: The famous Increment/Decrement ported from Elm
*)
// on file upload https://stackoverflow.com/questions/43457646/convert-react-formevent-to-bodyinit-using-fable-elmish/43589876
open Fable.Core
open Fable.Import
open Elmish
type ImageURL = string
// MODEL

type Model = 
    | Iddle
    | LoadingImages
    | Images of ImageURL[]
    | ImagePicked of ImageURL * ImageURL[]
    | Error of string

type Msg =
    | LoadImages
    | LoadedImages of ImageURL[]
    | PickedImage of ImageURL
let init() : Model = Iddle

// UPDATE

let update (msg:Msg) (model:Model) =
    match msg with
    | PickedImage url ->
        match model with
        | Images images -> 
            ImagePicked (url, images)
        | ImagePicked (_, images) -> ImagePicked (url, images)
        | _ -> Error "Invalid state: Image was picked but i have no images list"
    | LoadedImages images ->
        let cats = [|
            "https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcTJB1DCcOWYml3Mni8ra2yppq-S0nrhG7Y0Zn7VMVzaC3Sw-W1Z"
            "https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcTGsNE90XVjKjGeVwu76dCyft44wphkhQJ6nJ7FMr9l2mCLB6Px_g"
            "https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcSiiu3AGeFA-5Jb6UVWIKQ5EfIvtI9Lp7QHV8loDP4YvBrVrJneEA"
            "https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcT99ACj8dZ91EXXS-bslXJg9gPv3lRzoqPTa1vv8qGcNi0shtNVhg"
        |]
    
        Images images
    | LoadImages -> LoadingImages


module View =
    open Fable.Core.JsInterop
    open Fable.Helpers.React.Props
    module R = Fable.Helpers.React

    let iddle signal =
        R.button [ OnClick <| signal LoadImages] [ R.str "Load images" ]
    
    let loadingImages () =
        R.str "Loading images. Please wait ..."

    let images signal imagesArray =
        imagesArray
        |> Array.map (fun image ->
            R.a [ OnClick <| signal (PickedImage image)] [ R.image [ Src image; Width (U2.Case1 200.)] []])
        |> Array.toList
        |> R.div []
    
    let imagePicked pickedImage images =
        R.div[] [
            images
            |> Array.toList
            |> List.map (fun image ->
                R.li [] [ 
                            (if image = pickedImage then "selected " else "") |> R.str
                            R.str image])
            |> R.ul []
            R.image [Src pickedImage][]
        ]
    
    let error message =
        R.str message

// VIEW (rendered with React)
let view model dispatch =
    let signal (msg: Msg) =
        fun _ -> dispatch msg
    match model with
    | Iddle -> View.iddle signal
    | LoadingImages -> View.loadingImages ()
    | Images images -> View.images signal images
    | ImagePicked (pickedImage, images) -> View.imagePicked pickedImage images
    | Error message -> View.error message
    |> List.singleton
    |> Fable.Helpers.React.div []

open Elmish.React

// App
Program.mkSimple init update view
|> Program.withConsoleTrace
|> Program.withReact "elmish-app"
|> Program.run
