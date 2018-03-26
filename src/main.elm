module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Dom exposing (..)
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline as Pipe exposing (decode, required)
import Json.Encode as Encode
import Task
import Ports


type alias Flags =
    { email : String
    , name : String
    , learnings : List String
    , myEmail : String
    }


main : Program Flags Model Msg
main =
    Html.programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { step : Steps
    , name : String
    , email : String
    , learning : String
    , learnings : List String
    , myEmail : String
    , password : String
    , submitted : Bool
    , status : Bool
    , error : String
    }


type Steps
    = Capture
    | Write
    | Send
    | Confirm
    | WithError


type Msg
    = InputName String
    | InputEmail String
    | SaveCustomer
    | InputLearning String
    | AddLearning
    | RemoveLearning String
    | SendTranscript
    | InputMyEmail String
    | InputPassword String
    | Submit
    | SubmitResult (Result Http.Error Bool)
    | FocusResult (Result Dom.Error ())


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputName n ->
            ( { model | name = n }, Cmd.none )

        InputEmail e ->
            ( { model | email = e }, Cmd.none )

        SaveCustomer ->
            ( { model | step = Write }, Cmd.none )

        InputLearning l ->
            ( { model | learning = l }, Cmd.none )

        AddLearning ->
            let
                updatedModel =
                    { model | learnings = model.learning :: model.learnings, learning = "" }
            in
                ( updatedModel, Cmd.batch [ setFocus, saveToStorage updatedModel ] )

        RemoveLearning l ->
            let
                updatedModel =
                    { model | learnings = removeLearning l model.learnings }
            in
                ( updatedModel, saveToStorage updatedModel )

        SendTranscript ->
            ( { model | step = Send }, Cmd.none )

        InputMyEmail e ->
            ( { model | myEmail = e }, Cmd.none )

        InputPassword p ->
            ( { model | password = p }, Cmd.none )

        Submit ->
            ( { model | submitted = True }, Cmd.batch [ submit model, saveToStorage model ] )

        SubmitResult (Ok status) ->
            let
                updatedModel =
                    { model
                        | status = status
                        , step = Confirm
                        , learnings = []
                        , email = ""
                        , name = ""
                    }
            in
                ( updatedModel, saveToStorage updatedModel )

        SubmitResult (Err e) ->
            ( { model | error = toString e, step = WithError }, Cmd.none )

        FocusResult result ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div [ class "container", style [ ( "padding-top", "95px" ) ] ]
        [ h1 [ class "title is-1" ] [ text "Customer research form" ]
        , case model.step of
            Capture ->
                captureView model

            Write ->
                writeView model

            Send ->
                sendView model

            Confirm ->
                confirmView model

            WithError ->
                errorView model
        , p [] []
        , p [ class "has-text-centered" ]
            [ a [ href "https://roadmap.space" ]
                [ img [ src "https://drofij53yrdlb.cloudfront.net/poweredby.png", height 50 ] []
                ]
            ]
        ]


captureView : Model -> Html Msg
captureView model =
    div []
        [ h3 [ class "subtitle is-3" ] [ text "Customer information" ]
        , p [ class "subtitle is-5" ] [ text "You may use this tool to capture learnings from a customer research call or interview." ]
        , p [ class "subtitle is-5" ]
            [ text "At the end of you call you will receive the transcript by email, and optionally you can push the learnings as product feedback to a Roadmap account." ]
        , p [ class "subtitle is-5" ] [ text "We will create an account automatically if you do not have one already." ]
        , div [ class "field" ]
            [ label [ class "label" ] [ text "Customer name" ]
            , div [ class "control" ] [ input [ class "input", onInput InputName, value model.name ] [] ]
            ]
        , div [ class "field" ]
            [ label [ class "label" ] [ text "Customer email" ]
            , div [ class "control" ] [ input [ class "input", onInput InputEmail, value model.email ] [] ]
            ]
        , button [ class "button is-primary", onClick SaveCustomer ] [ text "Continue" ]
        ]


writeView : Model -> Html Msg
writeView model =
    div []
        [ h3 [ class "subtitle is-3" ] [ text "Write your learnings" ]
        , div [ class "field" ]
            [ label [ class "label" ] [ text "Add a new learning" ]
            , div [ class "control" ] [ input [ id "learning", class "input", onInput InputLearning, value model.learning ] [] ]
            ]
        , p []
            [ button [ class "button is-success", onClick AddLearning ] [ text "Add learning" ]
            ]
        , learningsView model.learnings
        , p [] [ button [ class "button is-primary", onClick SendTranscript ] [ text "Get transcript" ] ]
        ]


learningsView : List String -> Html Msg
learningsView l =
    case l of
        [] ->
            div [ class "content" ]
                [ p [] []
                , p [ class "subtitle is-5", style [] ] [ text "Your learnings will be added here" ]
                , p [] []
                ]

        learnings ->
            div [] (List.map learningView l)


learningView : String -> Html Msg
learningView l =
    div [ class "card" ]
        [ div [ class "card-content" ]
            [ div [ class "media" ]
                [ div [ class "media-content" ] [ p [ class "title is-4" ] [ text l ] ]
                , div [ class "media-right" ] [ button [ onClick (RemoveLearning l) ] [ text "X" ] ]
                ]
            ]
        ]


sendView : Model -> Html Msg
sendView model =
    div [ class "content" ]
        [ h3 [ class "subtitle is-3" ] [ text "Get the transcript" ]
        , p [] [ text "We will send you the transcript via email." ]
        , div [ class "notification is-info" ] [ text "You may add each learnings as feedback to Roadmap. Use your existing email or we can create an account for you from here." ]
        , div [ class "field" ]
            [ label [ class "label" ] [ text "What's your email address" ]
            , div [ class "control" ] [ input [ class "input", onInput InputMyEmail, value model.myEmail ] [] ]
            ]
        , (roadmapView model)
        , p [] []
        , p [] [ button [ class "button is-primary", onClick Submit ] [ text "Process the data" ] ]
        ]


roadmapView : Model -> Html Msg
roadmapView model =
    div []
        [ h3 [ class "title is-2" ] [ text "Send to Roadmap" ]
        , p [] [ text "Roadmap can help you handle product feedback in an organized way. We can add each learning you've recorded here as product feedback in a centralized feedback inbox." ]
        , p [] [ text "From there your team can prioritize feedback and promote them to product ideas where you may get insights from internal and external users. Here's an example of the feedback inbox." ]
        , p [] [ img [ src "https://roadmap.space/img/tour/feedback-inbox.png", alt "Roadmap feedback inbox" ] [] ]
        , p [ class "notification is-info" ] [ text "Enter a password if you would want us to add to your existing account or create a new account for you." ]
        , div [ class "field" ]
            [ div [ class "control" ]
                [ label [ class "label" ] [ text "Roadmap password" ]
                , input [ class "input", type_ "password", onInput InputPassword, value model.password ] []
                ]
            ]
        ]


confirmView : Model -> Html Msg
confirmView model =
    div [ class "content" ]
        [ h3 [ class "subtitle is-3" ] [ text ("We've sent you the transcript to " ++ model.myEmail ++ ".") ]
        ]


errorView : Model -> Html Msg
errorView model =
    div [ class "content" ]
        [ h3 [ class "subtitle is-3" ] [ text "Shoot! Something wrong happened" ]
        , p [ class "notification is-warning" ] [ text model.error ]
        , p [] [ text "You may contact us via the chat widget on the bottom-right." ]
        , p [] [ text "Here's your learnings" ]
        , learningsView model.learnings
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { step = Capture
      , name = flags.name
      , email = flags.email
      , learning = ""
      , learnings = flags.learnings
      , myEmail = flags.myEmail
      , password = ""
      , submitted = False
      , status = False
      , error = ""
      }
    , Cmd.none
    )


removeLearning : String -> List String -> List String
removeLearning l learnings =
    let
        ne =
            notEqual l
    in
        List.filter ne learnings


notEqual : String -> String -> Bool
notEqual a b =
    a /= b


submit : Model -> Cmd Msg
submit model =
    let
        body =
            encodeBody model
                |> Http.jsonBody

        url =
            "https://app.roadmap.space/cr"

        request =
            Http.post url body decodeRequest
    in
        Http.send SubmitResult request


decodeRequest : Decode.Decoder Bool
decodeRequest =
    Decode.at [ "result" ] Decode.bool


encodeBody : Model -> Encode.Value
encodeBody model =
    Encode.object
        [ ( "name", Encode.string model.name )
        , ( "clientEmail", Encode.string model.email )
        , ( "learnings", Encode.list (List.map Encode.string model.learnings) )
        , ( "email", Encode.string model.myEmail )
        , ( "password", Encode.string model.password )
        ]


modelStorageChange : Sub (Maybe Model)
modelStorageChange =
    Ports.onStorageChange (Decode.decodeValue modelDecoder >> Result.toMaybe)


modelDecoder : Decode.Decoder Model
modelDecoder =
    decode Model
        |> Pipe.required "step" stepDecoder
        |> Pipe.required "name" Decode.string
        |> Pipe.required "email" Decode.string
        |> Pipe.required "learning" Decode.string
        |> Pipe.required "learnings" (Decode.list Decode.string)
        |> Pipe.required "myEmail" Decode.string
        |> Pipe.required "password" Decode.string
        |> Pipe.required "submitted" Decode.bool
        |> Pipe.required "status" Decode.bool
        |> Pipe.required "error" Decode.string


stepDecoder : Decode.Decoder Steps
stepDecoder =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "Capture" ->
                        Decode.succeed Capture

                    "Write" ->
                        Decode.succeed Write

                    "Send" ->
                        Decode.succeed Send

                    x ->
                        Decode.succeed Write
            )


modelEncoder : Model -> Encode.Value
modelEncoder model =
    Encode.object
        [ ( "step", Encode.string (stepString model.step) )
        , ( "name", Encode.string model.name )
        , ( "email", Encode.string model.email )
        , ( "learning", Encode.string model.learning )
        , ( "learnings", Encode.list (List.map Encode.string model.learnings) )
        , ( "myEmail", Encode.string model.myEmail )
        , ( "password", Encode.string model.password )
        , ( "submitted", Encode.bool model.submitted )
        , ( "status", Encode.bool model.status )
        , ( "error", Encode.string model.error )
        ]


stepString : Steps -> String
stepString step =
    case step of
        Capture ->
            "Capture"

        Write ->
            "Write"

        Send ->
            "Send"

        Confirm ->
            "Confirm"

        WithError ->
            "WithError"


saveToStorage : Model -> Cmd Msg
saveToStorage model =
    modelEncoder model
        |> Encode.encode 0
        |> Just
        |> Ports.storeModel


setFocus : Cmd Msg
setFocus =
    Task.attempt FocusResult (Dom.focus "learning")
