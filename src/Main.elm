module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Debug exposing (..)
import  Array exposing (..)

---- MODEL ----
type alias Entry =
    { 
        id : Int, 
        description : String
    }

newEntry : Int -> String -> Entry
newEntry id desc =
    {
        id = id,
        description = desc
    }

type alias Model =
    {
        todoList: List Entry,
        inputField: String,
        id: Int
    }

todos: List Entry
todos = []

init : ( Model, Cmd Msg )
init =
    ( {
        todoList = todos,
        inputField = "",
        id = (List.maximum (List.map (\t -> t.id) todos) |> Maybe.withDefault 0) + 1
    }, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp
    | Delete Int
    | AddTodo
    | UpdateInputField String

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )
    
        UpdateInputField str ->
            (
                { model | inputField = str }
                , Cmd.none
            )
        
        AddTodo ->
            log model.inputField
            (
                {
                    model
                    | id = model.id + 1
                    , inputField = ""
                    , todoList = 
                        if String.isEmpty model.inputField then
                            model.todoList
                        else
                            model.todoList ++ [newEntry model.id model.inputField]
                }
                , Cmd.none
            )
        Delete id ->
            (
                { model | todoList = List.filter (\t -> t.id /= id) model.todoList }
                , Cmd.none
            )



---- VIEW ----


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ img [ class "logo",src "/todos.png" ] []
        , span [ class "header"] [ text "Hoang's todo list" ]
        , div [ class "input-group"]
              [ input [ value model.inputField, class "form-control", placeholder "Your new task", onInput UpdateInputField ] []
               ,div [ class "input-group-append", onClick AddTodo ]
                    [ button [ class "btn btn-primary" ] [ text "Add"]]
              ]
        , br [] []
        , ul [ class "list-group" ]
             <| List.map renderTodo model.todoList  
        ]

renderTodo : Entry -> Html Msg
renderTodo todo = 
    li  [ class "list-group-item"]
        [ text todo.description
        , span [ class "text-danger float-right clickable", onClick (Delete todo.id) ]
            [ text "Delete"]
        ]

---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
