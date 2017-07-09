import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Json.Decode exposing (..)
import Dom exposing (focus)
import Task exposing (attempt)
import Todos exposing (..)
import Api exposing (..)

main : Program Never AppModel Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = (\_ -> Sub.none) }

-- AppModel

type alias AppModel =
    { lists : List TodoList
    , activeList : Maybe TodoList
    , activeTodo : Maybe Todo
    , newTitle : String
    , enteringNewTitle : Bool
    , newName : String
    , error : Maybe String }

init : (AppModel, Cmd Msg)
init =
    (AppModel 
        []
        Nothing
        Nothing
        ""
        False
        "" 
        Nothing
    , getAllLists GotLists)


updateTodoList : AppModel -> TodoList -> AppModel
updateTodoList model list =
    let
        update lists list =
            case lists of
            l :: rest ->
                if l.name == list.name 
                then list :: rest
                else l :: update rest list
            [] -> []
    in
        { model | lists = update model.lists list }

type Msg
    = AddTodoButtonClick TodoList
    | DeleteTodo TodoList Int
    | SetDone TodoList Todo Bool
    | SetTodoTitle TodoList Todo String
    | StartEditingTodo Todo
    | StopEditingTodo
    | NewTitle String
    | AddTodoInputKeyPress TodoList Int
    | EditTodoInputKeyPress Todo Int
    | FocusResult (Result Dom.Error ())
    | GotTodo TodoList (ApiResult Todo)
    | DeletedTodo TodoList (Result Api.Error Int)
    | UpdatedTodo TodoList (Result Api.Error Todo)
    | CreateList
    | DeleteList String
    | NewName String
    | SetActiveList TodoList
    | GotList (Result Api.Error TodoList)
    | GotLists (Result Api.Error (List TodoList))
    | DeletedList (Result Api.Error String)
    | NoOp

error : AppModel -> String -> AppModel
error model message =
    { model | error = Just message }

getError : Api.Error -> String
getError err =
    case err of
    Other s ->
        s
    ApiNotAvailable ->
        "Todo API is unavailable"
    NoSuchEntity ->
        "Todo or List does not exist"


-- Update

todoId : Todo -> String
todoId todo =
    "todo-" ++ toString todo.id

updatePostTodo : AppModel -> TodoList -> (AppModel, Cmd Msg)
updatePostTodo model list =
    let
        title =
            model.newTitle
        cmd =
            postTodo (GotTodo list) list title
    in
        ({ model | newTitle = "" }, cmd)

updatePostList : AppModel -> (AppModel, Cmd Msg)
updatePostList model =
    let
        name = model.newName
    in
        ({ model | newName = "" }, postList GotList name)

updateSetError : AppModel -> Api.Error -> (AppModel, Cmd Msg)
updateSetError model error =
    ({ model | error = Just (getError error) }, Cmd.none)

update : Msg -> AppModel -> (AppModel, Cmd Msg)
update msg model =
    case msg of
    DeleteTodo list id ->
        (model, deleteTodo (DeletedTodo list) list id)
    SetDone list todo done ->
        (model, putTodo (GotTodo list) list { todo | done = done })
    SetTodoTitle list todo title ->
        (model, putTodo (GotTodo list) list { todo | title = title })
    StartEditingTodo todo ->
        ({ model | activeTodo = Just todo }, todoId todo |> focus |> attempt FocusResult)
    StopEditingTodo ->
        ({ model | activeTodo = Nothing }, Cmd.none)
    NewTitle s ->
        ({ model | newTitle = s }, Cmd.none)
    AddTodoButtonClick list ->
        updatePostTodo model list
    AddTodoInputKeyPress list keyCode ->
        if keyCode == 13
        then updatePostTodo model list
        else (model, Cmd.none)
    GotTodo list (Ok todo) ->
        (addOrUpdateTodo list todo |> (updateTodoList model), Cmd.none)
    DeletedTodo list (Ok id) ->
        (removeTodo list id |> (updateTodoList model), Cmd.none)
    UpdatedTodo list (Ok todo) ->
        (addOrUpdateTodo list todo |> (updateTodoList model), Cmd.none)
    FocusResult result ->
        case result of
        Err (Dom.NotFound id) ->
            (error model id, Cmd.none)
        Ok id ->
            (model, Cmd.none)
    CreateList ->
        updatePostList model
    DeleteList listName ->
        (model, deleteList DeletedList listName)
    NewName s ->
        ({ model | newName = s }, Cmd.none)
    SetActiveList list ->
        ({ model | activeList = Just list }, Cmd.none)
    GotList (Ok list) ->
        ({ model | lists = list :: model.lists }, Cmd.none)
    GotLists (Ok lists) ->
        ({ model | lists = model.lists ++ lists }, Cmd.none)
    DeletedList (Ok listName) ->
        ({ model | lists = List.filter (\l -> not <| l.name == listName) model.lists }, Cmd.none)
    EditTodoInputKeyPress todo key ->
        (model, Cmd.none)
    GotList (Err e) ->
        updateSetError model e
    GotLists (Err e) ->
        updateSetError model e
    DeletedList (Err e) ->
        updateSetError model e
    GotTodo list (Err e) ->
        updateSetError model e
    DeletedTodo list (Err e) ->
        updateSetError model e
    UpdatedTodo list (Err e) ->
        updateSetError model e
    NoOp ->
        (model, Cmd.none)


-- View

row : List (Html Msg) -> Html Msg
row content =
    div [ class "row" ] content

col : String -> List (Html Msg) -> Html Msg
col size content =
    div [ class ("col-" ++ size) ] content

view : AppModel -> Html Msg
view model =
    div [ class "container" ] 
        ([row
            [ col "md-12"
                [ h1 [] [ text "" ]
                , viewError model
                ]
            ]
        , row
            [ col "md-4"
                [ div [ class "input-group" ]
                    [ input [ class "form-control", type_ "text", onInput NewName, Html.Attributes.value model.newName ] []
                    , span [ class "input-group-btn"] 
                        [ button [ class "btn btn-success", onClick CreateList ] [ text "New list" ] ]
                    ]
                ]
            ]
        ] ++ (List.map (viewTodoList model) model.lists))

viewError : AppModel -> Html Msg
viewError model =
    case model.error of
        Maybe.Just message ->
            h2 [] [ text ("Error: " ++ message) ]
        _ ->
            h2 [ style [("display", "none")] ] []

glyphicon : String -> List (Html.Attribute Msg) -> Html Msg
glyphicon name attributes =
    span ([ class ("glyphicon glyphicon-" ++ name) ] ++ attributes) [ ]

viewTodoList : AppModel -> TodoList -> Html Msg
viewTodoList model todoList =
    let
        title =
            case Maybe.map (\l -> l.name == todoList.name) model.activeList of
            Just True ->
                model.newTitle
            _ ->
                ""
    in
        row
            [ col "md-12"
                [ row
                    [ col "md-2"
                        [ h3 [] [ text todoList.name ] ]
                    , col "md-2"
                        [ glyphicon "trash" [ style [ ("cursor", "pointer") ], onClick (DeleteList todoList.name) ] ]
                    ]
                , row
                    [ col "md-12" 
                        [ ul [] (List.filter (not << isNull) todoList.todos |> List.map (viewTodo model todoList)) ]
                    ]
                , row
                    [ col "md-4"
                        [ div [ class "input-group" ]
                            [ input 
                                [ type_ "text"
                                , class "form-control"
                                , onFocus (SetActiveList todoList)
                                , onInput NewTitle
                                , onKeyDown (AddTodoInputKeyPress todoList)
                                , Html.Attributes.value title ]
                                []
                            , span [ class "input-group-btn"] 
                                [ button [ class "btn btn-success", onClick (AddTodoButtonClick todoList) ] [ text "Add" ] ]
                            ]
                        ]
                    ]
                ]
            ]

onKeyDown : (Int -> msg) -> Attribute msg
onKeyDown tagger =
    on "keydown" (Json.Decode.map tagger keyCode)

onSpecificKeyDown : List Int -> (Int -> msg) -> msg -> Attribute msg
onSpecificKeyDown keys tagger fallback =
    on "keydown" (Json.Decode.map (\i -> if (List.member i keys) then tagger i else fallback) keyCode) 

viewTodo : AppModel -> TodoList -> Todo -> Html Msg
viewTodo model list todo =
    li [] 
        [ viewTodoTitle model list todo
        , input [ type_ "checkbox", checked todo.done, onCheck (SetDone list todo) ] []
        , a [ onClick (DeleteTodo list todo.id), href "#" ] [ text "delete" ] ]

viewTodoTitle : AppModel -> TodoList -> Todo -> Html Msg
viewTodoTitle model list todo =
    case Maybe.map (\t -> t.id == todo.id) model.activeTodo of
    Just True ->
        div []
            [ input 
                [ id (todoId todo), type_ "text", Html.Attributes.value todo.title, onInput (SetTodoTitle list todo) ]
                []
            , glyphicon "ok" 
                [ style [ ("color", "green"), ("cursor", "pointer") ]
                , onClick StopEditingTodo
                , onSpecificKeyDown [ 13 ] (\_ -> StopEditingTodo) NoOp ]
            ]
        
    _ ->
        span 
            [ style [ ("text-decoration", if todo.done then "line-through" else "") ]
            , onClick (StartEditingTodo todo) ]
            [ text todo.title ]