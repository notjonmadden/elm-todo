module Api exposing
    ( Error 
        ( NoSuchEntity
        , ApiNotAvailable
        , Other
        )
    , ApiResult
    , Tagger
    , getAllLists
    , postList
    , deleteList
    , postTodo
    , deleteTodo
    , putTodo
    )

import Todos exposing (..)
import Http exposing (..)
import Json.Decode exposing (..)

type Error
    = NoSuchEntity
    | ApiNotAvailable
    | Other String

type alias ApiResult a =
    Result Error a

type alias Tagger a msg =
    ApiResult a -> msg

type alias RequestLite a =
    { expect : Expect a
    , method : String 
    , route : String }

createRequest : RequestLite a -> Http.Request a
createRequest lite =
    Http.request 
        { body = Http.emptyBody
        , expect = lite.expect
        , method = lite.method
        , url = "http://localhost:8080" ++ lite.route
        , headers = 
            [ Http.header "Origin" "localhost"
            , Http.header "Access-Control-Request-Method" lite.method ]
        , timeout = Nothing
        , withCredentials = False }

listUrl : String -> String
listUrl listName =
    "/lists/" ++ listName

todosUrl : String -> String
todosUrl listName =
    listUrl listName ++ "/todos"

todoUrl : String -> Int -> String
todoUrl listName todoId =
    todosUrl listName ++ "/" ++ toString todoId

toApiError : Http.Error -> Error
toApiError httpError =
    case httpError of
    Http.BadStatus result ->
        if result.status.code == 404
        then NoSuchEntity 
        else Other result.status.message
    Http.BadPayload s r ->
        Other "API returned unexpected JSON"
    Http.BadUrl s ->
        Other "API url invalid"
    _ ->
        ApiNotAvailable

sendRequest : Tagger a msg -> RequestLite a -> Cmd msg
sendRequest tagger requestLite =
    let
        convert =
            Result.mapError toApiError >> tagger
        request =
            createRequest requestLite
    in
        Http.send convert request

getAllLists : Tagger (List TodoList) msg -> Cmd msg
getAllLists tagger =
    sendRequest tagger
        { expect = expectJson decodeTodoLists
        , method = "GET"
        , route = "/lists" }

postTodo : Tagger Todo msg -> TodoList -> String -> Cmd msg
postTodo tagger list title =
    sendRequest tagger
        { expect = expectJson decodeTodo
        , method = "POST" 
        , route = todosUrl list.name ++ "?title=" ++ title }

deleteTodo : Tagger Int msg -> TodoList -> Int -> Cmd msg
deleteTodo tagger list id =
    sendRequest tagger
        { expect = expectStringResponse (\_ -> Ok id)
        , method = "DELETE"
        , route = todoUrl list.name id }

putTodo : Tagger Todo msg -> TodoList -> Todo -> Cmd msg
putTodo tagger list todo =
    sendRequest tagger
        { expect = expectJson decodeTodo 
        , method = "PUT"
        , route = todoUrl list.name todo.id ++ "?done=" ++ String.toLower (toString todo.done) }

postList : Tagger TodoList msg -> String -> Cmd msg
postList tagger listName =
    sendRequest tagger
        { expect = expectJson decodeTodoList
        , method = "POST"
        , route = "/lists?name=" ++ listName }

deleteList : Tagger String msg -> String -> Cmd msg
deleteList tagger listName =
    sendRequest tagger
        { expect = expectStringResponse (\_ -> Ok listName)
        , method = "DELETE"
        , route = listUrl listName }


decodeTodo : Decoder Todo
decodeTodo =
    map3 Todo (field "title" string) (field "done" bool) (field "id" int)

decodeTodos : Decoder (List Todo)
decodeTodos =
    Json.Decode.list (oneOf [decodeTodo, null nullTodo])

decodeTodoList : Decoder TodoList
decodeTodoList =
    map2 TodoList (field "name" string) (field "todos" decodeTodos)

decodeTodoLists : Decoder (List TodoList)
decodeTodoLists =
    Json.Decode.list decodeTodoList
