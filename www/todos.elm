module Todos exposing (..)

type alias Todo =
    { title : String
    , done : Bool
    , id : Int }

type alias TodoList =
    { name : String
    , todos : List Todo }

    
nullTodo : Todo
nullTodo =
    Todo "null" False -1

isNull : Todo -> Bool
isNull todo =
    todo.id == -1

addOrUpdateTodo : TodoList -> Todo -> TodoList
addOrUpdateTodo list todo =
    let
        -- doit existingTodo = if existingTodo.id == todo.id then todo else existingTodo
        addOrUpdate : List Todo -> List Todo
        addOrUpdate todos =
            case todos of
            t :: rest ->
                if t.id == todo.id
                then (todo :: rest)
                else t :: (addOrUpdate rest)
            [] ->
                [ todo ]

    in
        { list | todos = addOrUpdate list.todos }

removeTodo : TodoList -> Int -> TodoList
removeTodo list id =
    { list | todos = List.filter (\t -> not (t.id == id)) list.todos }
