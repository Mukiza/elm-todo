module Todo where

import String
import Html (..)
import Html.Attributes(..)
import Html.Events(..)
import Html.Tags(..)
import Html.Optimize.RefEq as Ref
import Maybe
import Window
import Graphics.Input
import Graphics.Input as Input

type State =
     { tasks      : [Task]
     , field      : String
     , uid        : Int
     , visibility : String
     }

type Task =
     { description : String
     , completed   : Bool
     , editing     : Bool
     , id          : Int
     }

newTask : String -> Int -> Task
newTask desc id =
        { description = desc
        , editing     = False
        , completed   = False
        , id          = id
        }

emptyState : State
emptyState =
       { tasks      = []
       , visibility = "All"
       , field      = ""
       , uid        = 0
       }

data Action
    = NoOp
    | UpdateField String
    | EditingTask Int Bool
    | Add
    | UpdateTask Int String
    | Delete Int
    | DeleteComplete
    | Check Int Bool
    | CheckAll Bool
    | ChangeVisibility String

step : Action -> State -> State
step action state =
    case action of
      NoOp -> state
      Add ->
          { state | uid <- state.uid + 1
          , field <- ""
          , tasks <- if String.isEmpty state.field
                     then state.tasks
                     else state.tasks ++ [newTask state.field state.uid]
          }

      UpdateField str ->
          { state | field <- str }

      EditingTask id isEditing ->
          let update t  = if t.id == id then { t | editing <- isEditing } else t
          in { state | tasks <- map update state.tasks }

      Delete id ->
          { state | tasks <- filter (\t -> t.id /= id) state.tasks }

      DeleteComplete ->
          { state | tasks <- filter (not << .completed) state.tasks }

      Check id isCompleted ->
          let update t = if t.id == id then { t | completed <- isCompleted } else t
          in { state | tasks <- map update state.tasks }

      CheckAll isCompleted ->
          let update t = { t | completed <- isCompleted }
          in { state | tasks <- map update state.tasks }

      ChangeVisibility visibility ->
          { state | visibility <- visibility }

view : State -> Html
view state =
    div
      [ class "todomvc-wrapper"
      , style [prop "visibility" "hidden"]
      ]
      [ section
        [ id "todo-app"]
        [ Ref.lazy taskEntry state.field
        , Ref.lazy2 taskList state.visibility state.tasks
        , Ref.lazy2 controls state.visibility state.tasks
        ]
      , infoFooter
      ]

onEnter : Input.Handle a -> a -> Attribute
onEnter handle value =
        on "keydown" (when (\k -> k.keyCode ==13) getKeyboardEvent) handle (always value)

taskEntry : String -> Html
taskEntry task =
      header
        [id "header"]
        [h1 [] [text "My Todos"]
        , input
            [ id "new-todo"
            , placeholder "what needs to be done?"
            , autofocus True
            , value task
            , on "input" getValue actions.handle UpdateField
            , onEnter actions.handle Add
            ]
            []
        ]

taskList : String -> [Task] -> Html
taskList visibility tasks =
         let isVisible todo = case visibility of
                       "Completed" -> todo.completed
                       "Active" -> not todo.completed
                       "All" -> True

             allCompleted = all .completed tasks
             cssVisibility = if isEmpty tasks then "hidden" else "visible"
         in section
            [id "main"
            , style [prop "visibility" cssVisibility]
            ]
            [input
                [ id "toggle-all"
                , type' "checkbox"
                , name "toggle"
                , checked allCompleted
                , onclick actions.handle (\_ -> CheckAll (not allCompleted))
                ]
                []
            , label
              [for "toggle-all"]
              [text "Mark all as complete"]
            , ul
              [id "todo-list"]
              (map todoItem (filter isVisible tasks))
              ]

todoItem : Task -> Html
todoItem todo =
         let className = (if todo.completed then "completed" else "") ++
                         (if todo.editing then "editing" else "")
          in
          li
            [ class className ]
            [ div
                [ class "view"
                , type' "checkbox"
                , checked todo.completed
                , onclick actions.handle (\_ -> Check todo.id (not todo.completed))
                ]
                []
            , label
              [onclick actions.handle (\_ -> EditingTask todo.id True)]
              [ text todo.description ]
            , button
              [ class "destroy"
              , onclick actions.handle (always (Delete todo.id))
              ]
              []
            , input
              [ class "edit"
              , value "todo.description"
              , id ("todo-" ++ show todo.id)
              , on "input" getValue actions.handle (UpdateTask todo.id)
              , onblur actions.handle (EditingTask todo.id False)
              , onEnter actions.handle (EditingTask todo.id False)
              ]
              []
            ]

controls : String -> [Task] -> Html
controls visibility tasks =
         let tasksCompleted = length (filter .completed tasks)
             tasksLeft = length tasks - tasksCompleted
             item_ = if tasksLeft == 1 then " item" else " items"
         in footer
            [ id "footer"
            , hidden (isEmpty tasks)
            ]
            [ span
              [ id "todo-count" ]
              [ strong [] [text (show tasksLeft)]
              , text (item_ ++ " left")
              ]
            , ul
               [ id "filters" ]
               [ visibilitySwap "#/" "All" visibility
               , text " "
               ,  visibilitySwap "#/active" "Active" visibility
               , text " "
               , visibilitySwap "#/completed" "Completed" visibility
               ]
            , button
              [ class "clear-completed"
              , id "clear-completed"
              , hidden (tasksCompleted == 0)
              , onclick actions.handle (always DeleteComplete)
              ]
              [ text ("Clear Completed (" ++ show tasksCompleted ++ ")")]
             ]

visibilitySwap : String -> String -> String -> Html
visibilitySwap uri visibility actualVisibility =
               let className = if visibility == actualVisibility then "selected" else "" in
               li
                [ onclick actions.handle (always (ChangeVisibility visibility)) ]
                [ a [ class className, href uri ] [ text visibility ]]

infoFooter : Html
infoFooter =
     footer [ id "info" ]
        [ p [] [ text "Doube-click to edit a todo"]
        , p [] [ text "Written by Mukiza"]
        , p [] [ text "Credits "
               , a [ href "https://github.com/evancz"] [ text "Evan Czaplicki" ]]
        ]

main : Signal Element
main = lift2 scene state Window.dimensions

scene : State -> (Int, Int) -> Element
scene state (w, h) =
      container w h midTop (toElement 550 h (view state))

state : Signal State
state = foldp step startingState actions.signal

startingState : State
startingState = Maybe.maybe emptyState identity getStorage

actions : Input.Input Action
actions = Input.input NoOp

port focus : Signal String
port focus =
     let needsFocus act =
         case act of
              EditingTask id bool -> bool
              _ -> False

         toSelector (EditingTask id _) = ("#todo-" ++ show id)
     in
       toSelector <~ keepIf needsFocus (EditingTask 0 True) actions.signal

port getStorage : Maybe State

port setStorage : Signal State
port setStorage = state