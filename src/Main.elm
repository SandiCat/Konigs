import MouseManipulator
import Html.App


main =
    Html.App.program
        { init = MouseManipulator.init 
        , update = MouseManipulator.update
        , subscriptions = MouseManipulator.subscriptions
        , view = MouseManipulator.view
        }