import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Data.Functor
import Data.IORef

main :: IO ()
-- The main function initializes the GUI application using the Threepenny library.
main = startGUI defaultConfig { jsStatic = Just "static" } setup
-- The setup function initializes the UI by creating elements, event streams, and behaviors.
setup :: Window -> UI ()
setup window = do
    -- Create UI elements
    counterDisplay <- UI.label

    -- Create event streams
    let incrementEvent = UI.click buttonClick $> (+ 1) -- add 1
    -- Create event streams
    let incrementEvent = UI.click buttonClick $> (+ 1) -- add 1
    -- Accumulate the counter behavior
    counterBehavior <- accumB 0 incrementEvent
    -- Display the counter
    element counterDisplay # sink UI.text (show <$> counterBehavior)