import Network.Socket
import Graphics.UI.Qtah.Gui
import Graphics.UI.Qtah.Core
import Graphics.UI.Qtah.Widgets

main :: IO ()
main = do
  -- Create a socket
  sock <- socket AF_INET Stream defaultProtocol
  
  -- Set up the server address
  let serverAddr = SockAddrInet 8080 (tupleToHostAddress (127, 0, 0, 1))
  
  -- Connect to the server
  connect sock serverAddr
  
  -- Create the main application window
  app <- newQApplication []
  window <- newQWidget
  
  -- Set up the layout
  layout <- newQVBoxLayout
  setLayout window layout
  
  -- Create the chat input field
  input <- newQLineEdit
  layoutAddWidget layout input
  
  -- Create the chat output field
  output <- newQTextEdit
  layoutAddWidget layout output
  
  -- Create the send button
  sendButton <- newQPushButton
  setButtonText sendButton "Send"
  layoutAddWidget layout sendButton
  
  -- Handle button click events
  on clicked sendButton $ do
    -- Get the text from the input field
    text <- text input
    
    -- Send the text to the server
    send sock text (length text) []
    
    -- Clear the input field
    clear input
  
  -- Show the window
  show window
  
  -- Run the application
  exec app
