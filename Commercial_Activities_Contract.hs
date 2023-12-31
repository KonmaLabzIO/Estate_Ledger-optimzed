module Commercial_Activities_Contract where
import Land_info
import Text.Read (readMaybe)

-- Define safeReadDouble function
safeReadDouble :: String -> IO Double
safeReadDouble prompt = do
    putStrLn prompt
    input <- getLine
    case readMaybe input of
        Just val -> return val
        Nothing  -> putStrLn "Invalid input, please enter a number." >> safeReadDouble prompt

commercialActivitiesContract :: String -> IO ()
commercialActivitiesContract escrowWallet = do
    activityType <- getNonEmptyInput "Enter type of activity:"
    activityName <- getNonEmptyInput "Enter name of activity:"
    income <- safeReadDouble "Enter income generated by this activity (Ada):"
    -- Add logic to verify and record commercial activities
    -- ...
    putStrLn "Commercial activity recorded."

getNonEmptyInput :: String -> IO String
getNonEmptyInput prompt = do
    putStrLn prompt
    input <- getLine
    if null input
        then putStrLn "Input cannot be empty." >> getNonEmptyInput prompt
        else return input