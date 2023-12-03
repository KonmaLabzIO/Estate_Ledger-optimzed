module Land_Development_Contract where
import Land_info
import Text.Read (readMaybe)

-- Define getValidWallet function
getValidWallet :: String -> IO String
getValidWallet prompt = do
    putStrLn prompt
    wallet <- getLine
    if null wallet
        then putStrLn "Wallet address cannot be empty." >> getValidWallet prompt
        else return wallet

-- Define getNonEmptyInput function
getNonEmptyInput :: String -> IO String
getNonEmptyInput prompt = do
    putStrLn prompt
    input <- getLine
    if null input
        then putStrLn "Input cannot be empty." >> getNonEmptyInput prompt
        else return input

-- Define safeReadInt function
safeReadInt :: String -> IO Int
safeReadInt prompt = do
    putStrLn prompt
    input <- getLine
    case readMaybe input of
        Just val -> return val
        Nothing  -> putStrLn "Invalid input, please enter an integer." >> safeReadInt prompt

-- Helper function for reading and validating Double input
safeReadDouble :: String -> IO Double
safeReadDouble prompt = do
    putStrLn prompt
    input <- getLine
    case readMaybe input of
        Just val -> return val
        Nothing  -> putStrLn "Invalid input, please enter a number." >> safeReadDouble prompt

landDevelopmentContract :: Params -> String -> IO Result
landDevelopmentContract land escrowWallet = do
    plan <- getNonEmptyInput "Enter project plan:"
    budget <- safeReadDouble "Enter project budget (Ada):"
    timeline <- getNonEmptyInput "Enter project timeline (in months):"
    developerWallet <- getValidWallet "Enter developer's wallet address:"
    milestones <- safeReadInt "Enter number of milestones required for the project:"
    milestoneResults <- collectMilestoneInfo milestones
    -- Add logic for project tracking and fund release based on milestones
    -- ...
    return "Land development project initiated."

collectMilestoneInfo :: Int -> IO [(String, Int, Bool)]
collectMilestoneInfo 0 = return []
collectMilestoneInfo count = do
    milestoneName <- getNonEmptyInput $ "Enter milestone " ++ show count ++ "'s name:"
    phaseNumber <- safeReadInt $ "Enter milestone " ++ show count ++ "'s phase number:"
    confirmed <- getYesNoInput "Confirm funding for this milestone (y/n):"
    rest <- collectMilestoneInfo (count - 1)
    return $ (milestoneName, phaseNumber, confirmed) : rest

getYesNoInput :: String -> IO Bool
getYesNoInput prompt = do
    putStrLn prompt
    input <- getLine
    case input of
        "y" -> return True
        "n" -> return False
        _   -> putStrLn "Invalid input, please enter 'y' or 'n'." >> getYesNoInput prompt