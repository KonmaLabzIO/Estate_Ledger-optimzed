module Profit_Distribution_Contract where
import Land_info

-- Define getNonEmptyInput function
getNonEmptyInput :: String -> IO String
getNonEmptyInput prompt = do
    putStrLn prompt
    input <- getLine
    if null input
        then putStrLn "Input cannot be empty." >> getNonEmptyInput prompt
        else return input

profitDistributionContract :: String -> String -> Double -> Int -> IO ()
profitDistributionContract landOwnerWallet _ totalProfit investorCount = do
    investors <- collectInvestorInfo investorCount
    let profitPerRecipient = totalProfit / fromIntegral (investorCount + 1)
    putStrLn "Transferring profits to recipients..."
    -- Add logic for profit distribution to investors and landowner
    -- ...
    putStrLn "Profit distribution completed."

collectInvestorInfo :: Int -> IO [(String, String)]
collectInvestorInfo 0 = return []
collectInvestorInfo count = do
    investorName <- getNonEmptyInput $ "Enter investor " ++ show count ++ "'s name:"
    investorWallet <- getValidWallet $ "Enter wallet address for investor " ++ show count ++ ":"
    rest <- collectInvestorInfo (count - 1)
    return $ (investorName, investorWallet) : rest

getValidWallet :: String -> IO String
getValidWallet prompt = do
    putStrLn prompt
    getLine -- Add real wallet validation logic here