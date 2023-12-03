module Land_Acquisition_Contract where
import Land_info
import Text.Read (readMaybe)

-- Helper function for reading and validating Double input
safeReadDouble :: String -> IO Double
safeReadDouble prompt = do
    putStrLn prompt
    input <- getLine
    case readMaybe input of
        Just val -> return val
        Nothing  -> putStrLn "Invalid input, please enter a number." >> safeReadDouble prompt

landAcquisitionContract :: Params -> IO (String, String, String, Double)
landAcquisitionContract land = do
    putStrLn $ "Acquiring land at location: " ++ location land
    putStrLn $ "Size: " ++ show (size land) ++ " square feet"
    putStrLn $ "Price: ada" ++ show (price land)
    putStrLn $ "From owner: " ++ landOwner land
    escrowWallet <- getValidWallet "Enter the escrow wallet address for the NFT:"
    landOwnerWallet <- getValidWallet "Enter the land owner's wallet address:"
    return ("Land acquisition confirmed for " ++ location land, escrowWallet, landOwnerWallet, price land)

-- Validate wallet address (placeholder for real validation logic)
getValidWallet :: String -> IO String
getValidWallet prompt = do
    putStrLn prompt
    getLine -- Add real wallet validation logic here
