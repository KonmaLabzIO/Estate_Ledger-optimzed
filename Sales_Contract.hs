module Sales_Contract where
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

salesContract :: NFT -> String -> IO (NFT, Result)
salesContract nft escrowWallet = do
    buyerName <- getNonEmptyInput "Enter buyer's name:"
    walletAddress <- getValidWallet "Enter buyer's receiving wallet address:"
    case nft of
        Fractionalized count -> processFractionalSale count buyerName walletAddress escrowWallet
        Single               -> processSingleSale buyerName walletAddress escrowWallet

processFractionalSale :: Int -> String -> String -> String -> IO (NFT, Result)
processFractionalSale count buyerName walletAddress escrowWallet = do
    nftToBuy <- safeReadInt "Enter number of NFTs to buy:"
    if nftToBuy <= count then do
        putStrLn "Payment verified!"
        -- Add payment verification logic here
        -- ...
        return (Fractionalized (count - nftToBuy), "Sale confirmed for " ++ show nftToBuy ++ " NFTs to buyer " ++ buyerName)
    else do 
        putStrLn "Not enough NFTs available!"
        return (Fractionalized count, "Sale not completed due to insufficient NFTs")

processSingleSale :: String -> String -> String -> IO (NFT, Result)
processSingleSale buyerName walletAddress escrowWallet = do
    putStrLn "Payment verified!"
    -- Add payment verification logic here
    -- ...
    return (Single, "Sale confirmed of single NFT to buyer " ++ buyerName)

-- Utility function for reading and validating Int input
safeReadInt :: String -> IO Int
safeReadInt prompt = do
    putStrLn prompt
    input <- getLine
    case readMaybe input of
        Just val -> return val
        Nothing  -> putStrLn "Invalid input, please enter an integer." >> safeReadInt prompt

getNonEmptyInput :: String -> IO String
getNonEmptyInput prompt = do
    putStrLn prompt
    input <- getLine
    if null input
        then putStrLn "Input cannot be empty." >> getNonEmptyInput prompt
        else return input