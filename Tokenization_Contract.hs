module Tokenization_Contract where
import Land_info

tokenizationContract :: Params -> String -> String -> IO NFT
tokenizationContract land escrowWallet landOwnerWallet = do
    putStrLn "NFT generated for the acquired land!"
    putStrLn $ "Transferring NFT from land owner's wallet to escrow account: " ++ landOwnerWallet ++ " -> " ++ escrowWallet
    putStrLn "NFT transfer complete."
    chooseFractionalization land

chooseFractionalization :: Params -> IO NFT
chooseFractionalization land = do
    putStrLn "Choose your option:"
    putStrLn "1. Fractionalize the NFT"
    putStrLn "2. Keep it as a single NFT"
    choice <- getLine
    case choice of
        "1" -> fractionalize land
        "2" -> return Single
        _   -> putStrLn "Invalid choice!" >> chooseFractionalization land

fractionalize :: Params -> IO NFT
fractionalize land = do
    let nftCount = round (size land) 
    putStrLn $ "Generated " ++ show nftCount ++ " fractional NFTs for the land at " ++ location land
    return (Fractionalized nftCount)
