module Main where

import Land_info (Land(..))
import Land_Acquisition_Contract (landAcquisitionContract)
import Tokenization_Contract (tokenizationContract)
import Sales_Contract (salesContract)
import Land_Development_Contract (landDevelopmentContract)
import Commercial_Activities_Contract (commercialActivitiesContract)
import Profit_Distribution_Contract (profitDistributionContract)

main :: IO ()
main = do
    putStrLn "Hello, Haskell!"
    -- Create a Land object with your desired values
    let land = Land { location = "Location", size = 1000, price = 500, landOwner = "Owner", landOwnerAddress = "Address" }
    (confirmation, escrowWallet, landOwnerWallet, _) <- landAcquisitionContract land
    putStrLn confirmation
    nft <- tokenizationContract land escrowWallet landOwnerWallet
    putStrLn "Tokenization completed."
    (nftAfterSale, saleConfirmation) <- salesContract nft escrowWallet
    putStrLn saleConfirmation
    developmentResult <- landDevelopmentContract land escrowWallet
    putStrLn developmentResult
    commercialActivitiesContract escrowWallet
    putStrLn "Commercial activity recorded."
    profitDistributionContract landOwnerWallet escrowWallet 10000 5
    putStrLn "Profit distribution completed."