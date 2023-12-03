module Land_info where 

data Land = Land
  { location :: String,
    size :: Double,
    price :: Double,
    landOwner :: String,
    landOwnerAddress :: String
  }
  deriving (Show)

data NFT = Fractionalized Int | Single deriving Show

type Params = Land
type Result = String