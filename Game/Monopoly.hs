module Game.Monopoly
  ( Property      (..)
  , PropertyState (..)
  , BoardPosition (..)
  , PlayerState   (..)
  , nextBoardPosition
  , diceRolls
  ) where

import Data.Bits
import System.Random

-- | Properties.
data Property
  = MediterraneanAvenue
  | BalticAvenue
  | ReadingRailroad
  | OrientalAvenue
  | VermontAvenue
  | ConnecticutAvenue
  | StCharlesPlace
  | ElectricCompany
  | StatesAvenue
  | VirginiaAvenue
  | PennsylvaniaRailroad
  | StJamesPlace
  | TennesseAvenue
  | NewYorkAvenue
  | KentuckyAvenue
  | IndianaAvenue
  | IllinoisAvenue
  | BAndORailroad
  | AtlanticAvenue
  | VentnorAvenue
  | WaterWorks
  | MarvinGardens
  | PacificAvenue
  | NorthCarolinaAvenue
  | PennsylvaniaAvenue
  | ShortLine
  | ParkPlace
  | Boardwalk
  deriving (Show, Eq)

-- | Board position
data BoardPosition
  = Property Property
  | Go
  | CommunityChest1
  | IncomeTax
  | Chance1
  | InJail
  | JustVisiting
  | CommunityChest2
  | FreeParking
  | Chance2
  | CommunityChest3
  | Chance3
  | LuxuryTax
  deriving (Show, Eq)

-- | A players state
data PlayerState = PlayerState
  { cash                  :: Int
  , properties            :: [(Property, PropertyState)]
  , getOutOfJailFreeCards :: Int
  , boardPosition         :: BoardPosition
  }

-- | Property state.
data PropertyState
  = Active
  | Active1Houses
  | Active2Houses
  | Active3Houses
  | Active4Houses
  | ActiveHotel
  | Mortaged

-- | Next board position.
nextBoardPosition :: BoardPosition -> Int -> BoardPosition
nextBoardPosition a b = case a of
  InJail -> InJail
  a -> dropWhile (/= a) board !! b
  where
  board :: [BoardPosition]
  board = cycle
    [ Go
    , Property MediterraneanAvenue
    , CommunityChest1
    , Property BalticAvenue
    , IncomeTax
    , Property ReadingRailroad
    , Property OrientalAvenue
    , Chance1
    , Property VermontAvenue
    , Property ConnecticutAvenue
    , JustVisiting
    , Property StCharlesPlace
    , Property ElectricCompany
    , Property StatesAvenue
    , Property VirginiaAvenue
    , Property PennsylvaniaRailroad
    , Property StJamesPlace
    , CommunityChest2
    , Property TennesseAvenue
    , Property NewYorkAvenue
    , FreeParking
    , Property KentuckyAvenue
    , Chance2
    , Property IndianaAvenue
    , Property IllinoisAvenue
    , Property BAndORailroad
    , Property AtlanticAvenue
    , Property VentnorAvenue
    , Property WaterWorks
    , Property MarvinGardens
    , InJail  -- Go to jail is the same as InJail.
    , Property PacificAvenue
    , Property NorthCarolinaAvenue
    , CommunityChest3
    , Property PennsylvaniaAvenue
    , Property ShortLine
    , Chance3
    , Property ParkPlace
    , LuxuryTax
    , Property Boardwalk
    ]

diceRolls :: Int -> [(Int, Int)]
diceRolls seed = pair $ filter validRoll $ map (.&. 0x7) $ randoms $ mkStdGen seed
  where
  validRoll a = a >= 1 && a <= 6
  pair :: [Int] -> [(Int, Int)]
  pair a = case a of
    a : b : c -> (a, b) : pair c
    _ -> []

