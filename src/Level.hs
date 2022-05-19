module Level where

import Data.Map (Map, (!))
import qualified Data.Map as Map
import DataTypes
  ( Level (Level, lv_size, lv_tiles),
    Merchant (Merchant),
    PriceTag (PriceTag),
    Tile (DeepWater, Island, ShallowWater),
  )

mMorshu =
  Merchant
    "Morshu"
    "Bomb, ropes, lamp oil. You want it? It's yours my friend."
    "There are two other islands to the south-east."
    [ PriceTag "rum" 1.0,
      PriceTag "gunpowder" 1.0,
      PriceTag "blunderbuss" 1.0,
      PriceTag "ration" 1.0,
      PriceTag "banana" 1.0
    ]
    [ PriceTag "rum" 0.9,
      PriceTag "gunpowder" 0.9,
      PriceTag "blunderbuss" 0.9,
      PriceTag "ration" 0.9,
      PriceTag "banana" 0.9
    ]

mTem =
  Merchant
    "Tem"
    "Tem go to colleg ye-ya!"
    "sail eestwardz"
    [ PriceTag "rum" 1.4,
      PriceTag "gunpowder" 0.6,
      PriceTag "blunderbuss" 0.6,
      PriceTag "ration" 1.8,
      PriceTag "map_piece_1" 1.0
    ]
    [ PriceTag "rum" 1.3,
      PriceTag "gunpowder" 0.5,
      PriceTag "blunderbuss" 0.5,
      PriceTag "ration" 0.7,
      PriceTag "banana" 1.1
    ]

mDaniel =
  Merchant
    "Daniel Jacks"
    "My business was ruined by those pirate folk..."
    "If you're brave enough, try east-northeast!"
    [ PriceTag "rum" 1.2,
      PriceTag "gunpowder" 1.6,
      PriceTag "blunderbuss" 1.2,
      PriceTag "ration" 0.8,
      PriceTag "mercenary" 1.0
    ]
    [ PriceTag "rum" 1.1,
      PriceTag "gunpowder" 1.5,
      PriceTag "blunderbuss" 1.1,
      PriceTag "ration" 0.7,
      PriceTag "banana" 1.3
    ]

mFred =
  Merchant
    "Fred"
    "Good evening."
    "Other ships often take a course towards north..."
    [ PriceTag "rum" 1.2,
      PriceTag "gunpowder" 1.4,
      PriceTag "blunderbuss" 1.2,
      PriceTag "ration" 1.0,
      PriceTag "mercenary" 0.8
    ]
    [ PriceTag "rum" 1.1,
      PriceTag "gunpowder" 1.3,
      PriceTag "blunderbuss" 1.1,
      PriceTag "ration" 0.9,
      PriceTag "banana" 1.6
    ]

_S = ShallowWater

_D = DeepWater

m1 = Island "Northstable Island" mMorshu

m2 = Island "Storm Reef" mTem

m3 = Island "White Tiger Island" mDaniel

m4 = Island "Bishop Rock Island" mFred

level :: Level
level =
  Level
    { lv_size = (6, 6),
      lv_tiles =
        [ [_S, _S, _S, m1, _S, _S],
          [_S, _D, _D, _D, _D, _S],
          [_S, _D, m2, _D, _D, _S],
          [m3, _D, _D, _D, _D, _S],
          [_S, _D, _D, _D, _D, _S],
          [_S, _S, _S, _S, m4, _S]
        ]
    }

itemValues :: Map String Int
itemValues =
  Map.fromList
    [ ("balls", 1)
    ]

itemPirAtt :: Map String Int
itemPirAtt =
  Map.fromList
    [ ("balls", -40)
    ]

pirAttMin :: Int
pirAttMin = 300

pirAttMax :: Int
pirAttMax = 300

pirMaxProb :: Float
pirMaxProb = 50.0
