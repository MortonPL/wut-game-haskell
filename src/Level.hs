module Level where

import Data.Map (Map, fromList)
import qualified Data.Map as Map
import DataTypes
  ( Level (Level, lv_size, lv_tiles),
    Merchant (Merchant, mc_buying, mc_desc, mc_merchants, mc_name, mc_selling),
    Tile (DeepWater, Island, ShallowWater),
  )

mMorshu =
  Merchant
    { mc_name = "Morshu",
      mc_desc = "Bomb, ropes, lamp oil. You want it? It's yours my friend.",
      mc_merchants = "There are two other islands to the south-east.",
      mc_selling =
        fromList
          [ ("rum", 1.0),
            ("gunpowder", 1.0),
            ("blunderbuss", 1.0),
            ("ration", 1.0),
            ("banana", 1.0)
          ],
      mc_buying =
        fromList
          [ ("rum", 0.9),
            ("gunpowder", 0.9),
            ("blunderbuss", 0.9),
            ("ration", 0.9),
            ("banana", 0.9)
          ]
    }

mTem =
  Merchant
    { mc_name = "Tem",
      mc_desc = "Tem go to colleg ye-ya!",
      mc_merchants = "sail eestwardz",
      mc_selling =
        fromList
          [ ("rum", 1.4),
            ("gunpowder", 0.6),
            ("blunderbuss", 0.6),
            ("ration", 1.8),
            ("map_piece_1", 1.0)
          ],
      mc_buying =
        fromList
          [ ("rum", 1.3),
            ("gunpowder", 0.5),
            ("blunderbuss", 0.5),
            ("ration", 0.7),
            ("banana", 1.1)
          ]
    }

mDaniel =
  Merchant
    { mc_name = "Daniel Jacks",
      mc_desc = "My business was ruined by those pirate folk...",
      mc_merchants = "If you're brave enough, try east-northeast!",
      mc_selling =
        fromList
          [ ("rum", 1.2),
            ("gunpowder", 1.6),
            ("blunderbuss", 1.2),
            ("ration", 0.8),
            ("mercenary", 1.0)
          ],
      mc_buying =
        fromList
          [ ("rum", 1.1),
            ("gunpowder", 1.5),
            ("blunderbuss", 1.1),
            ("ration", 0.7),
            ("banana", 1.3)
          ]
    }

mFred =
  Merchant
    { mc_name = "Fred",
      mc_desc = "Good evening.",
      mc_merchants = "Other ships often take a course towards north...",
      mc_selling =
        fromList
          [ ("rum", 1.2),
            ("gunpowder", 1.4),
            ("blunderbuss", 1.2),
            ("ration", 1.0),
            ("mercenary", 0.8)
          ],
      mc_buying =
        fromList
          [ ("rum", 1.1),
            ("gunpowder", 1.3),
            ("blunderbuss", 1.1),
            ("ration", 0.9),
            ("banana", 1.6)
          ]
    }

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
    , ("rum", 35)
    , ("gunpowder", 10)
    , ("blunderbuss", 250)
    , ("ration", 10)
    , ("map_piece_2", 1500)
    , ("banana", 12)
    , ("mercenary", 500)
    ]

itemPirAtt :: Map String Int
itemPirAtt =
  Map.fromList
    [ ("balls", 40)
    , ("rum", 50)
    , ("gunpowder", 30)
    , ("blunderbuss", 30)
    , ("ration", 10)
    , ("banana", 10)
    , ("map_piece_1", 5000)
    , ("map_piece_2", 5000)
    , ("mercenary", -1000)
    , ("coin", 1)
    ]

pirAttMin :: Int
pirAttMin = 0

pirAttMax :: Int
pirAttMax = 10

pirMaxProb :: Float
pirMaxProb = 0.5
