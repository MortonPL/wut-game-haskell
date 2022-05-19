module Level where

import DataTypes (Level (Level, lv_size, lv_tiles), Merchant (Merchant), Tile (DeepWater, Island, ShallowWater))


mMorshu = Merchant
  "Morshu"
  "Bomb, ropes, lamp oil. You want it? It's yours my friend."
  "There are two other islands to the south-east."
  []
mTem = Merchant
  "Tem"
  "Tem go to colleg ye-ya!"
  "sail eestwardz"
  []
mDaniel = Merchant
  "Daniel Jacks"
  "My business was ruined by those pirate folk..."
  "If you're brave enough, try east-northeast!"
  []
mFred = Merchant
  "Fred"
  "Good evening."
  "Other ships often take a course towards north..."
  []

_S = ShallowWater
_D = DeepWater

i1 = Island "Northstable Island" mMorshu
i2 = Island "Storm Reef" mTem
i3 = Island "White Tiger Island" mDaniel
i4 = Island "Bishop Rock Island" mFred

level :: Level
level =
  Level
    { lv_size = (6, 6),
      lv_tiles =
        [ [_S, _S, _S, i1, _S, _S],
          [_S, _D, _D, _D, _D, _S],
          [_S, _D, i2, _D, _D, _S],
          [i3, _D, _D, _D, _D, _S],
          [_S, _D, _D, _D, _D, _S],
          [_S, _S, _S, _S, i4, _S]
        ]
    }
