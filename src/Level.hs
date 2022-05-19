module Level where

import DataTypes (Level (Level, lv_size, lv_tiles), Merchant (Merchant), Tile (DeepWater, Island, ShallowWater))

mMorshu = Merchant
  "Morshu"
  "Bomb, ropes, lamp oil. You want it? It's yours my friend."
  "There are two other islands to the south-east."
  [ ["rum", 1.0]
  , ["gunpowder", 1.0]
  , ["blunderbuss", 1.0]
  , ["ration", 1.0]
  , ["banana", 1.0]]
  [ ["rum", 0.9]
  , ["gunpowder", 0.9]
  , ["blunderbuss", 0.9]
  , ["ration", 0.9]
  , ["banana", 0.9]]
mTem = Merchant
  "Tem"
  "Tem go to colleg ye-ya!"
  "sail eestwardz"
  [ ["rum", 1.4]
  , ["gunpowder", 0.6]
  , ["blunderbuss", 0.6]
  , ["ration", 1.8]
  , ["map_piece_1", 1.0]]
  [ ["rum", 1.3]
  , ["gunpowder", 0.5]
  , ["blunderbuss", 0.5]
  , ["ration", 0.7]
  , ["banana", 1.1]]
mDaniel = Merchant
  "Daniel Jacks"
  "My business was ruined by those pirate folk..."
  "If you're brave enough, try east-northeast!"
  [ ["rum", 1.2]
  , ["gunpowder", 1.6]
  , ["blunderbuss", 1.2]
  , ["ration", 0.8]
  , ["mercenary", 1.0]]
  [ ["rum", 1.1]
  , ["gunpowder", 1.5]
  , ["blunderbuss", 1.1]
  , ["ration", 0.7]
  , ["banana", 1.3]]
mFred = Merchant
  "Fred"
  "Good evening."
  "Other ships often take a course towards north..."
  [ ["rum", 1.2]
  , ["gunpowder", 1.4]
  , ["blunderbuss", 1.2]
  , ["ration", 1.0]
  , ["mercenary", 0.8]]
  [ ["rum", 1.1]
  , ["gunpowder", 1.3]
  , ["blunderbuss", 1.1]
  , ["ration", 0.9]
  , ["banana", 1.6]]

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
