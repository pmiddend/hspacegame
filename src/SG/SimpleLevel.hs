{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module SG.SimpleLevel where

import Control.Lens ((^.))
import Linear.V2 (V2(V2), _y)
import Numeric.Lens (negated)
import SG.Constants
import SG.Time
import SG.Types

simpleLevel :: Level
simpleLevel =
  [ Spawn
      { _spawnTimeDiff = milliseconds @Integer 2000
      , _spawnType =
          SpawnTypeHint
            (TextDescriptor
               { _rtFontDescriptor = hudFont
               , _rtColor = hintTextColor
               , _rtText = "use [W A S D] to move around"
               })
      }
  , Spawn
      { _spawnTimeDiff = milliseconds @Integer 3000
      , _spawnType =
          SpawnTypeMeteor
            meteorGreyBig1
            (V2 440 (fromIntegral (meteorGreyBig1 ^. meteorSize . _y . negated)))
      }
  , Spawn
      { _spawnTimeDiff = milliseconds @Integer 4000
      , _spawnType =
          SpawnTypeMeteor
            meteorBrownBig1
            (V2
               600
               (fromIntegral (meteorBrownBig1 ^. meteorSize . _y . negated)))
      }
  , Spawn
      { _spawnTimeDiff = milliseconds @Integer 5000
      , _spawnType =
          SpawnTypeMeteor
            meteorGreyBig2
            (V2 100 (fromIntegral (meteorGreyBig2 ^. meteorSize . _y . negated)))
      }
  , Spawn
      { _spawnTimeDiff = milliseconds @Integer 6000
      , _spawnType =
          SpawnTypeMeteor
            meteorGreyBig3
            (V2 800 (fromIntegral (meteorGreyBig3 ^. meteorSize . _y . negated)))
      }
  , Spawn
      { _spawnTimeDiff = milliseconds @Integer 11000
      , _spawnType =
          SpawnTypeHint
            (TextDescriptor
               { _rtFontDescriptor = hudFont
               , _rtColor = hintTextColor
               , _rtText = "use [SPACE] to shoot - watch your energy!"
               })
      }
  , Spawn
      { _spawnTimeDiff = milliseconds @Integer 12000
      , _spawnType =
          SpawnTypeMeteor
            meteorBrownMed1
            (V2
               500
               (fromIntegral (meteorBrownMed1 ^. meteorSize . _y . negated)))
      }
  ]
