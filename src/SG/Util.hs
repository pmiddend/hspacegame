module SG.Util where

import Control.Lens (Getter, to)
import Data.Text (Text, pack)
import Linear.V2 (V2)

continue :: (Monad m, Monoid b) => m a -> m b
continue x = x >> pure mempty

resetIf :: Bool -> Maybe a -> Maybe a
resetIf True _ = Nothing
resetIf _ x = x

filterMaybe :: (a -> Bool) -> Maybe a -> Maybe a
filterMaybe _ Nothing = Nothing
filterMaybe f (Just x) =
  if f x
    then Just x
    else Nothing

textShow :: Show a => a -> Text
textShow = pack . show

floatV2 :: (Integral a, Floating b) => V2 a -> V2 b
floatV2 = (fromIntegral <$>)

floatingV2 :: (Integral a, Floating b) => Getter (V2 a) (V2 b)
floatingV2 = to floatV2

lerp :: Num a => a -> a -> a -> a
lerp a b t = (1 - t) * a + t * b
