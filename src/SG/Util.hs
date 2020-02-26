module SG.Util where

import Data.Text (Text, pack)

continue :: (Monad m, Monoid b) => m a -> m b
continue x = x >> pure mempty

resetIf :: Bool -> Maybe a -> Maybe a
resetIf True _ = Nothing
resetIf _ x = x

textShow :: Show a => a -> Text
textShow = pack . show
