module RocksmithPsarcHelpers where

import qualified Data.ByteString.Lazy as B
import Data.Binary.Get

data GetResult a = GetResult (B.ByteString, ByteOffset, a)
  deriving (Show, Eq)
  
instance Functor GetResult where
  fmap f (GetResult (bs, bo, a)) = GetResult (bs, bo, f a)

result :: GetResult a -> a
result (GetResult (_,_,a)) = a

unconsumed :: GetResult a -> B.ByteString
unconsumed (GetResult (b,_,_)) = b

runGetResultOrFail :: Get a -> B.ByteString -> Either String (GetResult a)
runGetResultOrFail g = eitherResult . runGetOrFail g
  where
    eitherResult :: Either (B.ByteString, ByteOffset, a) (B.ByteString, ByteOffset, b) -> Either a (GetResult b)
    eitherResult (Left (_,_,a)) = (Left a)
    eitherResult (Right t) = Right (GetResult t)