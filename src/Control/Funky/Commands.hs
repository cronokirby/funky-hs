{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
module Control.Funky.Commands
    ( Func
    , HList(..)
    , ReadFuncs(..)
    , Command(..)
    , runCommand
    , parseHelper
    , wargs
    , int
    )
where


import           Data.Text      as T
import           Data.Text.Read (decimal)

type family Func (xs :: [*]) t where
    Func '[] t = t
    Func (x ': xs) t = x -> Func xs t


data HList :: [*] -> * where
    HNil :: HList '[]
    HCons :: a -> HList ts -> HList (a ': ts)

infixr 7 `HCons`

callHList :: HList ts -> Func ts t -> t
callHList HNil f           = f
callHList (x `HCons` xs) f = callHList xs (f x)


data Command :: * -> * where
  Command ::
    { commName :: Text
    , commParser :: Text -> Either (Int, Text) (HList ts)
    , commGood :: Func ts a
    , commBad :: Int -> Text -> a
    } -> Command a

-- |Runs a command, using the "good" handle if the parsing succeeds,
-- and otherwise using the "bad" handle to produce an output.
runCommand :: Command a -> Text -> a
runCommand (Command _ parser good bad) =
    either (uncurry bad) (`callHList` good) . parser


data ReadFuncs :: [*] -> * where
    RFNil :: ReadFuncs '[]
    RFCons :: (Text -> Maybe a) -> ReadFuncs ts -> ReadFuncs (a ': ts)

infixr 7 `RFCons`

-- |Helper function for creating a parser.
-- Example parser that reads integers
-- >  > readInt :: Text -> Maybe Int
-- >  > parser = parseHelper words $ readInt `RFCons` readInt `RFCons` RFNil
-- >  > parser "1 3"
-- >  Right (HFCons 1 (HFCons 3 HFNil))
parseHelper :: (Text -> [Text]) -- ^ A function to split arguments
            -> ReadFuncs ts     -- ^ A list of corresponding parsers
            -> (Text -> Either (Int, Text) (HList ts)) -- ^ final parser
parseHelper split readers = go 0 readers . split
  where
    go :: Int -> ReadFuncs ts -> [Text] -> Either (Int, Text) (HList ts)
    go _ RFNil  _               = Right HNil
    go _ (RFCons r _)  []       = Left (3, "")
    go n (RFCons r rs) (x : xs) =
        maybe (Left (n, x)) ((<$> go (n+1) rs xs) . HCons) $ r x


wargs :: ReadFuncs ts -> (Text -> Either (Int, Text) (HList ts))
wargs = parseHelper T.words


int :: Integral a => Text -> Maybe a
int = either (const Nothing) (pure . fst) . decimal
