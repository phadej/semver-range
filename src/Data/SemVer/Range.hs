{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.SemVer.Range (
  -- * Types
    RangeOp(..)
  , RangeSpec(..)
  , Version(..)
  , version
  , VersionRange
  , range
  , Identifier(..)
  , LatticeSyntax(..)
  , (/\)
  , (\/)
  -- * Parsers
  , parseVersion
  , parseVersionRange
  ) where

import Control.Applicative
import Control.DeepSeq
import Control.Monad
import Control.Monad.Trans.State
import Data.Data
import Data.Foldable
import Data.String
import Data.Char
import Data.Traversable
import GHC.Generics
import Prelude hiding (or, all)
import Text.Regex.Applicative as RE
import Text.Regex.Applicative.Common as RE

eitherToMaybe :: Either e a -> Maybe a
eitherToMaybe (Right x) = Just x
eitherToMaybe (Left _)  = Nothing

-- Semver
data Version = Version
    { _versionMajor   :: !Int
    , _versionMinor   :: !Int
    , _versionPatch   :: !Int
    , _versionRelease :: [Identifier]
    } deriving (Eq, Ord, Show, Typeable, Data, Generic)
-- todo hashable

version :: Int -> Int -> Int -> Version
version x y z = Version x y z []

versionR :: RE Char Version
versionR = uncurry4 Version <$> threeR

parseVersion :: String -> Maybe Version
parseVersion = RE.match versionR

data Identifier = INum  !Int
                | IStr !String
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

-- TODO: hashable

instance IsString Identifier where
  fromString str
    | all isDigit str = INum $ read str
    | otherwise       = IStr str

identifiers :: RE Char [Identifier]
identifiers = maybe [] id <$> optional (identifiers')

identifiers' :: RE Char [Identifier]
identifiers' = (:) <$ sym '-' <*> identifier <*> many (sym '.' *> identifier)

identifier :: RE Char Identifier
identifier = INum <$> decimal <|> IStr <$> many (psym (flip Prelude.elem $ '-' : ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']))

---

data RangeOp = ROLT -- ^ @<@
             | ROLE -- ^ @<=@
             | ROGT -- ^ @>@
             | ROGE -- ^ @>=@
             | ROEQ -- ^ @=@
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

instance NFData RangeOp

data LatticeSyntax a = LVar a
                     | LBound Bool
                     | LJoin (LatticeSyntax a) (LatticeSyntax a)
                     | LMeet (LatticeSyntax a) (LatticeSyntax a)
  deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Typeable, Data)

infixr 3 /\
infixr 2 \/

-- | Infix version of 'LMeet'
(/\) :: LatticeSyntax a -> LatticeSyntax a -> LatticeSyntax a
(/\) = LMeet

-- | Infix version of 'LJoin'
(\/) :: LatticeSyntax a -> LatticeSyntax a -> LatticeSyntax a
(\/) = LJoin

instance Applicative LatticeSyntax where
  pure  = return
  (<*>) = ap

instance Monad LatticeSyntax where
  return = LVar
  LVar x    >>= f = f x
  LBound b  >>= _ = LBound b
  LJoin a b >>= f = LJoin (a >>= f) (b >>= f)
  LMeet a b >>= f = LMeet (a >>= f) (b >>= f)

freeVars :: LatticeSyntax a -> [a]
freeVars = toList

dual :: LatticeSyntax a -> LatticeSyntax a
dual (LVar v) = LVar v
dual (LBound t) = LBound $ not t
dual (LJoin a b) = LMeet (dual a) (dual b)
dual (LMeet a b) = LJoin (dual a) (dual b)

-- | Test for equivalence.
--
-- >>> equivalent (LMeet (LVar 'a') (LVar 'b')) (LMeet (LVar 'b') (LVar 'a'))
-- True
--
-- >>> equivalent (LVar 'a') (LMeet (LVar 'a') (LVar 'a'))
-- True
--
-- >>> equivalent (LMeet (LVar 'a') (LVar 'b')) (LMeet (LVar 'b') (LVar 'b'))
-- False
equivalent :: Eq a => LatticeSyntax a -> LatticeSyntax a -> Bool
equivalent a b = all (uncurry (==)) . runEval $ p
  where p = (,) <$> evalLattice a <*> evalLattice b

-- | Test for preorder.
--
-- @ a ≤ b ⇔ a ∨ b ≡ b ⇔ a ≡ a ∧ b @
--
-- >>> preorder (LVar 'a' `LMeet` LVar 'b') (LVar 'a')
-- True
--
-- >>> preorder (LVar 'a') (LVar 'a' `LMeet` LVar 'b')
-- False
preorder :: Eq a => LatticeSyntax a -> LatticeSyntax a -> Bool
preorder a b = (a `LJoin` b) `equivalent` b

-- | Return `True` if for some variable assigment expression evaluates to `True`.
satisfiable :: Eq a => LatticeSyntax a -> Bool 
satisfiable = or . runEval . evalLattice

newtype Eval v a = Eval { unEval :: StateT [(v, Bool)] [] a }
  deriving (Functor, Applicative, Alternative, Monad, MonadPlus)

runEval :: Eval v a -> [a]
runEval act = evalStateT (unEval act) []

evalLattice :: Eq v => LatticeSyntax v -> Eval v Bool
evalLattice (LVar v)    = guess v
evalLattice (LBound b)  = return b
evalLattice (LJoin a b) = evalLattice a ||^ evalLattice b
evalLattice (LMeet a b) = evalLattice a &&^ evalLattice b

guess :: Eq v => v -> Eval v Bool
guess v = Eval $ do
  st <- get
  let remember b = put ((v, b) : st) >> return b
  case lookup v st of
    Just b  -> return b
    Nothing -> remember True <|> remember False

-- From Control.Monad.Extra of extra

-- | Like @if@, but where the test can be monadic.
ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM b t f = do b' <- b; if b' then t else f

-- | The lazy '||' operator lifted to a monad. If the first
--   argument evaluates to 'True' the second argument will not
--   be evaluated.
--
-- > Just True  ||^ undefined  == Just True
-- > Just False ||^ Just True  == Just True
-- > Just False ||^ Just False == Just False
(||^) :: Monad m => m Bool -> m Bool -> m Bool
(||^) a b = ifM a (return True) b

-- | The lazy '&&' operator lifted to a monad. If the first
--   argument evaluates to 'False' the second argument will not
--   be evaluated.
--
-- > Just False &&^ undefined  == Just False
-- > Just True  &&^ Just True  == Just True
-- > Just True  &&^ Just False == Just False
(&&^) :: Monad m => m Bool -> m Bool -> m Bool
(&&^) a b = ifM a b (return False)

--

data RangeSpec = RS !RangeOp !Version
  deriving (Eq, Ord, Show, Typeable, Data, Generic)

type VersionRange = LatticeSyntax RangeSpec

range :: RangeOp -> Version -> VersionRange
range op v = pure (RS op v)

fullRange :: VersionRange
fullRange = range ROGE (version 0 0 0)

-- Range parser

scalarRangeR :: RE Char VersionRange
scalarRangeR = ge <|> gt <|> lt <|> le <|> eq
  where ge = LVar . RS ROGE <$ RE.string ">=" <*> versionR
        gt = LVar . RS ROGT <$ RE.string ">"  <*> versionR
        le = LVar . RS ROLE <$ RE.string "<=" <*> versionR
        lt = LVar . RS ROLT <$ RE.string "<"  <*> versionR
        eq = LVar . RS ROEQ <$> versionR

separatedBy :: (a -> a -> a) -> RE c a -> RE c () -> RE c a
separatedBy f re sep = foldl' f <$> re <*> many (sep *> re)

ws :: RE Char ()
ws = void $ some $ psym isSpace

conR :: RE Char VersionRange
conR =  separatedBy (/\) scalarRangeR ws

disR :: RE Char VersionRange
disR = separatedBy (\/) conR (ws *> string "||" *> ws)

starR :: RE Char VersionRange
starR = fullRange <$ string "*"

xRange1R :: RE Char VersionRange
xRange1R = f <$> RE.decimal <* sym '.' <*> RE.decimal <* string ".x"
  where f x y = range ROGE (version x y 0) /\ range ROLT (version x (y + 1) 0)

xRange2R :: RE Char VersionRange
xRange2R = f <$> RE.decimal <* string ".x"
  where f x = range ROGE (version x 0 0) /\ range ROLT (version (x + 1) 0 0)

xRange3R :: RE Char VersionRange
xRange3R = f <$> RE.decimal <* string ".x.x"
  where f x = range ROGE (version x 0 0) /\ range ROLT (version (x + 1) 0 0)

threeR :: RE Char (Int, Int, Int, [Identifier])
threeR = (,,,) <$>  RE.decimal <* sym '.' <*> RE.decimal <* sym '.' <*> RE.decimal <*> identifiers

uncurry4 :: (a -> b -> c -> d -> e) -> (a, b, c, d) -> e
uncurry4 f (a,b,c,d) = f a b c d
{- INLINE uncurry4 -}

twoR :: RE Char (Int, Int)
twoR = (,) <$> RE.decimal <* sym '.' <*> RE.decimal

oneR :: RE Char Int
oneR = RE.decimal

partial1R :: RE Char VersionRange
partial1R = uncurry f <$> twoR
  where f x y = range ROGE (version x y 0) /\ range ROLT (version x (y + 1) 0)

partial2R :: RE Char VersionRange
partial2R = f <$> oneR
  where f x = range ROGE (version x 0 0) /\ range ROLT (version (x + 1) 0 0)

tilde1R :: RE Char VersionRange
tilde1R = uncurry4 f <$ sym '~' <*> threeR
  where f 0 0 z i = range ROGE (Version 0 0 z i) /\ range ROLT (version 0 0 (z + 1))
        f 0 y z i = range ROGE (Version 0 y z i) /\ range ROLT (version 0 (y + 1) 0)
        f x y z i = range ROGE (Version x y z i) /\ range ROLT (version x (y + 1) 0)

tilde2R :: RE Char VersionRange
tilde2R = uncurry f  <$ sym '~' <*> twoR
  where f x y = range ROGE (version x y 0) /\ range ROLT (version x (y + 1) 0)

tilde3R :: RE Char VersionRange
tilde3R = f <$ sym '~' <*> oneR
  where f x = range ROGE (version x 0 0) /\ range ROLT (version (x + 1) 0 0)

caret1R :: RE Char VersionRange
caret1R = uncurry4 f <$ sym '^' <*> threeR
  where f 0 0 z i = range ROGE (Version 0 0 z i) /\ range ROLT (version 0 0 (z + 1))
        f 0 y z i = range ROGE (Version 0 y z i) /\ range ROLT (version 0 (y + 1) 0)
        f x y z i = range ROGE (Version x y z i) /\ range ROLT (version (x + 1) 0 0)

caret2R :: RE Char VersionRange
caret2R = uncurry f <$ sym '^' <*> twoR <* optional (string ".x")
  where f 0 y = range ROGE (version 0 y 0) /\ range ROLT (version 0 (y + 1) 0)
        f x y = range ROGE (version x y 0) /\ range ROLT (version (x + 1) 0 0)

caret3R :: RE Char VersionRange
caret3R = f <$ sym '^' <*> oneR <* string ".x"
  where f x = range ROGE (version x 0 0) /\ range ROLT (version (x + 1) 0 0)

choose :: Alternative f => [f a] -> f a
choose = Data.Foldable.foldr (<|>) empty 

hyphenR :: RE Char VersionRange
hyphenR = (/\) <$> hyphenLoR <* ws <* sym '-' <* ws <*> hyphenHiR

hyphenLoR :: RE Char VersionRange
hyphenLoR = h1 <|> h2 <|> h3
  where h1 = range ROGE <$> versionR
        h2 = uncurry (\x y -> range ROGE (version x y 0)) <$> twoR
        h3 = (\x -> range ROGE (version x 0 0)) <$> oneR
        
hyphenHiR :: RE Char VersionRange
hyphenHiR = h1 <|> h2 <|> h3
  where h1 = range ROLE <$> versionR
        h2 = uncurry (\x y -> range ROLT (version x (y + 1) 0)) <$> twoR
        h3 = (\x -> range ROLT (version (x + 1) 0 0)) <$> oneR

advandedRangeR :: RE Char VersionRange
advandedRangeR = choose 
 [ xRange1R
 , xRange2R
 , xRange3R
 , partial1R
 , partial2R
 , tilde1R
 , tilde2R
 , tilde3R
 , caret1R
 , caret2R
 , caret3R
 , hyphenR
 ]

rangeR :: RE Char VersionRange
rangeR = disR <|> starR <|> advandedRangeR <|> pure fullRange

parseVersionRange :: String -> Maybe VersionRange
parseVersionRange = RE.match rangeR
