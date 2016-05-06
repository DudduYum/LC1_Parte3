module ErroreType where

  import Control.Monad (MonadPlus(..), liftM)
  import Control.Applicative (Applicative(..), Alternative(..))


  data Err a = Good a | NoGood String
    deriving (Read, Show, Eq, Ord)

  instance Monad Err where
    return      = Good
    fail        = NoGood
    Good a  >>= f = f a
    NoGood s >>= _ = NoGood s



  instance Applicative Err where
    pure = Good
    (NoGood s) <*> _ = NoGood s
    (Good f) <*> o  = liftM f o


  instance Functor Err where
    fmap = liftM

  instance MonadPlus Err where
    mzero = NoGood "Err.mzero"
    mplus (NoGood _) y = y
    mplus x       _ = x

  instance Alternative Err where
    empty = mzero
    (<|>) = mplus
