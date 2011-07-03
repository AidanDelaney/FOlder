{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, TemplateHaskell, UndecidableInstances  #-}
module Types.Foldr where

import Happstack.Data                        (Default, Version, deriveAll)
--import Happstack.State.ClockTime             () -- instance Data ClockTime
import Control.Applicative                   ( (<$>) )
import Data.SafeCopy
import Data.Data
import Happstack.Data.Proxy
import Happstack.Data.IxSet                  (IxSet(..), ixSet, ixGen, Indexable, (@=))
import qualified Happstack.Data.IxSet        as IxSet
-- Strangely SafeCopy => IxSet a is defined in here.
import Happstack.Auth.Core.Auth
import Data.Int

newtype DocId = DocId { theId :: Int64 } deriving (Eq, Ord, Show, Data, Typeable, Read)

-- |Document - Meta-information about a document and the editable content.
data Document = Document
  { author      :: String
    , docid     :: DocId
    , title     :: String
--  , ctime     :: ClockTime
--  , mtime     :: ClockTime
    , content   :: String
  } deriving (Show, Eq, Ord, Data, Typeable)

instance Num DocId where
  (+) (DocId x) (DocId y) = DocId (x+y)
  (-) (DocId x) (DocId y) = DocId (x-y)
  (*) (DocId x) (DocId y) = DocId (x*y)
  negate (DocId x)        = DocId (negate x)
  abs (DocId x)           = DocId (abs x)
  signum (DocId x)        = DocId (signum x)
  fromInteger x           = DocId (fromInteger x)

instance Indexable Document where
  empty = ixSet [ixGen (Proxy :: Proxy DocId)]

 -- |Foldr - A folder containing documents 
data Foldr = Foldr {ixset :: (IxSet Document), nextId :: DocId} deriving (Show, Eq, Ord, Typeable)

$(deriveSafeCopy 0 'base ''Document)
$(deriveSafeCopy 1 'base ''Foldr)
$(deriveSafeCopy 2 'base ''DocId)

{-
instance (SafeCopy a, Ord a, Typeable a, IxSet.Indexable a) => SafeCopy (IxSet a) where
    putCopy ixSet = contain $ safePut (IxSet.toList ixSet)
    getCopy = contain $ IxSet.fromList <$> safeGet
-}