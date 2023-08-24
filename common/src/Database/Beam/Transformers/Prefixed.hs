{-# LANGUAGE UndecidableInstances #-}
module Database.Beam.Transformers.Prefixed.Types where

import Control.Lens.Iso
import Data.Kind (Constraint)
import Database.Beam

data PrefixedTable prefix tbl f = PrefixedTable
  { _prefixedTable_prefix :: prefix f
  , _prefixedTable_value :: tbl f
  } deriving (Generic)

type HasPrefixedTableConstraint (c :: * -> Constraint) prefix tbl f =
  ( c (prefix f)
  , c (tbl f)
  )

deriving instance HasPrefixedTableConstraint Eq prefix tbl f => Eq (PrefixedTable prefix tbl f)
deriving instance HasPrefixedTableConstraint Ord prefix tbl f => Ord (PrefixedTable prefix tbl f)
deriving instance HasPrefixedTableConstraint Show prefix tbl f => Show (PrefixedTable prefix tbl f)

instance (Beamable prefix, Beamable tbl) => Beamable (PrefixedTable prefix tbl)

instance (Beamable prefix, Typeable prefix, Table tbl) => Table (PrefixedTable prefix tbl) where
  data PrimaryKey (PrefixedTable prefix tbl) f = PrefixedTableId
    { _prefixedTableId_prefix :: prefix f
    , _prefixedTableId_value :: PrimaryKey tbl f
    } deriving (Generic)
  primaryKey v = PrefixedTableId
    { _prefixedTableId_prefix = _prefixedTable_prefix v
    , _prefixedTableId_value = primaryKey $ _prefixedTable_value v
    }

type HasPrefixedTableIdConstraint (c :: * -> Constraint) prefix tbl f =
  ( c (prefix f)
  , c (PrimaryKey tbl f)
  )

deriving instance HasPrefixedTableIdConstraint Eq prefix tbl f => Eq (PrimaryKey (PrefixedTable prefix tbl) f)
deriving instance HasPrefixedTableIdConstraint Ord prefix tbl f => Ord (PrimaryKey (PrefixedTable prefix tbl) f)
deriving instance HasPrefixedTableIdConstraint Show prefix tbl f => Show (PrimaryKey (PrefixedTable prefix tbl) f)

instance (Beamable prefix, Beamable (PrimaryKey tbl)) => Beamable (PrimaryKey (PrefixedTable prefix tbl))

_PrefixedTableId :: Iso' (prefix f, PrimaryKey tbl f) (PrimaryKey (PrefixedTable prefix tbl) f)
_PrefixedTableId = iso (\(t, i) -> PrefixedTableId t i) (\(PrefixedTableId t i) -> (t, i))

-- | Get a reference to a primary key of one prefixed table from another prefixed table
prefixedPkRef :: (tblA f -> PrimaryKey tblB f) -> PrefixedTable prefix tblA f -> PrimaryKey (PrefixedTable prefix tblB) f
prefixedPkRef f p = PrefixedTableId
  { _prefixedTableId_prefix = _prefixedTable_prefix p
  , _prefixedTableId_value = f $ _prefixedTable_value p
  }
