{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
module Database.Beam.Transformers.Prefixed
  ( module Database.Beam.Transformers.Prefixed
  , module Database.Beam.Transformers.Prefixed.Types
  ) where

import Data.Functor.Identity
import Database.Beam
import Database.Beam.AutoMigrate
import Database.Beam.Backend.SQL
import Database.Beam.Postgres
import Database.Beam.Schema.Tables

import Database.Beam.Transformers.Prefixed.Types
import Database.Beam.Transformers.Virtual

prefixedV
  :: forall prefix tbl be db
  .  ( BeamSqlBackend be
     , Beamable tbl
     , HasTableEquality be prefix
     , SqlValableTable be prefix
     )
  => prefix Identity
  -> VirtualTable be db (PrefixedTable prefix tbl)
  -> VirtualTable be db tbl
prefixedV prefix tbl = VirtualTable
  { -- Filter to only include our established prefix
    _virtualTable_all = fmap _prefixedTable_value $ filter_ (\p -> _prefixedTable_prefix p ==. val_ prefix) $ _virtualTable_all tbl
    -- Add the known prefix to every row we're inserting
  , _virtualTable_insertFrom = \expr -> mapSqlInsert _prefixedTable_value $ _virtualTable_insertFrom tbl (fmap (PrefixedTable $ val_ prefix) expr)
  , _virtualTable_insertExpressions = \vals -> mapSqlInsert _prefixedTable_value $ _virtualTable_insertExpressions tbl (fmap (PrefixedTable $ val_ prefix) vals)
  , _virtualTable_insertFromWith = \expr -> mapSqlInsert _prefixedTable_value $ _virtualTable_insertFromWith tbl ((fmap . fmap) (PrefixedTable $ val_ prefix) expr)
    -- Add the known prefix to every row we're inserting
  , _virtualTable_insertFromOnConflict = \expr target action -> mapSqlInsert _prefixedTable_value $ _virtualTable_insertFromOnConflict tbl (fmap (PrefixedTable $ val_ prefix) expr) target action
  , _virtualTable_insertExpressionsOnConflict = \vals target action -> mapSqlInsert _prefixedTable_value $ _virtualTable_insertExpressionsOnConflict tbl (fmap (PrefixedTable $ val_ prefix) vals) target action
    -- Limit updates so they only apply to rows matching the prefix
  , _virtualTable_update = \assignments predicate -> mapSqlUpdate _prefixedTable_value $ _virtualTable_update tbl (assignments . _prefixedTable_value) $ \(PrefixedTable p v) -> p ==. val_ prefix &&. predicate v
    -- Only delete rows matching the prefix
  , _virtualTable_delete = \predicate -> mapSqlDelete _prefixedTable_value $ _virtualTable_delete tbl $ \(PrefixedTable p v) -> p ==. val_ prefix &&. predicate v
  }

mapSqlInsert :: forall tblA tblB be. Beamable tblB => (forall f. tblA f -> tblB f) -> SqlInsert be tblA -> SqlInsert be tblB
mapSqlInsert f = \case
  SqlInsert tblSettings expr -> SqlInsert (coerceTableFields @tblA @tblB $ f tblSettings) expr
  SqlInsertNoRows -> SqlInsertNoRows

mapSqlUpdate :: forall tblA tblB be. Beamable tblB => (forall f. tblA f -> tblB f) -> SqlUpdate be tblA -> SqlUpdate be tblB
mapSqlUpdate f = \case
  SqlUpdate tblSettings expr -> SqlUpdate (coerceTableFields @tblA @tblB $ f tblSettings) expr
  SqlIdentityUpdate -> SqlIdentityUpdate

mapSqlDelete :: forall tblA tblB be. Beamable tblB => (forall f. tblA f -> tblB f) -> SqlDelete be tblA -> SqlDelete be tblB
mapSqlDelete f = \case
  SqlDelete tblSettings expr -> SqlDelete (coerceTableFields @tblA @tblB $ f tblSettings) expr

coerceTableFields :: forall tblA tblB tbl. Beamable tbl => tbl (TableField tblA) -> tbl (TableField tblB)
coerceTableFields = mapBeamFields (\(Columnar' a) -> Columnar' $ coerceTableField a)

coerceTableField :: forall tblA tblB a. TableField tblA a -> TableField tblB a
coerceTableField (TableField path name) = TableField path name

mapBeamFields :: Beamable tbl => (forall x. Columnar' a x -> Columnar' b x) -> tbl a -> tbl b
mapBeamFields f tbl = runIdentity $ zipBeamFieldsM (\a _ -> Identity $ f a) tbl tbl

{-
data PrefixedTableEntity prefix f e where
  PrefixedTableEntity :: f (TableEntity (PrefixedTable prefix tbl)) -> PrefixedTableEntity prefix f (TableEntity tbl)

newtype PrefixedDb prefix db f = PrefixedDb { unPrefixedDb :: db (PrefixedTableEntity prefix f) } deriving (Generic)

deriving instance Show (db (PrefixedTableEntity prefix f)) => Show (PrefixedDb prefix db f)

instance ArgDictT db => ArgDictT (PrefixedDb prefix db) where
  type ConstraintsForT (PrefixedDb prefix db) c = ConstraintsForT db c
  hoistWithArgDictT_ proxy f (PrefixedDb db) = PrefixedDb $ hoistWithArgDictT_ proxy (\(PrefixedTableEntity a) -> PrefixedTableEntity (f a)) db

instance DMappable db => DMappable (PrefixedDb prefix db) where
  dmap f (PrefixedDb a) = fmap PrefixedDb $ dmap f a

instance DZippable db => DZippable (PrefixedDb prefix db) where
  dzip f (PrefixedDb a) (PrefixedDb b) = fmap PrefixedDb $ dzip f a b

instance DPointed db => DPointed (PrefixedDb prefix db) where
  dpure f = fmap PrefixedDb $ dpure (PrefixedTableEntity f)

instance Database be db => Database be (PrefixedDb prefix db)
-}

annotatePrefixedTableFields
  :: (Beamable prefix, Beamable tbl)
  => (tbl (FieldModification (TableFieldSchema (PrefixedTable prefix tbl)))
      -> tbl (FieldModification (TableFieldSchema (PrefixedTable prefix tbl))))
  -> EntityModification
       (AnnotatedDatabaseEntity be db)
       be
       (TableEntity (PrefixedTable prefix tbl))
annotatePrefixedTableFields foo = annotateTableFields $
  let t = tableModification
  in t
     { _prefixedTable_value = foo (_prefixedTable_value t)
     }
