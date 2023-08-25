{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Database.Beam.Transformers.Virtual where

import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Writer

import Data.Coerce
import Database.Beam
import Database.Beam.Backend.SQL
import Database.Beam.Backend.SQL.BeamExtensions
import Database.Beam.Postgres
import Database.Beam.Postgres.Full hiding (insert)
import Database.Beam.Postgres.Syntax
import Database.Beam.Query.CTE as CTE
import Database.Beam.Query.Internal

data VirtualTable be db tbl = VirtualTable
  { _virtualTable_all
    :: forall s
    .  Q be db s (tbl (QExpr be s))
  , _virtualTable_insertFrom
    :: (forall s. Q be db s (tbl (QExpr be s)))
    -> SqlInsert be tbl
  , _virtualTable_insertFromOnConflict
    :: (forall s. Q be db s (tbl (QExpr be s)))
    -> SqlConflictTargetV
    -> SqlConflictActionV
    -> SqlInsert be tbl
  , _virtualTable_insertFromWith
    :: (forall s. With be db (Q be db s (tbl (QExpr be s))))
    -> SqlInsert be tbl
  , _virtualTable_insertExpressions
    :: (forall s. [tbl (QExpr be s)])
    -> SqlInsert be tbl
  , _virtualTable_insertExpressionsOnConflict
    :: (forall s. [tbl (QExpr be s)])
    -> SqlConflictTargetV
    -> SqlConflictActionV
    -> SqlInsert be tbl
  , _virtualTable_update
    :: (forall s. tbl (QField s) -> QAssignment be s)
    -> (forall s. tbl (QExpr be s) -> QExpr be s Bool)
    -> SqlUpdate be tbl
  , _virtualTable_delete
    :: (forall s. tbl (QExpr be s) -> QExpr be s Bool)
    -> SqlDelete be tbl
  }

fromConcrete
  :: ( Database Postgres db
     , Beamable tbl
     )
  => DatabaseEntity Postgres db (TableEntity tbl)
  -> VirtualTable Postgres db tbl
fromConcrete tbl = VirtualTable
  { _virtualTable_all = all_ tbl
  , _virtualTable_insertFrom = \vals -> insert tbl (insertFrom vals)
  , _virtualTable_insertFromOnConflict = \vals target action -> insertOnConflict tbl (insertFrom vals) (toBeamSqlConflictTarget target) (toBeamSqlConflictAction action)
  -- TODO upstream this one to beam
  , _virtualTable_insertFromWith = \(CTE.With mkQ) ->
      let (q, (recursiveness, ctes)) = evalState (runWriterT mkQ) 0
      in insert tbl $ case recursiveness of
        CTE.Nonrecursive -> SqlInsertValues $
          (coerce :: PgSelectSyntax -> PgInsertValuesSyntax) $
          withSyntax ctes $
          buildSqlQuery "t" q
        CTE.Recursive -> SqlInsertValues $
          (coerce :: PgSelectSyntax -> PgInsertValuesSyntax) $
          withRecursiveSyntax ctes $
          buildSqlQuery "t" q
  , _virtualTable_insertExpressions = \vals -> insert tbl (insertExpressions vals)
  , _virtualTable_insertExpressionsOnConflict = \vals target action -> insertOnConflict tbl (insertExpressions vals) (toBeamSqlConflictTarget target) (toBeamSqlConflictAction action)
  , _virtualTable_update = update tbl
  , _virtualTable_delete = delete tbl
  }

allV_
  :: VirtualTable be db tbl
  -> Q be db s (tbl (QExpr be s))
allV_ = _virtualTable_all

lookupV_
  :: ( BeamSqlBackend be
     , HasQBuilder be
     , Table tbl
     , SqlValableTable be (PrimaryKey tbl)
     , HasTableEquality be (PrimaryKey tbl)
     )
  => VirtualTable be db tbl
  -> PrimaryKey tbl Identity
  -> SqlSelect be (tbl Identity)
lookupV_ tbl k = select $ do
  r <- allV_ tbl
  guard_ $ pk r ==. val_ k
  pure r

insertValuesV
  :: ( BeamSqlBackend be
     , SqlValableTable be tbl
     )
  => VirtualTable be db tbl
  -> [tbl Identity]
  -> SqlInsert be tbl
insertValuesV tbl vals = _virtualTable_insertExpressions tbl $ fmap val_ vals

insertValuesOnConflictV
  :: ( BeamSqlBackend be
     , SqlValableTable be tbl
     )
  => VirtualTable be db tbl
  -> SqlConflictTargetV
  -> SqlConflictActionV
  -> [tbl Identity]
  -> SqlInsert be tbl
insertValuesOnConflictV tbl target action vals = _virtualTable_insertExpressionsOnConflict tbl (fmap val_ vals) target action

insertExpressionsV
  :: VirtualTable be db tbl
  -> (forall s. [tbl (QExpr be s)])
  -> SqlInsert be tbl
insertExpressionsV tbl vals = _virtualTable_insertExpressions tbl vals

insertExpressionsOnConflictV
  :: VirtualTable be db tbl
  -> SqlConflictTargetV
  -> SqlConflictActionV
  -> (forall s. [tbl (QExpr be s)])
  -> SqlInsert be tbl
insertExpressionsOnConflictV tbl target action vals = _virtualTable_insertExpressionsOnConflict tbl vals target action

insertExpressionsOnConflictReturningV
  :: ( Beamable tbl
     , ProjectibleWithPredicate
       AnyType
       Postgres
       (WithExprContext (BeamSqlBackendExpressionSyntax' Postgres))
       a
     )
  => VirtualTable Postgres db tbl
  -> SqlConflictTargetV
  -> SqlConflictActionV
  -> (tbl (QExpr Postgres PostgresInaccessible) -> a)
  -> (forall s. [tbl (QExpr Postgres s)])
  -> PgInsertReturning (QExprToIdentity a)
insertExpressionsOnConflictReturningV tbl target action ret vals = _virtualTable_insertExpressionsOnConflict tbl vals target action `returning` ret

insertFromV
  :: VirtualTable be db tbl
  -> (forall s. Q be db s (tbl (QExpr be s)))
  -> SqlInsert be tbl
insertFromV tbl expr = _virtualTable_insertFrom tbl expr

insertFromOnConflictV
  :: VirtualTable be db tbl
  -> SqlConflictTargetV
  -> SqlConflictActionV
  -> (forall s. Q be db s (tbl (QExpr be s)))
  -> SqlInsert be tbl
insertFromOnConflictV tbl target action expr = _virtualTable_insertFromOnConflict tbl expr target action

insertFromOnConflictReturningV
  :: ( Beamable tbl
     , ProjectibleWithPredicate
       AnyType
       Postgres
       (WithExprContext (BeamSqlBackendExpressionSyntax' Postgres))
       a
     )
  => VirtualTable Postgres db tbl
  -> SqlConflictTargetV
  -> SqlConflictActionV
  -> (tbl (QExpr Postgres PostgresInaccessible) -> a)
  -> (forall s. Q Postgres db s (tbl (QExpr Postgres s)))
  -> PgInsertReturning (QExprToIdentity a)
insertFromOnConflictReturningV tbl target action ret expr = _virtualTable_insertFromOnConflict tbl expr target action `returning` ret

-- TODO this is based on our own
insertFromWithV
  :: BeamSqlBackend be
  => VirtualTable be db tbl
  -> (forall s. With be db (Q be db s (tbl (QExpr be s))))
  -> SqlInsert be tbl
insertFromWithV tbl expr = _virtualTable_insertFromWith tbl expr

data SqlConflictTargetV = SqlConflictTargetV_AnyConflict

anyConflictV :: SqlConflictTargetV
anyConflictV = SqlConflictTargetV_AnyConflict

toBeamSqlConflictTarget
  :: BeamHasInsertOnConflict be
  => SqlConflictTargetV
  -> SqlConflictTarget be tbl
toBeamSqlConflictTarget = \case
  SqlConflictTargetV_AnyConflict -> anyConflict

data SqlConflictActionV = SqlConflictActionV_DoNothing

onConflictDoNothingV :: SqlConflictActionV
onConflictDoNothingV = SqlConflictActionV_DoNothing

toBeamSqlConflictAction
  :: BeamHasInsertOnConflict be
  => SqlConflictActionV -> SqlConflictAction be tbl
toBeamSqlConflictAction = \case
  SqlConflictActionV_DoNothing -> onConflictDoNothing

updateV
  :: BeamSqlBackend be
  => VirtualTable be db tbl
  -> (forall s. tbl (QField s) -> QAssignment be s)
  -> (forall s. tbl (QExpr be s) -> QExpr be s Bool)
  -> SqlUpdate be tbl
updateV = _virtualTable_update

deleteV
  :: BeamSqlBackend be
  => VirtualTable be db tbl
  -> (forall s. tbl (QExpr be s) -> QExpr be s Bool)
  -> SqlDelete be tbl
deleteV = _virtualTable_delete

joinV_
  :: BeamSqlBackend be
  => VirtualTable be db tbl
  -> (tbl (QExpr be s) -> QExpr be s Bool)
  -> Q be db s (tbl (QExpr be s))
joinV_ tbl cond = do
  row <- allV_ tbl
  guard_ $ cond row
  pure row

updateRowByKeyV
  :: ( BeamSqlBackend be
     , Table tbl
     , HasTableEquality be (PrimaryKey tbl)
     , SqlValableTable be (PrimaryKey tbl)
     )
  => VirtualTable be db tbl
  -> PrimaryKey tbl Identity
  -> (forall s. tbl (QField s) -> QAssignment be s)
  -> SqlUpdate be tbl
updateRowByKeyV tbl key assignments = updateV tbl assignments $ \t -> pk t ==. val_ key
