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

data VirtualTable db tbl = VirtualTable
  { _virtualTable_all
    :: forall s
    .  Q Postgres db s (tbl (QExpr Postgres s))
  , _virtualTable_insertFrom
    :: (forall s. Q Postgres db s (tbl (QExpr Postgres s)))
    -> SqlInsert Postgres tbl
  , _virtualTable_insertFromOnConflict
    :: (forall s. Q Postgres db s (tbl (QExpr Postgres s)))
    -> SqlConflictTargetV
    -> SqlConflictActionV
    -> SqlInsert Postgres tbl
  , _virtualTable_insertFromWith
    :: (forall s. With Postgres db (Q Postgres db s (tbl (QExpr Postgres s))))
    -> SqlInsert Postgres tbl
  , _virtualTable_insertExpressions
    :: (forall s. [tbl (QExpr Postgres s)])
    -> SqlInsert Postgres tbl
  , _virtualTable_insertExpressionsOnConflict
    :: (forall s. [tbl (QExpr Postgres s)])
    -> SqlConflictTargetV
    -> SqlConflictActionV
    -> SqlInsert Postgres tbl
  , _virtualTable_update
    :: (forall s. tbl (QField s) -> QAssignment Postgres s)
    -> (forall s. tbl (QExpr Postgres s) -> QExpr Postgres s Bool)
    -> SqlUpdate Postgres tbl
  , _virtualTable_delete
    :: (forall s. tbl (QExpr Postgres s) -> QExpr Postgres s Bool)
    -> SqlDelete Postgres tbl
  }

fromConcrete
  :: ( Database Postgres db
     , Beamable tbl
     )
  => DatabaseEntity Postgres db (TableEntity tbl)
  -> VirtualTable db tbl
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
  :: VirtualTable db tbl
  -> Q Postgres db s (tbl (QExpr Postgres s))
allV_ = _virtualTable_all

lookupV_
  :: ( Table tbl
     , SqlValableTable Postgres (PrimaryKey tbl)
     , HasTableEquality Postgres (PrimaryKey tbl)
     )
  => VirtualTable db tbl
  -> PrimaryKey tbl Identity
  -> SqlSelect Postgres (tbl Identity)
lookupV_ tbl k = select $ do
  r <- allV_ tbl
  guard_ $ pk r ==. val_ k
  pure r

insertValuesV
  :: ( SqlValableTable Postgres tbl
     )
  => VirtualTable db tbl
  -> [tbl Identity]
  -> SqlInsert Postgres tbl
insertValuesV tbl vals = _virtualTable_insertExpressions tbl $ fmap val_ vals

insertValuesOnConflictV
  :: ( SqlValableTable Postgres tbl
     )
  => VirtualTable db tbl
  -> SqlConflictTargetV
  -> SqlConflictActionV
  -> [tbl Identity]
  -> SqlInsert Postgres tbl
insertValuesOnConflictV tbl target action vals = _virtualTable_insertExpressionsOnConflict tbl (fmap val_ vals) target action

insertExpressionsV
  :: VirtualTable db tbl
  -> (forall s. [tbl (QExpr Postgres s)])
  -> SqlInsert Postgres tbl
insertExpressionsV tbl vals = _virtualTable_insertExpressions tbl vals

insertExpressionsOnConflictV
  :: VirtualTable db tbl
  -> SqlConflictTargetV
  -> SqlConflictActionV
  -> (forall s. [tbl (QExpr Postgres s)])
  -> SqlInsert Postgres tbl
insertExpressionsOnConflictV tbl target action vals = _virtualTable_insertExpressionsOnConflict tbl vals target action

insertExpressionsOnConflictReturningV
  :: ( Beamable tbl
     , ProjectibleWithPredicate
       AnyType
       Postgres
       (WithExprContext (BeamSqlBackendExpressionSyntax' Postgres))
       a
     )
  => VirtualTable db tbl
  -> SqlConflictTargetV
  -> SqlConflictActionV
  -> (tbl (QExpr Postgres PostgresInaccessible) -> a)
  -> (forall s. [tbl (QExpr Postgres s)])
  -> PgInsertReturning (QExprToIdentity a)
insertExpressionsOnConflictReturningV tbl target action ret vals = _virtualTable_insertExpressionsOnConflict tbl vals target action `returning` ret

insertFromV
  :: VirtualTable db tbl
  -> (forall s. Q Postgres db s (tbl (QExpr Postgres s)))
  -> SqlInsert Postgres tbl
insertFromV tbl expr = _virtualTable_insertFrom tbl expr

insertFromOnConflictV
  :: VirtualTable db tbl
  -> SqlConflictTargetV
  -> SqlConflictActionV
  -> (forall s. Q Postgres db s (tbl (QExpr Postgres s)))
  -> SqlInsert Postgres tbl
insertFromOnConflictV tbl target action expr = _virtualTable_insertFromOnConflict tbl expr target action

insertFromOnConflictReturningV
  :: ( Beamable tbl
     , ProjectibleWithPredicate
       AnyType
       Postgres
       (WithExprContext (BeamSqlBackendExpressionSyntax' Postgres))
       a
     )
  => VirtualTable db tbl
  -> SqlConflictTargetV
  -> SqlConflictActionV
  -> (tbl (QExpr Postgres PostgresInaccessible) -> a)
  -> (forall s. Q Postgres db s (tbl (QExpr Postgres s)))
  -> PgInsertReturning (QExprToIdentity a)
insertFromOnConflictReturningV tbl target action ret expr = _virtualTable_insertFromOnConflict tbl expr target action `returning` ret

-- TODO this is based on our own
insertFromWithV
  :: VirtualTable db tbl
  -> (forall s. With Postgres db (Q Postgres db s (tbl (QExpr Postgres s))))
  -> SqlInsert Postgres tbl
insertFromWithV tbl expr = _virtualTable_insertFromWith tbl expr

data SqlConflictTargetV = SqlConflictTargetV_AnyConflict

anyConflictV :: SqlConflictTargetV
anyConflictV = SqlConflictTargetV_AnyConflict

toBeamSqlConflictTarget :: SqlConflictTargetV -> SqlConflictTarget Postgres tbl
toBeamSqlConflictTarget = \case
  SqlConflictTargetV_AnyConflict -> anyConflict

data SqlConflictActionV = SqlConflictActionV_DoNothing

onConflictDoNothingV :: SqlConflictActionV
onConflictDoNothingV = SqlConflictActionV_DoNothing

toBeamSqlConflictAction :: SqlConflictActionV -> SqlConflictAction Postgres tbl
toBeamSqlConflictAction = \case
  SqlConflictActionV_DoNothing -> onConflictDoNothing

updateV
  :: VirtualTable db tbl
  -> (forall s. tbl (QField s) -> QAssignment Postgres s)
  -> (forall s. tbl (QExpr Postgres s) -> QExpr Postgres s Bool)
  -> SqlUpdate Postgres tbl
updateV = _virtualTable_update

deleteV
  :: VirtualTable db tbl
  -> (forall s. tbl (QExpr Postgres s) -> QExpr Postgres s Bool)
  -> SqlDelete Postgres tbl
deleteV = _virtualTable_delete

joinV_
  :: VirtualTable db tbl
  -> (tbl (QExpr Postgres s) -> QExpr Postgres s Bool)
  -> Q Postgres db s (tbl (QExpr Postgres s))
joinV_ tbl cond = do
  row <- allV_ tbl
  guard_ $ cond row
  pure row

updateRowByKeyV
  :: ( Table tbl
     , HasTableEquality Postgres (PrimaryKey tbl)
     , SqlValableTable Postgres (PrimaryKey tbl)
     )
  => VirtualTable db tbl
  -> PrimaryKey tbl Identity
  -> (forall s. tbl (QField s) -> QAssignment Postgres s)
  -> SqlUpdate Postgres tbl
updateRowByKeyV tbl key assignments = updateV tbl assignments $ \t -> pk t ==. val_ key
