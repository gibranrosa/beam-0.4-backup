{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Database.Beam.Query.Types
    ( Q, QExpr, QExprToIdentity, TopLevelQ, IsQuery

    , Projectible(..)

    , Aggregation

    , queryToSQL'

    , optimizeExpr, optimizeExpr' ) where

import Database.Beam.Query.Internal

import Database.Beam.Schema.Tables
import Database.Beam.SQL hiding ((<>))
import Database.HDBC

import Control.Monad.State
import Control.Monad.Writer hiding (All)
import Control.Monad.Identity

import Data.String
import Data.Generics.Uniplate.Data

-- * Beam queries

type family QExprToIdentity x
type instance QExprToIdentity (table (QExpr s)) = table Identity
type instance QExprToIdentity (table (Nullable c)) = Maybe (QExprToIdentity (table c))
type instance QExprToIdentity (QExpr s a) = a
type instance QExprToIdentity (a, b) = (QExprToIdentity a, QExprToIdentity b)
type instance QExprToIdentity (a, b, c) = (QExprToIdentity a, QExprToIdentity b, QExprToIdentity c)
type instance QExprToIdentity (a, b, c, d) = (QExprToIdentity a, QExprToIdentity b, QExprToIdentity c, QExprToIdentity d)
type instance QExprToIdentity (a, b, c, d, e) = (QExprToIdentity a, QExprToIdentity b, QExprToIdentity c, QExprToIdentity d, QExprToIdentity e)

-- * Rewriting and optimization

booleanOpts :: SQLExpr -> Maybe SQLExpr
booleanOpts (SQLBinOpE "AND" (SQLValE (SqlBool False)) _) = Just (SQLValE (SqlBool False))
booleanOpts (SQLBinOpE "AND" _ (SQLValE (SqlBool False))) = Just (SQLValE (SqlBool False))
booleanOpts (SQLBinOpE "AND" (SQLValE (SqlBool True)) q) = Just q
booleanOpts (SQLBinOpE "AND" q (SQLValE (SqlBool True))) = Just q

booleanOpts (SQLBinOpE "OR" q (SQLValE (SqlBool False))) = Just q
booleanOpts (SQLBinOpE "OR" (SQLValE (SqlBool False)) q) = Just q
booleanOpts (SQLBinOpE "OR" (SQLValE (SqlBool True)) (SQLValE (SqlBool True))) = Just (SQLValE (SqlBool True))

booleanOpts _ = Nothing

-- | Rewrite function to optimize an expression, will return `Nothing`
-- if the current expression cannot be rewritten any further. Suitable
-- to be passed to `rewriteM`.
allExprOpts :: Applicative f => SQLExpr -> f (Maybe SQLExpr)
allExprOpts e = pure (booleanOpts e)

-- | Given a `SQLExpr' QField` optimize the expression and turn it into a `SQLExpr`.
optimizeExpr' :: SQLExpr' QField -> SQLExpr
optimizeExpr' = runIdentity . rewriteM allExprOpts . fmap mkSqlField

-- | Optimize a `QExpr` and turn it in into a `SQLExpr`.
optimizeExpr :: QExpr s a -> SQLExpr
optimizeExpr (QExpr e) = optimizeExpr' e

mkSqlField :: QField -> SQLFieldName
mkSqlField (QField _ (Just tblOrd) fieldNm) = SQLQualifiedFieldName fieldNm ("t" <> fromString (show tblOrd))
mkSqlField (QField _ Nothing fieldNm) = SQLFieldName fieldNm

-- | Turn a `Q` into a `SQLSelect` starting the table references at the given number
queryToSQL' :: Projectible a => Q db s a -> Int -> (a, Int, SQLSelect)
queryToSQL' q curTbl = let (res, qb) = runState (runQ q) emptyQb
                           emptyQb = QueryBuilder curTbl Nothing (SQLValE (SqlBool True)) Nothing Nothing [] Nothing
                           projection = map (\e -> SQLAliased (optimizeExpr' e) Nothing) (project res)

                           sel = SQLSelect
                                 { selProjection = SQLProj projection
                                 , selFrom = qbFrom qb
                                 , selWhere = optimizeExpr' (qbWhere qb)
                                 , selGrouping = qbGrouping qb
                                 , selOrderBy = qbOrdering qb
                                 , selLimit = qbLimit qb
                                 , selOffset = qbOffset qb }
                       in (res, qbNextTblRef qb, sel)
