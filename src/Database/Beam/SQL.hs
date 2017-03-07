{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Database.Beam.SQL
    ( -- * SQL pretty printing
      ppSQLGen

      -- * Untyped SQL types
    , module Database.Beam.SQL.Types
    , (<+>) , (<>), empty, text
    , SQL(..) ) where

import Database.Beam.SQL.Types
import Database.Beam.Internal (BeamBackend)

import Control.Arrow hiding ((<+>))
import Control.Monad.Writer hiding ((<>))

import Data.Text (Text, unpack)

import Database.HDBC

import Text.PrettyPrint

-- * Pretty printing support

-- | Convert a 'SQLCommand' into a SQL expression (with placeholders) and literal values to be submitted to the SQL server.
--   Splitting into a SQL expression and literals prevents SQL injection attacks.

type DocAndVals = Writer [SqlValue] Doc

ppJType :: Applicative f => SQLJoinType -> f Doc
ppJType SQLInnerJoin = pure (text "INNER JOIN")
ppJType SQLLeftJoin =  pure (text "LEFT JOIN")
ppJType SQLRightJoin = pure (text "RIGHT JOIN")
ppJType SQLOuterJoin = pure (text "OUTER JOIN")
ppJType SQLCrossJoin = pure (text "CROSS JOIN")

ppSQLGen :: SQL backend => backend -> SQLCommand -> (String, [SqlValue])
ppSQLGen b c = 
  first render (runWriter (ppCmd c))
  where
    ppCmd :: SQLCommand -> DocAndVals
    ppCmd (Select sel) = ppSel b sel
    ppCmd (CreateTable ct) = ppCreateTable b ct
    ppCmd (Insert i) = ppInsert b i
    ppCmd (Update u) = ppUpdate b u
    ppCmd (Delete d) = ppDelete d
    
      -- ** Delete printing support

    ppDelete :: SQLDelete -> DocAndVals
    ppDelete (SQLDelete tbl where_) =
        do whereClause_ <- case where_ of
                             Nothing -> return empty
                             Just x -> ppWhere b x
           return (text "DELETE FROM" <+> text (unpack tbl) <+> whereClause_)

ppAliased :: (a -> DocAndVals) -> SQLAliased a -> DocAndVals
ppAliased ppSub (SQLAliased x (Just as)) = do sub <- ppSub x
                                              return (sub <+> text "AS" <+> text (unpack as))
ppAliased ppSub (SQLAliased x Nothing) = ppSub x

ppVal :: SqlValue -> DocAndVals
ppVal val = tell [val] >> return (text "?")

class BeamBackend backend => SQL backend where

  
  -- ** Create table printing support

  ppCreateTable :: backend -> SQLCreateTable -> DocAndVals
  ppCreateTable b (SQLCreateTable tblName schema) =
      do fieldSchemas <- mapM (ppFieldSchema b) schema
         let primaryKeys = filter (elem SQLPrimaryKey . csConstraints . snd) schema
             primaryKeyExpr = case primaryKeys of
                                [] -> []
                                keys ->
                                    let pks = map (text . unpack . fst) keys
                                    in [text "PRIMARY KEY(" <+> hsep (punctuate comma pks) <+> text ")" ]
         return (text "CREATE TABLE" <+> text (unpack tblName) <+> parens (hsep (punctuate comma (fieldSchemas ++ primaryKeyExpr))))

  ppFieldSchema :: backend -> (Text, SQLColumnSchema) -> DocAndVals
  ppFieldSchema b (name, colSch) = (text (unpack name) <+>) <$> ppColSchema b colSch

  ppColSchema :: backend -> SQLColumnSchema -> DocAndVals
  ppColSchema b (SQLColumnSchema type_ constraints) =
      do typeDoc <- ppColType b type_
         constraints' <- mapM (ppConstraint b) constraints
         return (typeDoc <+> hsep constraints')

  ppConstraint :: backend -> SQLConstraint -> DocAndVals
  ppConstraint _ SQLPrimaryKey = return empty --(text "PRIMARY KEY")
  --ppConstraint SQLPrimaryKeyAutoIncrement = return (text "PRIMARY KEY")
  ppConstraint _ SQLAutoIncrement = return (text "AUTOINCREMENT") -- TODO this is different dependingon the backend
  ppConstraint _ SQLNotNull = return (text "NOT NULL")

  ppColType :: backend -> SqlColDesc -> DocAndVals
  ppColType _ SqlColDesc { colType = SqlVarCharT
                         , colSize = size } =
      return
        (text "VARCHAR" <+>
              case size of
                Nothing -> parens (text "255") --empty
                Just sz -> parens (text (show sz)))
  ppColType _ SqlColDesc { colType = SqlCharT
                       , colSize = size } =
      return
        (text "CHAR" <+>
              case size of
                Nothing -> parens (text "255")
                Just sz -> parens (text (show sz)))
  ppColType _ SqlColDesc { colType = SqlNumericT } = return (text "INTEGER")
  ppColType _ SqlColDesc { colType = SqlUTCDateTimeT } = return (text "DATETIME")
  ppColType _  _ = error "ppColType undefined for the type"

  -- ** Insert printing support

  ppInsert :: backend -> SQLInsert -> DocAndVals
  ppInsert _ (SQLInsert tblName values) =
      do vals <- mapM ppVal values
         return (text "INSERT INTO" <+> text (unpack tblName) <+> text "VALUES" <+>
                 parens (hsep (punctuate comma vals)))

  -- ** Update printing support

  ppAssignment :: backend -> SQLFieldName -> SQLExpr -> DocAndVals
  ppAssignment b field expr =
      do fieldD <- ppFieldName b field
         exprD  <- ppExpr b expr
         return (fieldD <> text "=" <> exprD)

  ppUpdate :: backend -> SQLUpdate -> DocAndVals
  ppUpdate b (SQLUpdate tbls assignments where_) =
      do assignmentsDs <- mapM (uncurry $ ppAssignment b) assignments
         whereClause_  <- case where_ of
                            Nothing -> return empty
                            Just x -> ppWhere b x
         return (text "UPDATE" <+> hsep (punctuate comma (map (text . unpack) tbls)) <+>
                 text "SET"    <+> hsep (punctuate comma assignmentsDs) <+>
                 whereClause_)

  -- ** Select printing support

  ppSel :: backend -> SQLSelect -> DocAndVals
  ppSel b sel =
      do proj   <- ppProj b (selProjection sel)
         source <- case selFrom sel of
                     Nothing -> pure empty
                     Just from -> do source <- ppFrom b from
                                     pure (text "FROM " <+> source)
         where_ <- ppWhere b (selWhere sel)
         grouping <- case selGrouping sel of
                       Nothing -> return empty
                       Just grouping -> ppGrouping b grouping
         orderBy <- ppOrderBy b (selOrderBy sel)
         let limit = maybe empty ((text "LIMIT" <+>) . text . show) (selLimit sel)
             offset = maybe empty ((text "OFFSET" <+>) . text . show) (selOffset sel)
         return (text "SELECT" <+> proj <+> source <+>
                 where_ <+> grouping <+> orderBy <+> limit <+> offset)

  ppProj :: backend -> SQLProjection -> DocAndVals
  ppProj _ SQLProjStar = return (text "*")
  ppProj b (SQLProj es) =
      do es' <- mapM (ppAliased $ ppExpr b) es
         return (hsep (punctuate comma es'))

  ppSource :: backend -> SQLSource -> DocAndVals
  ppSource _ (SQLSourceTable tbl) = return (text (unpack tbl))
  ppSource b (SQLSourceSelect sel) = parens <$> ppSel b sel

  ppWhere :: backend -> SQLExpr' SQLFieldName -> Writer [SqlValue] Doc
  ppWhere _ (SQLValE v)
      | safeFromSql v == Right True = return empty
  ppWhere b expr = (text "WHERE" <+>) <$> ppExpr b expr

  ppFieldName :: Applicative f => backend -> SQLFieldName -> f Doc
  ppFieldName _ (SQLQualifiedFieldName t table) = pure (text "`" <> text (unpack table) <> text "`.`" <>
                                                       text (unpack t) <> text "`")
  ppFieldName _ (SQLFieldName t) = pure (text (unpack t))

  ppGrouping :: backend -> SQLGrouping -> Writer [SqlValue] Doc
  ppGrouping b grouping = do exprs <- mapM (ppExpr b) (sqlGroupBy grouping)
                             having <-  ppHaving b (sqlHaving grouping)
                             return (text "GROUP BY" <+> hsep (punctuate comma exprs) <+> having)

  ppHaving :: backend -> SQLExpr' SQLFieldName -> Writer [SqlValue] Doc
  ppHaving _ (SQLValE v)
           | safeFromSql v == Right True = return empty
  ppHaving b expr = (text "HAVING" <+>) <$> ppExpr b expr

  ppFrom :: backend -> SQLFrom -> Writer [SqlValue] Doc
  ppFrom b x = fst <$> ppFrom' b x

  ppFrom' :: backend -> SQLFrom -> Writer [SqlValue] (Doc, Bool)
  ppFrom' b (SQLFromSource a) = (,False) <$> ppAliased (ppSource b) a
  ppFrom' b (SQLJoin jType x y on_) = do (xDoc, _) <- ppFrom' b x
                                         jTypeDoc <- ppJType jType
                                         (yDoc, yIsJoin) <- ppFrom' b y
                                         onDoc <- ppOn b on_
                                         return (xDoc <+> jTypeDoc <+> if yIsJoin then yDoc else yDoc <+> onDoc, True)


  ppOn :: backend -> SQLExpr' SQLFieldName -> Writer [SqlValue] Doc
  ppOn _ (SQLValE v)
       | safeFromSql v == Right True = return empty
  ppOn b expr = (text "ON" <+>) <$> ppExpr b expr

  ppOrderBy :: backend -> [SQLOrdering] -> Writer [SqlValue] Doc
  ppOrderBy _ [] = return empty
  ppOrderBy b xs = (text "ORDER BY" <+>) . hsep . punctuate comma <$> mapM ppOrdering xs
      where ppOrdering (Asc e) = do eDoc <- ppExpr b e
                                    return (eDoc <+> text "ASC")
            ppOrdering (Desc e) = do eDoc <- ppExpr b e
                                     return (eDoc <+> text "DESC")

  ppExpr :: backend -> SQLExpr -> DocAndVals
  ppExpr _ (SQLValE v) = ppVal v
  ppExpr b (SQLFieldE name) = ppFieldName b name
  ppExpr b (SQLBinOpE op oa ob) =
      do aD <- ppExpr b oa
         bD <- ppExpr b ob
         return (parens (aD <+> text (unpack op) <+> bD))
  ppExpr b (SQLUnOpE op oa) = do aDoc <- ppExpr b oa
                                 return (parens (text (unpack op) <+> parens aDoc))
  ppExpr b (SQLIsNothingE q) = do qDoc <- ppExpr b q
                                  return (parens (qDoc <+> text "IS NULL"))
  ppExpr b (SQLIsJustE q) = do qDoc <- ppExpr b q
                               return (parens (qDoc <+> text "IS NOT NULL"))
  ppExpr b (SQLListE xs) = do xsDoc <- mapM (ppExpr b) xs
                              return (parens (hsep (punctuate comma xsDoc)))
  ppExpr b (SQLFuncE f args) = do argDocs <- mapM (ppExpr b) args
                                  return (text (unpack f) <> parens (hsep (punctuate comma argDocs)) )
  ppExpr b (SQLExistsE q) = do selectDoc <- ppSel b q
                               pure (parens (text "EXISTS (" <> selectDoc <> text ")"))
  ppExpr b (SQLCaseE clauses else_) = do whenClauses <- forM clauses $ \(cond, then_) ->
                                                        do condDoc <- ppExpr b cond
                                                           thenDoc <- ppExpr b then_
                                                           pure (text "WHEN" <+> condDoc <+> text "THEN" <+> thenDoc)
                                         elseDoc <- ppExpr b else_
                                         pure (parens (text "CASE" <+> hsep whenClauses <+> text "ELSE" <+> elseDoc <+> text "END"))

