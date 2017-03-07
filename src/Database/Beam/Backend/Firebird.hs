module Database.Beam.Backend.Firebird where

import Database.Beam.Internal
import Database.Beam.Query
import Database.Beam.Query.Internal
import Database.Beam.Backend

import Database.Beam.SQL

import Control.Monad.Trans

import Database.HDBC.ODBC
import Database.HDBC

import Data.Text (Text, unpack)

-- * Sqlite3 support

newtype FirebirdSettings = FirebirdSettings String
                                                                                                                                                                                                                                                             deriving Show

instance BeamBackend FirebirdSettings where
    openBeam dbSettings b@(FirebirdSettings fp) =
        do conn <- liftIO (connectODBC fp)

           return Beam { beamDbSettings = dbSettings
                       , beamDebug = False
                       , closeBeam = liftIO (disconnect conn)
                       , compareSchemas = defaultBeamCompareSchemas
                       , adjustColDescForBackend =
                           \cs -> cs { csConstraints = filter (/=SQLAutoIncrement) (csConstraints cs) }
                       , getLastInsertedRow = getLastInsertedRow' conn
                       , withHDBCConnection = \f -> f conn 
                       , ppSQL = ppSQLGen b}

getLastInsertedRow' :: MonadIO m => Connection -> Text -> [Text] -> m [SqlValue]
getLastInsertedRow' conn tblName autoIncColsWithNulls = do
  let whereGens = foldr1 (\g1 g2->g1++" and "++g2) $ zipWith (\col gen -> unpack col ++ "=" ++ gen) autoIncColsWithNulls generators
      queryLastRow = concat ["SELECT first 1 * FROM ", tblNameStr, " WHERE ", whereGens]
  --FOR DEBUG  *** REMOVE ***
  liftIO $ putStrLn $ "LastInsertedRow query: " ++ queryLastRow
  [res] <- liftIO $ quickQuery conn queryLastRow []
  return res
  where 
    tblNameStr = unpack tblName
    generators =
      map (\nmCol -> concat ["gen_id(GEN_", tblNameStr, "_", unpack nmCol, ",0)"]) autoIncColsWithNulls

(++.) :: QExpr s Text -> QExpr s Text -> QExpr s Text
QExpr a ++. QExpr b = QExpr (SQLBinOpE "||" a b)

instance SQL FirebirdSettings where 
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
       orderBy_ <- ppOrderBy b (selOrderBy sel)
       let limit = maybe empty ((text "ROWS" <+>) . text . show) (selLimit sel)
           offset = maybe empty ((text "TO" <+>) . text . show) (selOffset sel)
       return (text "SELECT" <+> proj <+> source <+>
               where_ <+> grouping <+> orderBy_ <+> limit <+> offset)
  
  ppFieldName _ (SQLQualifiedFieldName t table) = pure (text "" <> text (unpack table) <> text "." <>
                                                       text (unpack t) <> text "")
  ppFieldName _ (SQLFieldName t) = pure (text (unpack t))
