module Database.Beam.Backend.Firebird (FirebirdSettings(..), Decimal) where

import Database.Beam.Internal
import Database.Beam.Query
import Database.Beam.Query.Internal
import Database.Beam.Backend

import Database.Beam.SQL

import Control.Monad.Trans

import Database.Beam.Schema.Fields
import Database.HDBC.ODBC
import Database.HDBC

import Data.ByteString.Char8 (pack, ByteString)
--import qualified  Data.ByteString as BS
import Data.Text (Text, unpack)
import Data.Decimal
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


numSchema :: Int -> FieldSchema Decimal
numSchema dec = FieldSchema {
    fsColDesc = notNull
      SqlColDesc{colType = SqlNumericT, colSize = Just 15,
                colOctetLength = Nothing, colDecDigits = Just dec,
                colNullable = Nothing}
  , fsHumanReadable = "numSchema"
  , fsMakeSqlValue = SqlByteString . pack . show
  , fsFromSqlValue = (read . fromSql) <$> popSqlValue }
instance HasDefaultFieldSchema Decimal where
    defFieldSchema = numSchema 6
instance FromSqlValues Decimal

binarySchema :: FieldSchema ByteString
binarySchema = FieldSchema {
    fsColDesc = notNull
      SqlColDesc{colType = SqlLongVarBinaryT, colSize = Nothing,
                colOctetLength = Nothing, colDecDigits = Nothing,
                colNullable = Nothing}
  , fsHumanReadable = "binarySchema"
  , fsMakeSqlValue = SqlByteString
  , fsFromSqlValue = fromSql <$> popSqlValue }
instance HasDefaultFieldSchema ByteString where
    defFieldSchema = binarySchema
instance FromSqlValues ByteString