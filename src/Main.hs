{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Main where

import Criterion
import Criterion.Main
import System.Random
import qualified Database.PostgreSQL.Simple as S
import qualified Hasql.Session as A
import qualified Hasql.Query as B
import qualified Hasql.Encoders as C
import qualified Hasql.Decoders as D
import qualified Hasql.Connection as E

type Location = (UUID, Double, Double)

main :: IO ()
main = do
        Right hconn <- E.acquire connStr

        pconn <- S.connectPostgreSQL connStr
        _     <- S.execute_ pconn "DROP TABLE IF EXISTS hasql_location"
        _     <- S.execute_ pconn "DROP TABLE IF EXISTS simple_location"
        _     <- S.execute_ pconn "CREATE TABLE hasql_location (id uuid NOT NULL, x double precision NOT NULL, y double precision NOT NULL)"
        _     <- S.execute_ pconn "CREATE TABLE simple_location (id uuid NOT NULL, x double precision NOT NULL, y double precision NOT NULL)"

        defaultMain
            [ env (genData 100) $ \locs ->
                bgroup "Insert 100"
                    [ bench "hasql" $ whnfIO $ insertUsingHasql locs hconn
                    , bench "postgresql-simple" $ whnfIO $ insertUsingPostgresqlSimple locs pconn
                    ]
            , env (genData 1000) $ \locs ->
                bgroup "Insert 1000"
                    [ bench "hasql" $ whnfIO $ insertUsingHasql locs hconn
                    , bench "postgresql-simple" $ whnfIO $ insertUsingPostgresqlSimple locs pconn
                    ]
            ]

insertUsingHasql :: [Location] -> E.Connection -> IO ()
insertUsingHasql locations connection =
  do
    Right result <- A.run session connection
    return result
  where
    session =
      A.query locations query
      where
        query =
          B.statement sql encoder decoder True
          where
            sql =
              "insert into hasql_location (id, x, y) select * from unnest ($1, $2, $3)"
            encoder =
              contramap unzip3 (contrazip3 (list C.uuid) (list C.float8) (list C.float8))
              where
                list value =
                  C.value (C.array (C.arrayDimension foldl' (C.arrayValue value)))
            decoder =
              D.unit

insertUsingPostgresqlSimple :: [Location] -> S.Connection -> IO ()
insertUsingPostgresqlSimple locs pconn = void (S.executeMany pconn query locs)
    where query = "INSERT INTO simple_location VALUES (?, ?, ?)"

connStr :: ByteString
connStr = "host=localhost port=5432 user=postgres dbname=sql_driver_race"

genData :: Int -> IO [Location]
genData i = do
        gen <- newStdGen
        let locs = zip3 (randoms gen) [1..] [1..]
        return $ take i locs
