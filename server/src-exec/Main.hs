module Main where

import Hasura.Prelude

import qualified Data.HashMap.Strict.Extended as Map
import qualified Data.HashSet as Set
import qualified Language.GraphQL.Draft.Syntax as G

import Control.Arrow.Extended
import Control.Monad.Unique

import Hasura.GraphQL.Context
import Hasura.GraphQL.Execute.Types
import Hasura.GraphQL.Parser.Monad
import Hasura.GraphQL.Parser.Column
import Hasura.GraphQL.Parser.Class
import Hasura.GraphQL.Schema
import Hasura.RQL.Types
import Hasura.SQL.Types

newtype AppM a = AppM { unAppM :: ReaderT SQLGenCtx IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadUnique)

instance HasSQLGenCtx m => HasSQLGenCtx (ExceptT e m) where
  askSQLGenCtx = lift askSQLGenCtx

instance HasSQLGenCtx AppM where
  askSQLGenCtx = AppM ask

deriving instance Show a => Show (QueryDB a)
deriving instance Show a => Show (ActionQuery a)
deriving instance Show ParseError
deriving instance Show UnpreparedValue
deriving instance Show PGColumnValue
deriving instance Show QueryReusability

main :: IO ()
main = do
  let inputs =
        ( QueryHasura
        , Map.fromListOn (_tciName._tiCoreInfo)
          [ TableInfo
            { _tiCoreInfo = TableCoreInfo
              { _tciName = QualifiedObject "public" "authors"
              , _tciDescription = Nothing
              , _tciSystemDefined = SystemDefined False
              , _tciFieldInfoMap = Map.fromListOn fieldInfoName
                [ FIColumn PGColumnInfo
                  { pgiColumn = "id"
                  , pgiName = $$(G.litName "id")
                  , pgiPosition = 0
                  , pgiType = PGColumnScalar PGInteger
                  , pgiIsNullable = False
                  , pgiDescription = Nothing
                  }
                , FIColumn PGColumnInfo
                  { pgiColumn = "name"
                  , pgiName = $$(G.litName "name")
                  , pgiPosition = 0
                  , pgiType = PGColumnScalar PGText
                  , pgiIsNullable = False
                  , pgiDescription = Nothing
                  }
                ]
              , _tciPrimaryKey = Nothing
              , _tciUniqueConstraints = mempty
              , _tciForeignKeys = mempty
              , _tciViewInfo = Nothing
              , _tciEnumValues = Nothing
              , _tciCustomConfig = emptyTableConfig
              }
            , _tiRolePermInfoMap = Map.fromList $ map ("user",)
              [ RolePermInfo
                  { _permIns = Nothing
                  , _permSel = Just SelPermInfo
                    { spiCols = Set.fromList ["id", "name"]
                    , spiScalarComputedFields = Set.fromList []
                    , spiFilter = gBoolExpTrue
                    , spiLimit = Nothing
                    , spiAllowAgg = True
                    , spiRequiredHeaders = []
                    }
                  , _permUpd = Nothing
                  , _permDel = Nothing
                  }
              ]
            , _tiEventTriggerInfoMap = mempty
            }
          ]
        , mempty
        , mempty
        , mempty
        , mempty
        )
  ((_, ctxt), _) <- buildGQLContext
    & runWriterA
    & runKleisli
    & ($ inputs)
    & runExceptT
    & unAppM
    & flip runReaderT (SQLGenCtx False)
    >>= (`onLeft` (error . show))

  let query =
        [ G.SelectionField G.Field
          { _fAlias = Nothing
          , _fName = $$(G.litName "__typename")
          , _fArguments = mempty
          , _fDirectives = []
          , _fSelectionSet = []
          }
        ]
  print $ gqlQueryParser ctxt query


{-

import           Control.Exception
import           Data.Text.Conversions      (convertText)

import           Hasura.App
import           Hasura.Logging             (Hasura)
import           Hasura.Prelude
import           Hasura.RQL.DDL.Metadata    (fetchMetadata)
import           Hasura.RQL.DDL.Schema
import           Hasura.RQL.Types
import           Hasura.Server.Init
import           Hasura.Server.Migrate      (downgradeCatalog, dropCatalog)
import           Hasura.Server.Version

import qualified Data.ByteString.Char8      as BC
import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.Environment           as Env
import qualified Database.PG.Query          as Q
import qualified Hasura.Tracing             as Tracing
import qualified System.Exit                as Sys
import qualified System.Posix.Signals       as Signals
import qualified System.Metrics             as EKG

main :: IO ()
main = do
  tryExit $ do
    args <- parseArgs
    env  <- Env.getEnvironment
    unAppM (runApp env args)
  where
    tryExit io = try io >>= \case
      Left (ExitException _code msg) -> BC.putStrLn msg >> Sys.exitFailure
      Right r -> return r

runApp :: Env.Environment -> HGEOptions Hasura -> AppM ()
runApp env (HGEOptionsG rci hgeCmd) =
  withVersion $$(getVersionFromEnvironment) $ case hgeCmd of
    HCServe serveOptions -> do
      (initCtx, initTime) <- initialiseCtx env hgeCmd rci
      ekgStore <- liftIO EKG.newStore
      let shutdownApp = return ()
      -- Catches the SIGTERM signal and initiates a graceful shutdown.
      -- Graceful shutdown for regular HTTP requests is already implemented in
      -- Warp, and is triggered by invoking the 'closeSocket' callback.
      -- We only catch the SIGTERM signal once, that is, if the user hits CTRL-C
      -- once again, we terminate the process immediately.
      _ <- liftIO $ Signals.installHandler
        Signals.sigTERM
        (Signals.CatchOnce (shutdownGracefully initCtx))
        Nothing
      runHGEServer env serveOptions initCtx Nothing initTime shutdownApp Nothing ekgStore

    HCExport -> do
      (initCtx, _) <- initialiseCtx env hgeCmd rci
      res <- runTx' initCtx fetchMetadata Q.ReadCommitted
      either (printErrJExit MetadataExportError) printJSON res

    HCClean -> do
      (initCtx, _) <- initialiseCtx env hgeCmd rci
      res <- runTx' initCtx dropCatalog Q.ReadCommitted
      either (printErrJExit MetadataCleanError) (const cleanSuccess) res

    HCExecute -> do
      (InitCtx{..}, _) <- initialiseCtx env hgeCmd rci
      queryBs <- liftIO BL.getContents
      let sqlGenCtx = SQLGenCtx False
      res <- runAsAdmin _icPgPool sqlGenCtx _icHttpManager $ do
        schemaCache <- buildRebuildableSchemaCache env
        execQuery env queryBs
          & Tracing.runTraceTWithReporter Tracing.noReporter "execute"
          & runHasSystemDefinedT (SystemDefined False)
          & runCacheRWT schemaCache
          & fmap (\(res, _, _) -> res)
      either (printErrJExit ExecuteProcessError) (liftIO . BLC.putStrLn) res

    HCDowngrade opts -> do
      (InitCtx{..}, initTime) <- initialiseCtx env hgeCmd rci
      let sqlGenCtx = SQLGenCtx False
      res <- downgradeCatalog opts initTime
             & runAsAdmin _icPgPool sqlGenCtx _icHttpManager
      either (printErrJExit DowngradeProcessError) (liftIO . print) res

    HCVersion -> liftIO $ putStrLn $ "Hasura GraphQL Engine: " ++ convertText currentVersion
  where
    runTx' initCtx tx txIso =
      liftIO $ runExceptT $ Q.runTx (_icPgPool initCtx) (txIso, Nothing) tx

    cleanSuccess = liftIO $ putStrLn "successfully cleaned graphql-engine related data"

-}
