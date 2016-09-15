{-# LANGUAGE MultiWayIf, OverloadedStrings, PatternGuards, RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving                                            #-}
module GHC.TypeLits.Symbols.Solver
    ( plugin
    ) where
import Control.Arrow
import Control.Monad.Reader
import Data.Maybe
import DataCon
import FastString
import GHC.TcPluginM.Extra
import GhcPlugins
import TcEvidence
import TcPluginM            hiding (newWanted)
import TcRnTypes
import TyCoRep

plugin :: Plugin
plugin = defaultPlugin { tcPlugin = const $ Just deconsPlugin }

deconsPlugin :: TcPlugin
deconsPlugin =
  tracePlugin "typelits-ghc-symbols" $
  TcPlugin { tcPluginInit  = return ()
           , tcPluginSolve = deconsPluginMain
           , tcPluginStop  = const $ return ()
           }

data MyEnv = MyEnv { viewsym :: TyCon
                   , append  :: TyCon
                   , symnil  :: TyCon
                   , symcons :: TyCon
                   }
         deriving (Eq, Ord)

deconsPluginMain :: () -> [Ct] -> [Ct] -> [Ct] -> TcPluginM TcPluginResult
deconsPluginMain _ gs [] [] = return $ TcPluginOk [] []
deconsPluginMain _ gs ds ws = do
  m <- lookupModule
       (mkModuleName "GHC.TypeLits.Symbols.Internal")
       (mkFastString "ghc-typelits-symbols") :: TcPluginM Module
  viewsym <- tcLookupTyCon =<< lookupOrig m (mkTcOcc "ViewSymbol")
  append  <- tcLookupTyCon =<< lookupOrig m (mkTcOcc "+++")
  symnil  <- promoteDataCon <$> (tcLookupDataCon =<< lookupOrig m (mkDataOcc "SymNil"))
  symcons <- promoteDataCon <$> (tcLookupDataCon =<< lookupOrig m (mkDataOcc "SymCons"))
  (sol'd, news) <- unzip . catMaybes <$> runMachine MyEnv{..} (mapM procCt ws)
  return $ TcPluginOk sol'd (map mkNonCanonical news)
  return $ TcPluginOk [] []

type Machine = ReaderT MyEnv TcPluginM

runMachine :: MyEnv -> Machine a -> TcPluginM a
runMachine e act = runReaderT act e

isEmptySym :: Type -> Bool
isEmptySym (LitTy (StrTyLit e)) = nullFS e
isEmptySym _ = False

singletonFS :: Char -> FastString
singletonFS = flip consFS nilFS

procCt :: Ct -> Machine (Maybe ((EvTerm, Ct), CtEvidence))
procCt ct
  | EqPred _ l r <- classifyPredType (ctPred ct)  = do
    l' <- simplify l
    r' <- simplify r
    if isJust l' || isJust r'
      then Just . (,) (evByFiat "ghc-typelits-symbols" l r, ct)
             <$> lift (newWanted (ctLoc ct) (mkPrimEqPred (fromMaybe l l') (fromMaybe r r')))
      else return Nothing
procCt _ = return Nothing

simplify :: Type -> Machine (Maybe Type)
simplify t = do
  MyEnv{..} <- ask
  case splitTyConApp_maybe t  of
    Just (con, [a0]) | con == viewsym -> do
      a <- fromMaybe a0 <$> simplify a0
      case a of
        LitTy (StrTyLit e)
          | nullFS e  -> return $ Just $ TyConApp symnil []
          | otherwise -> return $ Just $ TyConApp symcons $
                         map (LitTy . StrTyLit)
                         [ singletonFS (headFS e)
                         , tailFS e
                         ]
        _             -> return Nothing
    Just (con, [l0, r0]) | con == append -> do
      (l, r) <- (,) <$> (fromMaybe l0 <$> simplify l0)
                    <*> (fromMaybe r0 <$> simplify r0)
      if | isEmptySym l && isEmptySym r -> return $ Just $ LitTy $ StrTyLit nilFS
         | isEmptySym l                 -> return $ Just l
         | isEmptySym r                 -> return $ Just r
         | LitTy (StrTyLit l') <- l,
           LitTy (StrTyLit r') <- r     -> return $ Just $ LitTy $ StrTyLit (appendFS l' r')
         | otherwise                    -> return Nothing
    _ -> return Nothing
