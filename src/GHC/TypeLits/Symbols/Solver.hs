{-# LANGUAGE LambdaCase, MultiWayIf, OverloadedStrings, PatternGuards #-}
{-# LANGUAGE RecordWildCards, StandaloneDeriving                      #-}
module GHC.TypeLits.Symbols.Solver
    ( plugin
    ) where
import Control.Monad.Reader
import Data.Maybe
import DataCon
import FastString
import GHC.TcPluginM.Extra
import GhcPlugins           hiding (substTy)
import TcEvidence
import TcPluginM            hiding (newWanted)
import TcRnTypes
import TyCoRep
import Unify

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
deconsPluginMain _ _gs [] [] = return $ TcPluginOk [] []
deconsPluginMain _ gs ds ws = do
  let subst = foldr unionTCvSubst emptyTCvSubst $ map genSubst (gs ++ ds)
  tcPluginTrace "(givens, deriveds, wanteds) = " $ ppr (gs, ds, map (substTy subst . ctPred) ws)
  m <- lookupModule
       (mkModuleName "GHC.TypeLits.Symbols.Internal")
       (mkFastString "ghc-typelits-symbols") :: TcPluginM Module
  viewsym <- tcLookupTyCon =<< lookupOrig m (mkTcOcc "ViewSymbol")
  append  <- tcLookupTyCon =<< lookupOrig m (mkTcOcc "+++")
  symnil  <- promoteDataCon <$> (tcLookupDataCon =<< lookupOrig m (mkDataOcc "SymNil"))
  symcons <- promoteDataCon <$> (tcLookupDataCon =<< lookupOrig m (mkDataOcc "SymCons"))
  (sol'd, news) <- unzip . catMaybes <$> runMachine MyEnv{..} (mapM (procCt subst) ws)
  tcPluginTrace "(sol's, news) = " $ ppr $ zip sol'd news
  return $ TcPluginOk sol'd (map mkNonCanonical news)

genSubst :: Ct -> TCvSubst
genSubst ct = case classifyPredType (ctPred ct) of
  EqPred NomEq t u -> fromMaybe emptyTCvSubst $ tcUnifyTy t u
  _ -> emptyTCvSubst

type Machine = ReaderT MyEnv TcPluginM

runMachine :: MyEnv -> Machine a -> TcPluginM a
runMachine e act = runReaderT act e

isEmptySym :: Type -> Bool
isEmptySym (LitTy (StrTyLit e)) = nullFS e
isEmptySym _ = False

singletonFS :: Char -> FastString
singletonFS = flip consFS nilFS

procCt :: TCvSubst -> Ct -> Machine (Maybe ((EvTerm, Ct), CtEvidence))
procCt subst ct = simplify (substTy subst $ ctPred ct) >>= \case
  Just tp' ->
    case classifyPredType tp' of
      EqPred{} -> do
        w' <- lift (newWanted (ctLoc ct) tp')
        return $
          Just ((evByFiat "ghc-typelits-symbols" (ctPred ct) tp', ct), w')
      ClassPred {} -> do
        w' <- lift (newWanted (ctLoc ct) tp')
        let tm = ctEvTerm w'
            caster = mkUnivCo (PluginProv "ghc-typelits-symbols")
                     Nominal tp' (ctPred ct)
        return $
          Just ((mkEvCast tm caster, ct), w')
      IrredPred _pr -> return Nothing
  Nothing -> return Nothing

simplify :: Type -> Machine (Maybe Type)
simplify t = do
  lift $ tcPluginTrace "Simplifying: " $ ppr t
  MyEnv{..} <- ask
  case t of
    ForAllTy bnds tp -> do
      fmap (ForAllTy bnds) <$> simplify tp
    TyVarTy{} -> return Nothing
    AppTy l r -> do
      (l', r') <- (,) <$> simplify l <*> simplify r
      if isJust l' || isJust r'
        then do
          lift $ tcPluginTrace "Simplified(AT): " $ ppr (t, AppTy (fromMaybe l l') (fromMaybe r r'))
          return $ Just $ AppTy (fromMaybe l l') (fromMaybe r r')
        else lift (tcPluginTrace "Failed! (AT): " (ppr t)) >> return Nothing
    TyConApp tc [a0] | tc == viewsym -> do
      a' <- simplify a0
      let a = fromMaybe a0 a'
      case a of
        LitTy (StrTyLit e)
          | nullFS e  -> return $ Just $ TyConApp symnil []
          | otherwise -> return $ Just $ TyConApp symcons $
                         map (LitTy . StrTyLit)
                         [ singletonFS (headFS e)
                         , tailFS e
                         ]
        _ | isJust a' -> return $ Just $ TyConApp viewsym [a]
        _             -> lift (tcPluginTrace "Failed! (view): " (ppr (t, a))) >> return Nothing
    TyConApp tc [l0, r0] | tc == append -> do
      (l', r') <- (,) <$> (simplify l0)
                      <*> (simplify r0)
      let (l, r) = (fromMaybe l0 l', fromMaybe r0 r')
      if | isEmptySym l && isEmptySym r -> return $ Just $ LitTy $ StrTyLit nilFS
         | isEmptySym l                 -> return $ Just r
         | isEmptySym r                 -> return $ Just l
         | LitTy (StrTyLit lfs) <- l,
           LitTy (StrTyLit rfs) <- r     -> return $ Just $ LitTy $ StrTyLit (appendFS lfs rfs)
         | isJust l' || isJust r'       -> return $ Just $ TyConApp append [l, r]
         | otherwise                    -> lift (tcPluginTrace "Failed! (append): " (ppr (t, (l, r)))) >> return Nothing
    TyConApp tc ars -> do
      ars' <- mapM simplify ars
      if any isJust ars'
        then do
          lift $ tcPluginTrace "Simplified(TCA): " $ ppr (t, TyConApp tc $ zipWith fromMaybe ars ars')
          return $ Just $ TyConApp tc $ zipWith fromMaybe ars ars'
        else lift (tcPluginTrace "Failed! (TCA): " (ppr t)) >> return Nothing
    _ -> lift (tcPluginTrace "I don't have any idea" $ ppr t) >>return Nothing
