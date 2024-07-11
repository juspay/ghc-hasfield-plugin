{-# LANGUAGE CPP                    #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PatternSynonyms        #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE UndecidableInstances   #-}

-- | Thin compatibility layer around GHC
--
-- This should be the only module with GHC-specific CPP directives, and the
-- rest of the plugin should not import from any GHC modules directly.
module Data.Record.Plugin.Shim (
    -- * Miscellaneous
    importDecl
  , conPat
  , mkFunBind
  , HsModule
  , LHsModule
  , LRdrName
  , pattern GHC.HsModule
  , putLogMsg
  , patLoc
  , viewConPat

    -- * Extensions
  , HasDefaultExt(..)

    -- * Generalized @forall@
#if __GLASGOW_HASKELL__ >= 900
  , HsTyVarBndr
  , LHsTyVarBndr
#endif
  , hsFunTy
  , userTyVar
  , kindedTyVar
  , hsTyVarLName
  , setDefaultSpecificity
  , mkSrcSpanAnn

    -- * Re-exports

    -- The whole-sale module exports are not ideal for preserving compatibility
    -- across ghc versions, but we'll deal with this on a case by case basis.
#if __GLASGOW_HASKELL__ < 900
  , module Bag
  , module BasicTypes
  , module ErrUtils
  , module GHC
  , module GhcPlugins
  , module HscMain
  , module NameCache
  , module TcEvidence
#else
  , module GHC.Data.Bag
  , module GHC.Driver.Main
  , module GHC.Hs
  , module GHC.Plugins
  , module GHC.Tc.Types.Evidence
  , module GHC.Types.Name.Cache
  , module GHC.Utils.Error
#endif
#if __GLASGOW_HASKELL__ >= 902
  , module GHC.Types.SourceText
  , module GHC.Driver.Errors
  , module GHC.Utils.Logger
#else
  , module GHC.Types.Basic
#endif
  ) where

import Data.List.NonEmpty (NonEmpty(..))

import qualified Data.List.NonEmpty as NE

#if __GLASGOW_HASKELL__ < 900

import Bag (listToBag, emptyBag)
import BasicTypes (SourceText(NoSourceText))
import ConLike (ConLike)
import ErrUtils (mkErrMsg, mkWarnMsg)
import GHC hiding (AnnKeywordId(..), HsModule, exprType, typeKind, mkFunBind)
import GhcPlugins hiding ((<>), getHscEnv, putLogMsg)
import HscMain (getHscEnv)
import NameCache (NameCache(nsUniqs))
import PatSyn (PatSyn)
import TcEvidence (HsWrapper(WpHole))

import qualified GHC
import qualified GhcPlugins as GHC

#else

import GHC.Core.Class (Class)
import GHC.Core.ConLike (ConLike)
import GHC.Core.PatSyn (PatSyn)
import GHC.Data.Bag (listToBag, emptyBag)
import GHC.Driver.Main (getHscEnv)
import GHC.Hs hiding (LHsTyVarBndr, HsTyVarBndr, HsModule, mkFunBind, AnnType, AnnRec, AnnLet, AnnLam, AnnCase)
import GHC.Parser.Annotation (IsUnicodeSyntax(NormalSyntax))
import GHC.Plugins hiding ((<>), getHscEnv, putLogMsg)
import GHC.Tc.Types.Evidence (HsWrapper(WpHole))
import GHC.Types.Name.Cache (NameCache(nsUniqs))

import qualified GHC.Hs      as GHC
import qualified GHC.Plugins as GHC

#endif

#if __GLASGOW_HASKELL__ >= 902
import GHC.Types.SourceText (SourceText(NoSourceText))
import GHC.Utils.Logger (getLogger)
import qualified GHC.Utils.Logger as GHC (putLogMsg, Logger, LogAction)
import GHC.Utils.Error (Severity(SevError, SevWarning), mkErr, mkWarnMsg)
import GHC.Driver.Errors (printOrThrowWarnings)
#elif __GLASGOW_HASKELL__ >= 900 && __GLASGOW_HASKELL__ < 902
import GHC.Types.Basic (SourceText(NoSourceText))
import GHC.Utils.Error (Severity(SevError, SevWarning), mkErrMsg, mkWarnMsg)
#endif

{-------------------------------------------------------------------------------
  Miscellaneous
-------------------------------------------------------------------------------}

-- | Optionally @qualified@ import declaration
importDecl :: ModuleName -> Bool -> LImportDecl GhcPs
importDecl name qualified = noLocA $ ImportDecl {
#if __GLASGOW_HASKELL__ < 902
    ideclExt       = defExt
#else
    ideclExt       = noAnn
#endif
    , ideclSourceSrc = NoSourceText
#if __GLASGOW_HASKELL__ < 902
    , ideclName      = noLoc name
#else
    , ideclName      = noLocA name
#endif
    , ideclPkgQual   = Nothing
    , ideclSafe      = False
    , ideclImplicit  = False
    , ideclAs        = Nothing
    , ideclHiding    = Nothing
#if __GLASGOW_HASKELL__ < 810
    , ideclQualified = qualified
#else
    , ideclQualified = if qualified then QualifiedPre else NotQualified
#endif
#if __GLASGOW_HASKELL__ < 900
    , ideclSource    = False
#else
    , ideclSource    = NotBoot
#endif
    }

#if __GLASGOW_HASKELL__ < 902
conPat :: Located RdrName -> HsConPatDetails GhcPs -> Pat GhcPs
#else
conPat :: XRec GhcPs (ConLikeP GhcPs) -> HsConPatDetails GhcPs -> Pat GhcPs
#endif
#if __GLASGOW_HASKELL__ < 900
conPat x y = ConPatIn x y
#elif __GLASGOW_HASKELL__ >= 900 && __GLASGOW_HASKELL__ < 902
conPat x y = ConPat noExtField x y
#else
conPat x y = ConPat EpAnnNotUsed x y
#endif

#if __GLASGOW_HASKELL__ < 902
mkFunBind :: Located RdrName -> [LMatch GhcPs (LHsExpr GhcPs)] -> HsBind GhcPs
#else
mkFunBind :: LocatedN RdrName
          -> [GenLocated
               SrcSpanAnnA (Match GhcPs (GenLocated SrcSpanAnnA (HsExpr GhcPs)))]
          -> HsBind GhcPs
#endif
#if __GLASGOW_HASKELL__ < 810
mkFunBind = GHC.mkFunBind
#else
mkFunBind = GHC.mkFunBind Generated
#endif

#if __GLASGOW_HASKELL__ < 900
type HsModule = GHC.HsModule GhcPs
#else
type HsModule = GHC.HsModule
#endif

type LHsModule = Located HsModule
type LRdrName  = Located RdrName

#if __GLASGOW_HASKELL__ < 902
putLogMsg :: DynFlags -> WarnReason -> Severity -> SrcSpan -> SDoc -> IO ()
#else
putLogMsg :: GHC.Logger -> GHC.LogAction
#endif
#if __GLASGOW_HASKELL__ < 900
putLogMsg flags reason sev srcspan =
    GHC.putLogMsg flags reason sev srcspan (defaultErrStyle flags)
#elif __GLASGOW_HASKELL__ < 902
putLogMsg = GHC.putLogMsg
#else
putLogMsg = GHC.putLogMsg
#endif

{-------------------------------------------------------------------------------
  Extensions
-------------------------------------------------------------------------------}

class HasDefaultExt a where
  defExt :: a

#if __GLASGOW_HASKELL__ < 810
instance HasDefaultExt NoExt where
  defExt = noExt
#else
instance HasDefaultExt NoExtField where
  defExt = noExtField
#endif

#if __GLASGOW_HASKELL__ >= 900
instance HasDefaultExt LayoutInfo where
  defExt = NoLayoutInfo
#endif

{-------------------------------------------------------------------------------
  Generalized @forall@ in 9.0
-------------------------------------------------------------------------------}

#if __GLASGOW_HASKELL__ >= 900
type  HsTyVarBndr pass =  GHC.HsTyVarBndr () pass
type LHsTyVarBndr pass = GHC.LHsTyVarBndr () pass
#endif

hsFunTy :: XFunTy pass -> LHsType pass -> LHsType pass -> HsType pass
#if __GLASGOW_HASKELL__ < 900
hsFunTy = HsFunTy
#else
hsFunTy ext = HsFunTy ext (HsUnrestrictedArrow NormalSyntax)
#endif

#if __GLASGOW_HASKELL__ < 902
userTyVar ::
     XUserTyVar pass
  -> Located (IdP pass)
  -> HsTyVarBndr pass
#else
userTyVar ::
  XUserTyVar pass
  -> XRec pass (IdP pass)
  -> GHC.HsTyVarBndr () pass
#endif
#if __GLASGOW_HASKELL__ < 900
userTyVar = UserTyVar
#else
userTyVar ext = UserTyVar ext ()
#endif

#if __GLASGOW_HASKELL__ < 902
kindedTyVar ::
     XKindedTyVar pass
  -> Located (IdP pass)
  -> LHsKind pass
  -> HsTyVarBndr pass
#else
kindedTyVar ::
  XKindedTyVar pass
  -> LIdP pass
  -> LHsKind pass
  -> GHC.HsTyVarBndr () pass
#endif
#if __GLASGOW_HASKELL__ < 900
kindedTyVar = KindedTyVar
#else
kindedTyVar ext = KindedTyVar ext ()
#endif

-- | Like 'hsTyVarName', but don't throw away the location information
#if __GLASGOW_HASKELL__ < 902
hsTyVarLName :: HsTyVarBndr GhcPs -> LRdrName
#else
hsTyVarLName :: GHC.HsTyVarBndr flag GhcPs -> LIdP GhcPs
#endif
#if __GLASGOW_HASKELL__ < 900
hsTyVarLName (UserTyVar   _ n  ) = n
hsTyVarLName (KindedTyVar _ n _) = n
hsTyVarLName _ = panic "hsTyVarLName"
#else
hsTyVarLName (UserTyVar   _ _ n  ) = n
hsTyVarLName (KindedTyVar _ _ n _) = n
#endif

#if __GLASGOW_HASKELL__ < 900
setDefaultSpecificity :: LHsTyVarBndr pass -> GHC.LHsTyVarBndr pass
setDefaultSpecificity = id
#elif __GLASGOW_HASKELL__  >= 900 && __GLASGOW_HASKELL__ < 902
setDefaultSpecificity :: LHsTyVarBndr pass -> GHC.LHsTyVarBndr Specificity pass
setDefaultSpecificity (L l v) = L l $ case v of
    UserTyVar   ext () name      -> UserTyVar   ext SpecifiedSpec name
    KindedTyVar ext () name kind -> KindedTyVar ext SpecifiedSpec name kind
    XTyVarBndr  ext              -> XTyVarBndr  ext
#elif __GLASGOW_HASKELL__  >= 902
setDefaultSpecificity :: GenLocated l (GHC.HsTyVarBndr () pass)
                      -> GenLocated l (GHC.HsTyVarBndr Specificity pass)
setDefaultSpecificity (L l v) = L l $ case v of
    UserTyVar   ext () name      -> UserTyVar   ext SpecifiedSpec name
    KindedTyVar ext () name kind -> KindedTyVar ext SpecifiedSpec name kind
    XTyVarBndr  ext              -> XTyVarBndr  ext
#endif

-- New helper function
mkSrcSpanAnn :: SrcSpan -> SrcSpanAnnA
mkSrcSpanAnn l = SrcSpanAnn EpAnnNotUsed l

patLoc :: SrcSpan -> Pat (GhcPass id) -> LPat (GhcPass id)
#if __GLASGOW_HASKELL__ >= 810 && __GLASGOW_HASKELL__ <= 920
patLoc l p = L l' p
  where
    l' = mkSrcSpanAnn l
#else
patLoc _ p = p
#endif

#if __GLASGOW_HASKELL__ < 810
viewConPat :: LPat (GhcPass id) -> Maybe (Located (IdP (GhcPass id)), HsConPatDetails (GhcPass id))
viewConPat (ConPatIn a b) = Just (a, b)
#elif __GLASGOW_HASKELL__ >= 810 && __GLASGOW_HASKELL__ < 900
viewConPat :: LPat (GhcPass id) -> Maybe (Located (IdP (GhcPass id)), HsConPatDetails (GhcPass id))
viewConPat (L _ (ConPatIn a b)) = Just (a, b)
#elif __GLASGOW_HASKELL__ >= 900
viewConPat :: LPat (GhcPass id) -> Maybe (XRec (GhcPass id) (ConLikeP (GhcPass id)), HsConPatDetails (GhcPass id))
viewConPat (L _ (ConPat _ext a b)) = Just (a, b)
#endif
viewConPat _ = Nothing

