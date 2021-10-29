{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}

module Language.PureScript.Make.Simplified where

import Control.Monad (foldM)
import Control.Monad.Supply (evalSupplyT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (runStateT)
import Control.Monad.Trans.Writer (runWriterT)
import Data.Foldable (foldl')
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NEL
import Data.Text (Text)
import Language.PureScript qualified as P
import Language.PureScript.AST.Declarations qualified as AST
import Language.PureScript.CoreFn qualified as CoreFn
import Language.PureScript.CST qualified as CST
import Language.PureScript.Errors qualified as Errors
import Language.PureScript.Renamer qualified as Renamer
import Language.PureScript.Sugar.Names.Env qualified as Env
import Language.PureScript.TypeChecker.Monad qualified as TC

data BuildError
  = UnableToParse (NonEmpty CST.ParserError)
  | UnableToCompile Errors.MultipleErrors
  deriving Show

renderBuildError :: BuildError -> String
renderBuildError (UnableToParse xs) =
  unlines $
    "Parser errors:" : NEL.toList (fmap CST.prettyPrintError xs)
renderBuildError (UnableToCompile xs) =
  Errors.prettyPrintMultipleErrors Errors.defaultPPEOptions xs

-- | Parse and build a single PureScript module, returning the compiled CoreFn
-- module.
buildSingleModule :: [P.ExternsFile] -> Text -> Either BuildError (CoreFn.Module CoreFn.Ann, P.ExternsFile)
buildSingleModule externs moduleText = do
  case CST.parseFromFile "<input>" moduleText of
    (_, Left errs) ->
      Left (UnableToParse errs)
    (_, Right m) -> 
      case buildCoreFnOnly externs m of
        Left errs ->
          Left (UnableToCompile errs)
        Right (result, _) -> Right result

-- | Compile a single 'AST.Module' into a CoreFn module.
--
-- This function is based on the 'Language.PureScript.Make.rebuildModule'
-- function.
--
-- It is reproduced and modified here in order to make it simpler to build a 
-- single module without all of the additional capabilities and complexity of
-- the upstream API.
buildCoreFnOnly
  :: [P.ExternsFile]
  -> AST.Module
  -> Either Errors.MultipleErrors ((CoreFn.Module CoreFn.Ann, P.ExternsFile), Errors.MultipleErrors)
buildCoreFnOnly externs m@(AST.Module _ _ moduleName _ _) = runWriterT $ do
  let withPrim = P.importPrim m
      env = foldl' (flip P.applyExternsFileToEnvironment) P.initEnvironment externs
  exEnv <- fmap fst . runWriterT $ foldM P.externsEnv Env.primEnv externs
  evalSupplyT 0 $ do
    (desugared, (exEnv', _)) <- runStateT (P.desugar externs withPrim) (exEnv, mempty)
    let modulesExports = (\(_, _, exports) -> exports) <$> exEnv'
    (checked, TC.CheckState{..}) <- runStateT (P.typeCheckModule modulesExports desugared) $ TC.emptyCheckState env
    let AST.Module ss coms _ elaborated exps = checked
    deguarded <- P.desugarCaseGuards elaborated
    regrouped <- lift . P.createBindingGroups moduleName . P.collapseBindingGroups $ deguarded
    let mod' = AST.Module ss coms moduleName regrouped exps
        corefn = CoreFn.moduleToCoreFn checkEnv mod'
        optimized = CoreFn.optimizeCoreFn corefn
        (renamedIdents, renamed) = Renamer.renameInModule optimized
        newExterns = P.moduleToExternsFile mod' checkEnv renamedIdents
    pure (renamed, newExterns)