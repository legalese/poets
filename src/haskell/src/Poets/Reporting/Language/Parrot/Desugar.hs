{-# LANGUAGE TypeSynonymInstances, TypeOperators, FlexibleInstances, FlexibleContexts, TemplateHaskell #-}

--------------------------------------------------------------------------------
-- |
-- Module      : Poets.Reporting.Language.Parrot.Desugar
-- Copyright   : 3gERP, 2010
-- License     : All Rights Reserved
--
-- Maintainer  :  Patrick Bahr, Joakim Ahnfelt, Jon Elverkilde, Michael Werk
-- Stability   :  unknown
-- Portability :  unknown
--
-- This module provides functionality to convert a Parrot program with
-- syntactic sugar to one without.
--
--------------------------------------------------------------------------------
module Poets.Reporting.Language.Parrot.Desugar 
    (
     DesugarAlg(..),
     desugar,
     desugarProgram,
     desugarLibrary
    ) where

import Poets.Reporting.Language.Parrot.Syntax
import Poets.Reporting.Language.Parrot.FreshVar
import Control.Monad.Identity hiding (sequence)
import Prelude hiding (sequence)
import Data.Maybe
import Data.Comp.Derive

{-| This class defines algebras which allow to convert Parrot
    expressions with suger (anything not included in ExpCoreSigPos) 
    into Parrot expressions without sugar (ExpCoreSigPos).-}
class Functor f => DesugarAlg f where
    desugarAlg :: (MonadFreshVar VVarId m) => f (Term ExpCoreSigPos) -> m (Term ExpCoreSigPos)

$(derive [liftSum] [''DesugarAlg])

instance DesugarAlg (Val :&: SrcPos) where
    desugarAlg = return . inject

instance DesugarAlg (ValueExt :&: SrcPos) where
    desugarAlg = return . inject

instance DesugarAlg (ExpExt :&: SrcPos) where
    desugarAlg = return . inject

instance DesugarAlg (ExpSugar :&: SrcPos) where
    desugarAlg (sugar :&: position) = case sugar of
        BinOpSugar e1 op e2 ->
            case op of 
                OpAppend -> do
                    f <- desugarAlg (OperatorSection (NormalSection OpCons NoOperand) :&: position)
                    return $ mkFold f e2 e1
        Pick rec -> do 
            xs <- freshVar
            x <- freshVar
            comprehension <- desugarAlg $ ListComprehension (exp $ VVar x) [ListGenerator x (Just rec) (exp $ VVar xs)] :&: position
            return (exp $ VLam [xs] comprehension)
        TypePred e rec -> do 
            typeId <- freshVar
            let typeCase = mkTypeOf typeId rec (exp $ VBool True) (exp $ VBool False)
            return (exp $ Let [LetBind typeId [] e :&: position] typeCase)
        OperatorSection section -> case section of
            NormalSection op operand -> desugarSection (flip BinOpCore op) operand
            TypePredSection recordName -> do
                leftId <- freshVar
                typePred <- desugarAlg (TypePred (exp $ VVar leftId) recordName :&: position)
                return $ exp $ VLam [leftId] typePred
            where
                desugarSection operator operand = do
                leftId <- freshVar
                rightId <- freshVar
                return $ case operand of
                    NoOperand -> exp $ VLam [leftId, rightId] $ exp $ operator (exp $ VVar leftId) (exp $ VVar rightId)
                    LeftOperand e -> exp $ VLam [rightId] $ exp $ operator e (exp $ VVar rightId)
                    RightOperand e -> exp $ VLam [leftId] $ exp $ operator (exp $ VVar leftId) e
        ListComprehension eOpen [] -> return $ exp (VList [eOpen])
        ListComprehension eOpen (filter:filters) -> do
            accVar <- freshVar
            underscore <- freshVar 
            let (x, typeGuard, e) = normalizeFilter filter      
            rest <- desugarAlg $ ListComprehension eOpen filters :&: position
            appendRest <- mkAppend rest (exp (VVar accVar))
            let xVar = fromMaybe underscore x
            lambda <- case typeGuard of 
                Just t -> return $ 
                    exp (VLam [xVar, accVar] (mkTypeOf xVar t appendRest (exp (VVar accVar))))
                Nothing -> return $ 
                    exp (VLam [xVar, accVar] appendRest)
            return $ mkFold lambda (exp (VList [])) e
            where
              guard e = exp $ If e (exp $ VList [exp VUnit]) (exp $ VList [])
              normalizeFilter (ListGuard e) = (Nothing, Nothing, guard e) 
              normalizeFilter (ListLet x typeName e) = (Just x, typeName, exp $ VList [e])
              normalizeFilter (ListGenerator x typeName e) = (Just x, typeName, e)                   
        where
            exp e = inject (e :&: position)
            mkFold f init l = exp $ App (exp $ App (exp $ App (exp $ PrimOp Fold) f) init) l 
            mkAppend l1 l2 = desugarAlg (BinOpSugar l1 OpAppend l2 :&: position)
            mkTypeOf boundVar typeVar case1 case2 = 
                let trueCase = TypeCase typeVar case1 :&: position
                    falseCase = Just (DefTypeCase case2 :&: position)
                in exp $ TypeOf (Just boundVar) (inject $ VVar boundVar :&: position) [trueCase] falseCase


{-| This function desugars a Parrot expression. -}
desugar :: Term ExpSigPos -> Term ExpCoreSigPos
desugar = runIdentity . runFreshVarT . cataM desugarAlg

{-| This function desugars a Parrot declaration. -}
desugarDecl :: DeclSugar -> DeclCore
desugarDecl declSugar@FunDecl { declBody = body } = declSugar { declBody = desugar body }
desugarDecl d = RecDecl {
                  declPos = declPos d,
                  declRec = declRec d,
                  declSuper = declSuper d,
                  declFields = declFields d}

{-| This function desugars  a list of declarations. -}
desugarDecls :: [DeclSugar] -> [DeclCore]
desugarDecls decls = map desugarDecl decls


{-| This function desugars all expressions in a Parrot program. -}
desugarLibrary :: LibrarySugar -> LibraryCore
desugarLibrary (Library decls) =  Library $ desugarDecls decls

{-| This function desugars all expressions in a Parrot program. -}
desugarProgram :: ProgramSugar -> ProgramCore
desugarProgram prog = prog{programDecls = desugarDecls $ programDecls prog}

