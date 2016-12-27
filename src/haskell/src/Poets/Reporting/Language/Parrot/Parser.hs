{-# LANGUAGE TypeOperators, FlexibleInstances, FlexibleContexts #-}

--------------------------------------------------------------------------------
-- |
-- Module      : Poets.Reporting.Language.Parrot.Parser
-- Copyright   : 3gERP, 2010
-- License     : All Rights Reserved
--
-- Maintainer  :  Patrick Bahr
-- Stability   :  unknown
-- Portability :  unknown
--
-- This module defines the parser of the Parrot reporting language.
--
--------------------------------------------------------------------------------

module Poets.Reporting.Language.Parrot.Parser 
    (module Poets.Reporting.Language.Parrot.Syntax,
     parseProgramFile,
     parseProgram,
     parseLibraryFile,
     parseLibrary
    ) where

import Poets.Reporting.Language.Parrot.Syntax
import Poets.Data.Value.Utils
import Control.Monad.Identity hiding (sequence,mapM)
import Text.Parsec.Expr
import qualified Text.Parsec.Token as P
import qualified Text.ParserCombinators.Parsec.IndentParser.Token as IP
import Text.ParserCombinators.Parsec.IndentParser
import Text.Parsec.Char
import Text.Parsec.Language
import Text.Parsec hiding (parse, Error)
import System.IO

import Data.String.Utils

import qualified Data.Map as Map
import Data.List
import Data.Ord
import Prelude hiding (sequence, exp, mapM)


{-|
  The type of the parsers we are building here.
-}

type Parser a = IP.IndentCharParser () a


{-|
  Returns the current source position in the 'Parser' monad.
 -}
getPos :: Parser SrcPos
getPos = liftM Just getPosition



style =haskellStyle {
          P.reservedOpNames = ["<+>", "<->", "++", "+","*","/","->", "<-","=","/=","==","<=","<",">=",">",
                               "." , ":","=>", "#","-", "&&", "||", "\\", "|", "!", "@"],
          P.reservedNames = ["fold", "Inl", "Inr", "True", "False", "if", "then", "else",
                             "let", "in", "type", "reftype", "of", "pick", "not", "events", "error", "_"] -- expressions
                            ++ ["Bool", "Int", "Real", "Char", "Date", "Time", "DateTime", "Duration", "String", "Ord", "rec"] -- types
         }
{-|
  Definition of the general lexer.
-}
lexer = P.makeTokenParser style{P.identStart = lower}
lexerLower = P.makeTokenParser style{P.identStart = lower}
lexerUpper = P.makeTokenParser style{P.identStart = upper}

-------------------------------------
-- alias definitions for the lexer --
------------------------------------

whiteSpace = IP.whiteSpace lexer
parens = IP.parens lexer
reservedOp = IP.reservedOp lexer
reserved = IP.reserved lexer
braces = IP.braces lexer
bracesOrBlock = IP.bracesOrBlock lexer
angles = IP.angles lexer
brackets = IP.brackets lexer
colon = IP.colon lexer
comma = IP.comma lexer
symbol = IP.symbol lexer
lowerIdentifier = IP.identifier lexerLower
upperIdentifier = IP.identifier lexerUpper
commaSep = IP.commaSep lexer
commaSep1 = IP.commaSep1 lexer
commaSep2 p = do r <- p
                 comma
                 rs <- commaSep1 p
                 return (r:rs)
stringLiteral = IP.stringLiteral lexer
charLiteral = IP.charLiteral lexer
natural = IP.natural lexer
semiOrNewLineSep1 = IP.semiOrNewLineSep1 lexer


equals = reservedOp "="

{-| This function parses a Parrot program file returning either its
AST if the parsing was successful or an error otherwise -}

parseProgramFile :: FilePath -> IO (Either ParseError ProgramSugar)
parseProgramFile path = do
  cont <- readFile path
  return $ parseProgram path cont



{-| This function parses a Parrot library file returning either its
AST if the parsing was successful or an error otherwise -}

parseLibraryFile :: FilePath -> IO (Either ParseError LibrarySugar)
parseLibraryFile path = do
  cont <- readFile path
  return $ parseLibrary path cont 


{-| This function parses a Parrot program returning either its
AST if the parsing was successful or an error otherwise -}

parseProgram :: SourceName -> String -> Either ParseError ProgramSugar
parseProgram = parseInput program


{-| This function parses a Parrot program returning either its
AST if the parsing was successful or an error otherwise -}

parseLibrary :: SourceName -> String -> Either ParseError LibrarySugar
parseLibrary = parseInput library


{-|
  Parrot library parser.
-}
library :: Parser LibrarySugar
library = liftM Library $ many decl

{-|
  Parrot program parser.
-}
program :: Parser ProgramSugar
program = do 
  name <- liftM (fmap strip) $ optionMaybe reportName
  whiteSpace
  desc <- optionMaybe reportDesc
  whiteSpace
  tags <- option [] reportTags
  whiteSpace
  decls <- many decl
  return Program {programName = name, programDesc = desc, programTags = tags, programDecls = decls}

reportName :: Parser String
reportName = lineFold $ do
  reserved "name"
  colon
  indentedStr

reportDesc :: Parser String
reportDesc = lineFold $ do
  reserved "description"
  colon
  indentedStr

reportTags :: Parser [String]
reportTags = lineFold $ do
  reserved "tags"
  colon
  tags

tags :: Parser [String]
tags = commaSep1 tag
    where tag = do 
            txt <- many1 $ indentParser $ noneOf ","
            -- strip whitespaces
            return . reverse . dropWhile (`elem` " \n\r\t") . reverse $ txt

indentedStr :: Parser String
indentedStr =  wstr
    where str = many $ indentParser anyChar
          wstr = do
            whiteSpace
            res <- str 
            case res of 
              "" -> return ""
              _ -> liftM (res ++) wstr
  

{-|
  Declaration parser.
-}
decl :: Parser DeclSugar
decl = funDef <|> recDecl <?> "top-level declaration"
    where funDef = do
            pos <- getPos
            tyd <- optionMaybe $ lineFold tyDecl
            (id,args,e) <- lineFold $ funEq tyd
            return $ FunDecl pos id (fmap snd tyd) args e
          tyDecl = do
            id <- try $ do id <- funIdent
                           reservedOp ":"
                           return id
            ty <- typScheme
            return (id, ty)
          funEq tyd = do
            id <- try funIdent
            case tyd of
              Just (id',_)
                  | id /= id' ->
                      unexpected $ "Type declaration of \""++ id' ++ "\" is followed by function definition of \""++id ++ "\"!"
              _ -> return ()
            args <- sepBy vvarIdent whiteSpace
            reservedOp "="
            e <- exp
            return (id,args,e)
          recDecl = do
            pos <- getPos
            reserved "rec"
            supRecs <- option [] $ try $ do
                         recs <- commaSep1 recIdent
                         reservedOp ">"
                         return recs
            rec <- recIdent
            reservedOp "="
            fields <- braces $ commaSep fieldDecl
            return $ RecDecl pos rec supRecs fields
          fieldDecl = do
                     pos <- getPos
                     name <- fieldIdent
                     reservedOp ":"
                     ty <- typ
                     return $ FieldDecl pos name ty


{-|
  Parser for type schemes.
-}
typScheme :: Parser TypeScheme
typScheme = do
  pos <- getPos
  cxt <- option [] $ try context
  ty <- typ
  return $ TypeScheme pos cxt ty
    where context = do cxt <- parens (commaSep1 typConstr)
                       reservedOp "=>"
                       return cxt

{-|
  Parser for type constraints.
-}
typConstr :: Parser (TypeConstrPos PType)
typConstr = try ord <|> try eq <|> try subtype <|> field <?> "type constraint"
    where 
      ord = do
        pos <- getPos
        reserved "Ord"
        t <- typ
        return $ Ord t :&: pos
      eq = do
        pos <- getPos
        reserved "Eq"
        t <- typ
        return $ Eq t :&: pos
      subtype = do 
        t1 <- typ
        pos <- getPos
        reservedOp "<"
        t2 <- typ
        return $ SubType t1 t2 :&: pos
      field = do
        t1 <- typ
        reservedOp "."
        field <- fieldIdent
        pos <- getPos
        reservedOp ":"
        t2 <- typ
        return $ HasField t1 field t2 :&: pos

{-|
  Parser for type expressions.
-}
typ :: Parser PType
typ = buildExpressionParser tyOps tyFactor <?> "type"



{-|
  List of type operators
-}
--tyOps :: [[Op PType]]
tyOps = [[Infix tsum AssocLeft],
         [Infix tfun AssocRight]]
    where tsum = do
            pos <- getPos
            reservedOp "+"
            return (\x y -> inject $ TSum x y :&: pos :: PType)
          tfun = do
            pos <- getPos
            reservedOp "->"
            return (\x y -> inject $ TFun x y :&: pos)


{-|
  Supplementary parser for type expressions
-}
tyFactor :: Parser PType
tyFactor =
    do pos <- getPos
       let retInj x = return $ inj x
           inj x = inject (x :&: pos)
           retInj' x = return $ appCxt $ ann pos x 
           tvar = tvarIdent >>= (retInj . TVar)
           prim str con = try $ reserved str >> retInj con
           list = brackets typ >>= (retInj . TList)
           ref = angles typ >>= (retInj . TEnt)
           prod = parens (commaSep2 typ) >>= (retInj' . gProd)
           unit = parens whiteSpace >> retInj TUnit
           rec = recIdent >>= (retInj . TRecord)
       try (parens typ)
           <|> prim "Bool" TBool
           <|> prim "Int" TInt
           <|> prim "Real" TReal
           <|> prim "Char" TChar
           <|> prim "Time" TTime
           <|> prim "Date" TDate
           <|> prim "DateTime" TDateTime
           <|> prim "Duration" TDuration
           <|> prim "String" (TList (inj TChar))
           <|> rec
           <|> list
           <|> ref
           <|> try prod
           <|> unit
           <|> tvar
           <?> "type expression"
              

{-|
  Expression parser.
-}
exp :: Parser PExp
exp = buildExpressionParser expOps expFactor <?> "expression"

{-|
  Declaration of expression operators
-}

--expOps :: [[Op PExp]]
expOps = [[Postfix recAcc, Postfix typeCheck, Postfix recMod],
          [Postfix deref],
          [Infix jux AssocLeft],
          [binSugar "++" OpAppend],
          [binCore "*" OpTimes, binCore "/" OpDiv],
          [binCore "+" OpPlus, binCore "-" OpMinus],
          [binCore "<+>" OpDurPlus, binCore "<->" OpDurMinus],
          [binCore "/=" OpNeq, binCore "==" OpEq, binCore "<=" OpLe,
           binCore "<" OpLt, binCore ">=" OpGe,
           binCore ">" OpGt],
          [binCore "&&" OpAnd, binCore "||" OpOr],
          [cons]
         ]
    where
      postfix = recAcc <|> typeCheck <|> recMod <|> deref
      chainPostfix f = do
        postm <- optionMaybe postfix
        case postm of
          Nothing -> return f
          Just post -> return (post . f)
      deref = do
        pos <- getPos
        mode <- (reservedOp "!" >> return DerefNow)
                <|> (reservedOp "@" >> return DerefCxt)
        chainPostfix $ \ e -> inject $ Deref e mode :&: pos
      typeCheck = do
        pos <- getPos
        reservedOp ":?"
        rec <- refRec
        chainPostfix $ \ e -> inject $ TypePred e rec :&: pos
      recAcc = do
        pos <- getPos
        reservedOp "."
        fieldM <- optionMaybe fieldIdent
        case fieldM of
          Nothing ->
              do nat <- natural
                 chainPostfix $ \ e ->
                    case project e of
                      Just (VInt n :&: pos') -> inject $ VReal (read $ show n ++ "." ++ show nat) :&: (pos' :: SrcPos) :: PExp
                      _ -> if nat > 0 
                           then inject $ Proj e (ProjComp $ fromInteger nat) :&: pos :: PExp
                           else error "expected field name after \".\""
          Just field -> chainPostfix $ \ e -> inject $ RecAcc e field :&: pos :: PExp
                 
      jux = do
        pos <- getPos
        whiteSpace
        return $ \ e1 e2 -> inject $ App e1 e2 :&: pos :: PExp
      recMod = do
        pos <- getPos
        assigns <- fieldAssigns
        checkDupAssigns assigns
        chainPostfix $ \ e -> inject $ RecMod e assigns :&: pos :: PExp
--      cons :: Op PExp
      cons = Infix 
            (do pos <- getPos
                reservedOp "#"
                return $ \ e1 e2 -> inject $ BinOpCore e1 OpCons e2 :&: pos :: PExp)
            AssocRight
      {-|
        Parsing a binary left associative infix operator.
       -}
--      bin :: String -> BinOp -> Op PExp
      binCore sym op
          = Infix 
            (do pos <- getPos
                reservedOp sym
                return $ \ e1 e2 -> inject $ BinOpCore e1 op e2 :&: pos :: PExp)
            AssocLeft
      binSugar sym op
          = Infix 
            (do pos <- getPos
                reservedOp sym
                return $ \ e1 e2 -> inject $ BinOpSugar e1 op e2 :&: pos :: PExp)
            AssocLeft

{-|
  Parser for components of date expressions, i.e. either numerals or variables.
-}
dateComp :: Parser PExp
dateComp = do 
  pos <- getPos 
  orVar "numeral" (liftM (inject . (:&: pos) . VInt . fromInteger) natural)
  
orVar :: String -> Parser PExp -> Parser PExp
orVar msg other = do 
  pos <- getPos 
  let var = liftM (inject . (:&: pos) . VVar) vvarIdent
  other <|> var <?> (msg ++ " or variable")

duration :: Parser (DurationExp PExp)
duration = do 
  comps <- durComp `sepBy` comma
  let comps' = sortBy ordComp comps
  checkDups comps'
  let tbl = Map.fromAscList comps'
      comp lab = Map.lookup lab tbl
      res = VD { 
              durationSeconds = comp DurSec,
              durationMinutes = comp DurMin,
              durationHours = comp DurHour,
              durationDays = comp DurDay,
              durationWeeks = comp DurWeek,
              durationMonths = comp DurMon,
              durationYears = comp DurYear }
  return res                  
    where checkDups [] = return ()
          checkDups [_] = return ()
          checkDups ((x,_):xs@((y,_):_))
              | x == y = unexpected $ "duration label '" ++ show y ++ " is used twice"
              | otherwise = checkDups xs
          ordComp :: (DurationFields,a) -> (DurationFields,a) -> Ordering
          ordComp (s1,_) (s2,_) = compare s1 s2
          durComp :: Parser (DurationFields,PExp)
          durComp = do count <- dateComp
                       label <- durName
                       return (label,count)
          reserved' s = reserved s >> return s
          durName :: Parser DurationFields
          durName = do 
             name <- choice (map reserved' durationFieldList) <?> "duration label"
             case Map.lookup name durationFieldMap of
               Nothing -> fail $ "unable to normalise duration label '"
                          ++ name ++ "'"
               Just nName -> return nName

{-|
  Supplementary parser for expressions.
-}
expFactor :: Parser PExp
expFactor = do 
  pos <- getPos
  let retInj x = return $ inject (x :&:  pos)
      retInjOp op = retInj (PrimOp op)
      liftInj f x = do x' <- x
                       retInj $ f x'
      int = liftInj (VInt . fromInteger) natural
      bool = (reserved "True" >> retInj (VBool True))
             <|> (reserved "False" >> retInj (VBool False))
      string = liftInj VString stringLiteral
      charl = liftInj VChar charLiteral
      dateOrDur = do try $ symbol "<<"
                     dt <- try dateTime
                           <|> try date
                           <|> try time
                           <|> dur
                           <?> "date or time expression" 
                     symbol ">>"
                     return dt 
      date :: Parser PExp
      date = do dayComp <- dayP
                retInj $ Date dayComp
      time :: Parser PExp
      time = do timeComp <- timeP
                retInj $ Time timeComp
      dateTime :: Parser PExp
      dateTime = do dayComp <- orVar "date expression" (try date)
                    timeComp <- orVar "time expression" (try time)
                    retInj $ DateTime dayComp timeComp
      dur :: Parser PExp
      dur = liftInj Duration duration
      dayP = do  year <- dateComp
                 char '-'
                 month <- dateComp
                 char '-'
                 day <- dateComp
                 return $ DayExp year month day
      timeP = do whiteSpace
                 hours <- dateComp
                 char ':'
                 mins <- dateComp
                 secs <- optionMaybe (char ':' >> dateComp)
                 return $ TimeExp hours mins secs

      unit = parens whiteSpace >> retInj VUnit
      var = liftInj VVar vvarIdent
      ite = do
        reserved "if"
        ife <- exp
        reserved "then"
        thene <- exp
        reserved "else"
        elsee <- exp
        retInj $ If ife thene elsee
      tyOf = do
              reserved "type"
              mvar <- optionMaybe $ try $ 
                      do var <- vvarIdent
                         reservedOp "="
                         return var
              e <- exp
              let mvar' = case mvar of
                            Just _ -> mvar
                            Nothing -> case project e of
                                         Just (VVar v :&: pos) -> Just v
                                             where _ = pos :: SrcPos
                                         _ -> Nothing
              reserved "of"
              (cases,defCase) <- bracesOrBlock $ liftM2 (,)
                                 (semiOrNewLineSep1 typeCase)
                                 (optionMaybe defTypeCase)
              retInj $ TypeOf mvar' e cases defCase
      pick = do
              reserved "pick"
              rec <- refRec
              retInj $ Pick rec
      tuple = liftInj VTuple $ commaSep2 exp
      rec = liftInj VRecord record
      list = liftInj VList $ brackets $ commaSep exp
      listComprehension = brackets $ do
              expression <- exp
              reservedOp "|"
              filters <- commaSep1 listFilter
              retInj (ListComprehension expression filters)
              where
                  listFilter = try listGenerator <|> try listLet <|> listGuard
                  listGenerator = listAssignment "<-" ListGenerator
                  listLet = listAssignment "=" ListLet
                  listGuard = do
                      expression <- exp
                      return (ListGuard expression)
                  listAssignment operator constructor = do
                      var <- vvarIdent
                      typeFilter <- optionMaybe $ colon >> refRec
                      reservedOp operator
                      expression <- exp
                      return (constructor var typeFilter expression)
      lete = do
              reserved "let"
              binds <- bracesOrBlock $ semiOrNewLineSep1 letBind
              reserved "in"
              body <- exp
              retInj $ Let binds body
      lambda = do 
              reservedOp "\\" 
              args <- sepBy vvarIdent whiteSpace
              reservedOp "->"
              body <- exp
              retInj $ VLam args body
      typePredSection = do 
              try $ reservedOp ":?"
              rec <- refRec
              retInj $ OperatorSection $ TypePredSection rec
      normalSection = gsection binOpCore NormalSection
      gsection parseOperator selectSection = 
                try (do 
                  operand <- expFactor
                  operator <- parseOperator
                  retInj $ OperatorSection $ selectSection operator (LeftOperand operand))
                <|> 
                try (do 
                  operator <- parseOperator
                  operand <- expFactor
                  retInj $ OperatorSection $ selectSection operator (RightOperand operand))
                <|> 
                (do 
                  operator <- parseOperator
                  retInj $ OperatorSection $ selectSection operator NoOperand)
      primOp' = primOp >>= retInjOp
      pexp = try tuple
             <|> try exp
             <|> try normalSection 
             <|> typePredSection

  try unit 
    <|> (parens pexp )
    <|> dateOrDur
    <|> lete
    <|> try int
    <|> primOp'
    <|> bool
    <|> string
    <|> charl
    <|> var
    <|> ite
    <|> rec
    <|> try list
    <|> listComprehension
    <|> lambda
    <|> tyOf
    <|> pick
    <?> "simple expression"


primOp :: Parser PrimOp
primOp = choice (map op [Fold, Not, Case, Error, Events, LeftOp, RightOp])
         <?> "primitive operator"
    where op o = reserved (show o) >> return o

binOpCore :: Parser BinOpCore
binOpCore = choice (map op [OpTimes, OpDiv, OpPlus, OpMinus, 
                        OpDurPlus, OpDurMinus, OpEq, OpNeq, OpLe, 
                        OpLt, OpGe, OpGt, OpAnd, OpOr, OpCons])
            <?> "binary operator"
    where op o = reservedOp (show o) >> return o


typeCase :: Parser (TypeCasePos PExp)
typeCase = do 
  pos <- getPos
  rec <- refRec
  reservedOp "->"
  e <- exp
  return $ TypeCase rec e :&: pos


defTypeCase :: Parser (DefTypeCasePos PExp)
defTypeCase = do 
  pos <- getPos
  reserved "_"
  reservedOp "->"
  e <- exp
  return $ DefTypeCase e :&: pos

{-|
  Parser for let bindings.
-}
letBind :: Parser (LetBindPos PExp)
letBind = do 
  pos <- getPos
  var <- vvarIdent
  args <- sepBy vvarIdent whiteSpace
  equals
  e <- exp
  return $ LetBind var args e :&: pos

{-|
  Parser for record construction expressions.
-}
record :: Parser (VRecord PExp)
record = do name <- recIdent
            fields <- fieldAssigns'
            fields' <- either fail return $ newFields' fields
            return $ VR name fields'

{-| Parser for a field assignment used inside of record constructions
or record modifications.  -}

fieldAssign :: Parser (VFieldPos PExp)
fieldAssign = do 
  pos <- getPos
  field <- fieldIdent
  equals
  ex <- exp
  return $ VF field ex :&: pos

{-| Parser for a comma separated list of field assignments enclosed in
curly braces used for record constructions and record modifications. -}

fieldAssigns :: Parser [VFieldPos PExp]
fieldAssigns = braces $ commaSep fieldAssign

checkDupAssigns :: [VFieldPos e] -> Parser ()
checkDupAssigns assigns = chDup assigns'
    where cpName = comparing (liftA vfieldName)
          assigns' = sortBy cpName assigns
          chDup ((VF {vfieldName = n1} :&: _) : xs @((VF {vfieldName = n2} :&: p):_))
                | n1 == n2 = fail $ "field '" ++ n1 ++ "' is assigned a value twice:\n" ++ show p
                | otherwise = chDup xs
          chDup _ = return ()

{-| Parser for a field assignment used inside of record constructions
or record modifications.  -}

fieldAssign' :: Parser (VField PExp)
fieldAssign' = do 
  field <- fieldIdent
  equals
  ex <- exp
  return $ VF field ex

{-| Parser for a comma separated list of field assignments enclosed in
curly braces used for record constructions and record modifications. -}

fieldAssigns' :: Parser [VField PExp]
fieldAssigns' = braces $ commaSep fieldAssign'


{-|
  Parser for (value) variable identifiers.
-}
vvarIdent :: Parser VVarId
vvarIdent = liftM vVarId (lowerIdentifier <?> "variable identifier")

{-|
  Parser for type variable identifiers.
-}
tvarIdent :: Parser TVarId
tvarIdent = liftM tVarId (lowerIdentifier <?> "type variable identifier")


refRec :: Parser RefRec
refRec = IsRef `liftM` angles recIdent 
         <|> IsRec `liftM` recIdent 

{-|
  Parser for record identifiers.
-}
recIdent :: Parser RecordName
recIdent = upperIdentifier <?> "record identifier"


{-|
  Parser for fields.
-}
fieldIdent :: Parser FieldName
fieldIdent = lowerIdentifier <?> "field identifier"

{-|
  Parser for function identifiers.
-}
funIdent :: Parser FunId
funIdent = lowerIdentifier <?> "function identifier"
             

parseInput :: Parser a -> SourceName -> String -> Either ParseError a
parseInput p = parse p'
    where p' = whiteSpace >> p >>= \res -> eof >> return res

