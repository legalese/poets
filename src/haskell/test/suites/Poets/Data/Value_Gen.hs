{-# LANGUAGE
  ScopedTypeVariables,
  TypeOperators,
  FlexibleContexts,
  UndecidableInstances,
  FlexibleInstances,
  TemplateHaskell,
  MultiParamTypeClasses,
  TypeSynonymInstances #-}

module Poets.Data.Value_Gen
    (module Poets.Data.Type_Gen,
     forAllTerm)  where
import Poets.Data.Type_Gen
import Data.Comp
import Poets.Data.Value
import Poets.Data.Value.Utils
import Test.QuickCheck
import Control.Monad
import Control.Applicative
import Poets.Data.Type
import Poets.Data.Type.Utils
import Data.Maybe

instance Arbitrary Time where
  arbitrary = do 
    (h,m,s,ms) <- arbitrary
    return $ fromJust $ createTime (abs h `mod` 24) (abs m `mod` 60) (abs s `mod` 60) (abs ms `mod` 1000000)
    
instance Arbitrary Date where
  arbitrary = do
    (y,m,d) <- arbitrary
    return $ fromJust $ createDate (abs y) ((abs m `mod` 12) + 1) ((abs d `mod` 28)+1)
    
instance Arbitrary DateTime where
  arbitrary = do
    (date,time) <- arbitrary
    return $ createDateTime date time
                                

{-|
  Explicit universal quantification over values of a given type in a given environment.
-}
forAllTerm :: (Testable prop, TypedArbitrary v t, Show v) => RecordEnv (Term t) -> Term t -> (v -> prop) -> Property
forAllTerm env ty prop = do expr <- arbitraryT env ty
                            whenFail (putStrLn ("expression: "++show expr)) $ property $ prop expr

                  
{-|
  This instance allows to generate types which at least contain the type
  constants.
-}
instance (TypeConstant :<: t) => GenType TypeConstant t where
    genInject = inject
    genType' env = [(8,oneof $ [tint,tbool,tstring,ttime,tdate,tdatetime,tdouble] ++ mtrecord)]
        where tint = return $ TInt
              tbool = return $  TBool
              tstring = return $  TString
              ttime = return TTime
              tdate = return TDate
              tdatetime = return TDateTime
              tdouble = return $ TReal
              mtrecord
                  | isEmpty env = []
                  | otherwise = [TRecord <$> elements (getRecordNames' env)]

{-|
  This instance allows to generate types which at least contain list types.
-}
instance (TypeList :<: t) => GenType TypeList t where
    genInject = inject
    genType' env = [(1,oneof $ [tlist])]
        where tlist =  (TList . Term) <$> frequency (genType' env)

{-|
  This instance allows to generate types which at least contain reference types.
-}
instance (TypeEnt :<: t) => GenType TypeEnt t where
    genInject = inject
    genType' env = [(1,oneof $ [tref])]
        where tref =  (TEnt . Term) <$> frequency (genType' env)

{-|
  This instance allows to generate typed value expression.
-}
instance (TypeConstant :<: t, TypeList :<: t, TypeEnt :<: t) => TypedArbitraryF Val t where
    arbitraryTF' env ty = case project ty of
                           Just ty -> 
                               let gen = case ty of
                                           TInt -> VInt <$> arbitrary
                                           TBool -> VBool <$> arbitrary
                                           TString -> VString <$> arbitrary
                                           TDuration -> VDuration <$> arbitrary
                                           TTime -> VTime <$> arbitrary
                                           TDate -> VDate <$> arbitrary
                                           TDateTime -> VDateTime <$> arbitrary
                                           TReal -> VReal <$> arbitrary
                                           TRecord name -> VRecord <$> rarbitraryT env (force $ getRecordInfo env name)
                               in [(9,gen)]
                           Nothing ->
                               case project ty of
                                 Just (TList t1) -> 
                                     [(2,VList <$> larbitraryT env t1)]
                                 Nothing -> case project ty of
                                   Just (TEnt _) -> [(2,VEnt <$> arbitrary)]
                                   Nothing -> []


{-|
  Auxiliary function to generate record fields.
-}
farbitraryT :: TypedArbitrary v t => RecordEnv (Term t) -> Field (Term t) -> Gen (VField v)
farbitraryT env Field{fieldName = name, fieldType = ty} = do val <- arbitraryT env ty
                                                             return VF{vfieldName = name, vfieldValue = val}

{-|
  Auxiliary function to generate records.
-}
rarbitraryT :: TypedArbitrary v t => RecordEnv (Term t) -> Record (Term t) -> Gen (VRecord v)
rarbitraryT env record
    = do let fields = force $ getTypeFields env (recordName record)
         vfields <- mapM (farbitraryT env) (fieldEnvList fields)
         return VR {vrecordName = recordName record, vrecordFields = newFields vfields}

{-|
  Auxiliary function to generate lists.
-}
larbitraryT :: TypedArbitrary v t => RecordEnv (Term t) -> Term t -> Gen [v]
larbitraryT env ty = sized $ \n ->
                      do k <- choose (0,n)
                         replicateM k (arbitraryT env ty)



{-|
  This instance allows to generate arbitrary field values.
-}
instance ArbitraryF VField where
    arbitraryF = VF <$> arbitrary <*> arbitrary
    shrinkF (VF name val) = tail [ VF n v |
                                   n <- name : shrink name,
                                   v <- val : shrink val]

{-|
  This explicitly lifts the 'ArbitraryF' instance of 'VField' to
  an 'Arbitrary' instance.
-}
instance Arbitrary e => Arbitrary (VField e) where
    arbitrary = arbitraryF
    shrink = shrinkF



instance ArbitraryF VDuration where
    arbitraryF = VD <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    shrinkF (VD v1 v2 v3 v4 v5 v6 v7) 
        = tail [ VD x1 x2 x3 x4 x5 x6 x7 |
                 x1 <- v1 : shrink v1,
                 x2 <- v2 : shrink v2,
                 x3 <- v3 : shrink v3,
                 x4 <- v4 : shrink v4,
                 x5 <- v5 : shrink v5,
                 x6 <- v6 : shrink v6,
                 x7 <- v7 : shrink v7]

instance Arbitrary e => Arbitrary (VDuration e) where
    arbitrary = arbitraryF
    shrink = shrinkF

instance ArbitraryF VFields where
    arbitraryF = newFields <$> arbitrary
    shrinkF v  = map newFields $ shrink (fieldsList v)

instance Arbitrary e => Arbitrary (VFields e) where
    arbitrary = arbitraryF
    shrink = shrinkF


{-|
  This instance allows to generate arbitrary record values.
-}
instance ArbitraryF VRecord where
    arbitraryF = VR <$> arbitrary <*> arbitrary
    shrinkF (VR name fields) = tail [VR n f |
                                     n <- name : shrink name,
                                     f <- fields : shrink fields]


{-|
  This explicitly lifts the 'ArbitraryF' instance of 'VRecord' to
  an 'Arbitrary' instance.
-}
instance Arbitrary e => Arbitrary (VRecord e) where
    arbitrary = arbitraryF
    shrink = shrinkF

instance Arbitrary VEntity where
    arbitrary = VEntity <$> arbitrary <*> arbitrary <*> arbitrary
    shrink (VEntity rName refId mctx) = tail [VEntity r i c |
                                                 r <- rName : shrink rName,
                                                 i <- refId : shrink refId,
                                                 c <- mctx : shrink mctx]

{-|
  This instance allows to generate arbitrary value expressions.
-}
instance ArbitraryF Val where
    arbitraryF' = [(10,oneof [vint,vbool,vstring,vdate,vtime,vdatetime,vdouble,vrecord,vref,vlist])]
        where vint = VInt <$> arbitrary 
              vbool = VBool <$> arbitrary
              vstring = VString <$> arbitrary
              vdate = VDate <$> arbitrary
              vtime = VTime <$> arbitrary
              vdatetime = VDateTime <$> arbitrary
              vdouble = VReal <$> arbitrary
              vrecord = VRecord <$> arbitrary
              vref = VEnt <$> arbitrary
              vlist = VList <$> arbitrary
    shrinkF (VInt int) = map VInt $ shrink int
    shrinkF (VBool bool) = map VBool $ shrink bool
    shrinkF (VString string) = map VString $ shrink string
    shrinkF (VReal double) = map VReal $ shrink double
    shrinkF (VDate _) = []
    shrinkF (VTime _) = []
    shrinkF (VDateTime _) = []
    shrinkF (VDuration d) = map VDuration $ shrink d
    shrinkF (VRecord record) = map VRecord $ shrink record
    shrinkF (VEnt ref) = map VEnt $ shrink ref
    shrinkF (VList list) = map VList $ shrink list

{-|
  This explicitly lifts the 'ArbitraryF' instance of 'Value' to
  an 'Arbitrary' instance.
-}
instance Arbitrary e => Arbitrary (Val e) where
    arbitrary = arbitraryF
    shrink = shrinkF

