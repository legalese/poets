module Poets.Reporting.Language.Parrot.Evaluate_Test (tests) where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (test)

import Poets.Data.Value hiding (fromSeconds)
import Poets.Data.Value.Duration hiding (fromSeconds)
import Poets.Data.Value.Utils
import Poets.Data.Type
import Data.Comp
import Poets.Reporting.Loader 
import Data.Maybe (fromJust)
import Data.Set (Set)
import qualified Data.Set as Set


---------------------------------------------------
----------------- Test Suits ----------------------
---------------------------------------------------

main = defaultMain [tests]

tests = testGroup "Poets.Reporting.Language.Parrot.Evaluate" 
--        [testGroup "Simplify Bug" $ mkTestCases simplifyBug]
        [testGroup "Values in the core language" $ mkTestCases coreValues
        ,testGroup "Constants in the core language" $ mkTestCases coreConstants
        ,testGroup "Constants in the extended language" $ mkTestCases extensionConstants
        ,testGroup "Operator sections" $ mkTestCases operatorSections
        ,testGroup "Various fixed bugs" $ mkTestCases variousFixedBugs
        -- ,testGroup "Various unfixed bugs" $ mkTestCases variousUnfixedBugs
        -- ,testGroup "Particularities" $ mkTestCases particularities
        ,testGroup "List Equality" $ mkTestCases listEquality
        ,testGroup "List Ordering" $ mkTestCases listOrdering
        ,testGroup "Record Equality" $ mkTestCases recordEquality
        ,testGroup "DateTime Composition" $ mkTestCases dateTimeComp
        ,testGroup "DateTime Selectors" $ mkTestCases dateTimeSelectors
        ,testGroup "Date Selectors" $ mkTestCases dateSelectors
        ,testGroup "Time Selectors" $ mkTestCases timeSelectors
        ,testGroup "Duration Selectors" $ mkTestCases durationSelectors
       ]

adhocTest = testGroup "Adhoc Tests" $ mkTestCases adhoc


---------------------------------------------------
----------------- Unit Tests ----------------------
---------------------------------------------------
    
dateTimeComp = 
    [testGood "Compose date and time variable"
     (mk VDateTime (fromJust $ createDateTime' 1 2 3 4 5 6 0))
     (report [] "DateTime" "    let date = <<1-2-3>>; time = <<4:05:06>>\n    in <<date time>>"),
     testGood "Compose date and seconds variable"
     (mk VDateTime (fromJust $ createDateTime' 1 2 3 4 5 6 0))
     (report [] "DateTime" "    let date = <<1-2-3>>; s = 6\n    in <<date 4:05:s>>"),
     testGood "Compose date variable and time"
     (mk VDateTime (fromJust $ createDateTime' 1 2 3 4 5 6 0))
     (report [] "DateTime" "    let date = <<1-2-3>>\n    in <<date 4:05:06>>"),
     testGood "Compose time variable and date"
     (mk VDateTime (fromJust $ createDateTime' 1 2 3 4 5 6 0))
     (report [] "DateTime" "    let time = <<4:05:06>>\n    in <<1-2-3 time>>"),
     testGood "Compose time and year variable"
     (mk VDateTime (fromJust $ createDateTime' 1 2 3 4 5 6 0))
     (report [] "DateTime" "    let time = <<4:05:06>>; y = 1\n    in <<y-2-3 time>>"),
     testGood "Compose datetime with hour var"
     (mk VDateTime (fromJust $ createDateTime' 1 2 3 4 5 6 0))
     (report [] "DateTime" "    let h = 4\n    in <<1-2-3 h:5:6>>"),
     testGood "Compose datetime with month var"
     (mk VDateTime (fromJust $ createDateTime' 1 2 3 4 5 6 0))
     (report [] "DateTime" "    let m = 2\n    in <<1-m-3 4:5:6>>")]

dateTimeSelectors = 
    [testGood "Select Components (long form)"
     (mkIntList [1,2,3,4,5,6]) 
     (report [] "[Int]" "    let date = <<1-2-3 4:05:06>> \n    in [date.year,date.month,date.day,date.hour,date.minute,date.second]")
    ,testGood "Select Components (short form)"
     (mkIntList [11,12,13,14,15,16]) 
     (report [] "[Int]" "    let date = <<11-12-13 14:15:16>> \n    in [date.y,date.mon,date.d,date.h,date.m,date.s]")
    ,testGood "Select Date" 
     (mk VDate (fromJust $ createDate 11 12 13))
     (report [] "Date" "<<11-12-13 14:15:16>>.date")
    ,testGood "Select Time" 
     (mk VTime (fromJust $ createTime 14 15 16 0))
     (report [] "Time" "<<11-12-13 14:15:16>>.time")
    ,testGood "Modify Components (long form)" 
     (mk VDateTime (fromJust $ createDateTime' 1 2 13 4 15 6 0))
     (report [] "DateTime" "<<11-12-13 14:15:16>>{year=1,month=2,hour=4,second=6}")
    ,testGood "Modify Components (short form)" 
     (mk VDateTime (fromJust $ createDateTime' 1 12 3 4 5 16 0))
     (report [] "DateTime" "<<11-12-13 14:15:16>>{y=1,d=3,h=4,min=5}")]
    
dateSelectors = 
    [testGood "Select Components (long form)"
     (mkIntList [1,2,3]) 
     (report [] "[Int]" "    let date = <<1-2-3>> \n    in [date.year,date.month,date.day]")
    ,testGood "Select Components (short form)"
     (mkIntList [11,12,13]) 
     (report [] "[Int]" "    let date = <<11-12-13>> \n    in [date.y,date.mon,date.d]")
    ,testGood "Modify Components (long form)" 
     (mk VDate (fromJust $ createDate 1 2 13))
     (report [] "Date" "<<11-12-13>>{year=1,month=2}")
    ,testGood "Modify Components (short form)" 
     (mk VDate (fromJust $ createDate 1 12 3))
     (report [] "Date" "<<11-12-13>>{y=1,d=3}")]
    
timeSelectors = 
    [testGood "Select Components (long form)"
     (mkIntList [4,5,6]) 
     (report [] "[Int]" "    let date = <<4:05:06>> \n    in [date.hour,date.minute,date.second]")
    ,testGood "Select Components (short form)"
     (mkIntList [14,15,16]) 
     (report [] "[Int]" "    let date = <<14:15:16>> \n    in [date.h,date.m,date.s]")
    ,testGood "Modify Components (long form)" 
     (mk VTime (fromJust $ createTime 4 15 6 0))
     (report [] "Time" "<<14:15:16>>{hour=4,second=6}")
    ,testGood "Modify Components (short form)" 
     (mk VTime (fromJust $ createTime 4 5 16 0))
     (report [] "Time" "<<14:15:16>>{h=4,min=5}")]

durationSelectors = 
    [testGood "Select Components (long form)"
     (mkIntList [1,2,3,4,5,6,7]) 
     (report [] "[Int]" "    let dur = <<1 y, 2 mon, 3 w, 4 d, 5 h, 6 mins, 7 s>> \n    in [dur.year,dur.month,dur.week,dur.day,dur.hour,dur.minute,dur.second]")
    ,testGood "Select Components (short form)"
     (mkIntList [11,12,13,14,15,16,17]) 
     (report [] "[Int]" "    let dur = <<11 y, 12 mon, 13 w, 14 d, 15 h, 16 mins, 17 s>> \n    in [dur.y,dur.mons,dur.w,dur.d,dur.h,dur.m,dur.sec]")
    ,testGood "Modify Components (long form)" 
     (mk VDuration $ VD {
                 durationSeconds = 7,
                 durationMinutes = 16,
                 durationHours = 5,
                 durationDays = 14,
                 durationWeeks = 3,
                 durationMonths = 2,
                 durationYears = 1
               })
     (report [] "Duration" "<<11 y, 12 mon, 13 w, 14 d, 15 h, 16 mins, 17 s>>{week=3,year=1,month=2,hour=5,second=7}")
    ,testGood "Modify Components (short form)" 
     (mk VDuration $ VD {
                 durationSeconds = 17,
                 durationMinutes = 6,
                 durationHours = 5,
                 durationDays = 4,
                 durationWeeks = 3,
                 durationMonths = 12,
                 durationYears = 1
               })
     (report [] "Duration" "<<11 y, 12 mon, 13 w, 14 d, 15 h, 16 mins, 17 s>>{w=3,y=1,d=4,h=5,min=6}")]
            
coreValues = 
    [testGood "Value Int"
                  (mk VInt 42) 
                  (report [] "Int" "42")
    ,testGood "Value Bool True"
                  (mk VBool True) 
                  (report [] "Bool" "True")
    ,testGood "Value Bool False"
                  (mk VBool False) 
                  (report [] "Bool" "False")
    ,testGood "Value Real"
                  (mk VReal 42.0) 
                  (report [] "Real" "42.0")
    ,testGood "Value String"
                  (mk VString "BaNaNa") 
                  (report [] "String" "\"BaNaNa\"")
    ,testGood "Value DateTime"
                  (mk VDateTime (microSecondsToDateTime 0)) 
                  (report [] "DateTime" "<<1970-01-01 00:00>>")
    ,testGood "Value Date"
                  (mk VDate (fromJust $ createDate 1970 12 13)) 
                  (report [] "Date" "<<1970-12-13>>")
    ,testGood "Value Time"
                  (mk VTime (fromJust $ createTime 12 46 13 0)) 
                  (report [] "Time" "<<12:46:13>>")
    ,let dur = mk VDuration $ VD {
                 durationSeconds = 1,
                 durationMinutes = 2,
                 durationHours = 3,
                 durationDays = 4,
                 durationWeeks = 5,
                 durationMonths = 6,
                 durationYears = 7
               }
     in testGood "Value Duration"
                  dur
                  (report [] "Duration" "<<1 second, 2 minutes, 3 hours, 4 days, 5 weeks, 6 months, 7 years>>")
    ,testGood "Value empty list"
                  (mkIntList []) 
                  (report [] "[Int]" "[]")
    ,testGood "Value Int list"
                  (mkIntList [1,2,3]) 
                  (report [] "[Int]" "[1,2,3]")
    ,testGood "Record from the test ontologies"
                  (mkRecord "Event" [("name", mk VString "John Rambo")])
                  (report [] "Event" "Event{name = \"John Rambo\"}")
    ]

coreConstants = 
    [testGood "Cons with empty list"
                  (mkIntList [1]) 
                  (report [] "[Int]" "1#[]")
    ,testGood "Cons with non-empty list"
                  (mkIntList [1,2,3,4]) 
                  (report [] "[Int]" "1#[2,3,4]")
    ,testGood "Lookup on user defined record"
                  (mk VInt 1337) 
                  (report ["rec Foo = {foo: Int}"] 
                              "Int" "Foo{foo = 1337}.foo")
    ,testGood "Field of field with parenthesis"
                  (mk VInt 42) 
                  (report ["rec A = {a: Int}", "rec B = {b: A}"] 
                       "Int" "    let x = B { b = A { a = 42 } } \n    in (x.b).a")
    ]


simplifyBug = 
    [testGood "List comprehension with 2 generators and a guard - XOR" 
                  (mkIntList [10, 1]) 
                  (report [] "[Int]" "[ p+q*10 | p <- [0, 1], q <- [0, 1], (p==1 || q==1) && not (p==1 && q==1)]")]
                  
extensionConstants = 
    [testGood "List comprehension with 3 generators" 
                  (mkIntList [111, 112, 121, 122, 211, 212, 221, 222]) 
                  (report [] "[Int]" "[a+b+c | a <- [100,200], b <-[10,20], c <- [1,2]]")
    ,testGood "List comprehension with 1 type guarded generator" 
                  (mkIntList [844]) 
                  (report [
                    "rec Foo = {foo:Int}"
                   ,"rec Foo > FooBar = {bar: Int}"
                   ] "[Int]" "[x.bar | x:FooBar <- [Foo{foo=3}, FooBar{foo=700, bar=844}]]")
    ,testGood "List comprehension with 2 generators and a guard - XOR" 
                  (mkIntList [10, 1]) 
                  (report [] "[Int]" "[ p+q*10 | p <- [0, 1], q <- [0, 1], (p==1 || q==1) && not (p==1 && q==1)]")
    ,testGood "List comprehension with \"let\" binding" 
                  (mkIntList [42]) 
                  (report [] "[Int]" "[x | x = (40 + 2)]")
    ,testGood "List comprehension with || in the target expression without parenthesis" 
                  (mk VList (map (mk VBool) [False, True, True, True])) 
                  (report [] "[Bool]" "[ p || q | p <- [False, True], q <- [False, True]]")
    ,testGood "Append two empty lists"
                  (mkIntList []) 
                  (report [] "[Int]" "[]++[]")
    ,testGood "Append, left empty"
                  (mkIntList [1,2]) 
                  (report [] "[Int]" "[]++[1,2]")
    ,testGood "Append, right empty"
                  (mkIntList [1,2]) 
                  (report [] "[Int]" "[1,2]++[]")
    ,testGood "Append, no empty"
                  (mkIntList [1,2,3,4]) 
                  (report [] "[Int]" "[1,2]++[3,4]")
    ,testGood "Pick apples"
                  (mk VList [mkRecord "Apple" [("weight", mk VInt 15), ("ripe" , mk VBool False), ("color", mk VString "yellow")]])
                  (report [] "[Apple]" "pick Apple ["++
                      "Banana {weight = 10, ripe = True, length = 5},"++
                      "Apple {weight = 15, ripe = False, color = \"yellow\"},"++
                      "Fruit {weight = 0, ripe = True}" ++
                      " ]")
    ]

-- Minus sections (-) are used because they may some day clash with unary minus
operatorSections = 
    [testGood "Ordinary binary operators, no arguments given."
                  (mk VInt 8) 
                  (report [] "Int" "(-) 10 2")
    ,testGood "Ordinary binary operators, left argument given."
                  (mk VInt 8) 
                  (report [] "Int" "(10-) 2")
    ,testGood "Ordinary binary operators, right argument given."
                  (mk VInt 8) 
                  (report [] "Int" "(-2) 10")
    ,testGood "Cons data constructor (#), no arguments given."
                  (mkIntList [1, 2, 3]) 
                  (report [] "[Int]" "(#) 1 [2, 3]")
    ,testGood "Cons data constructor (#), left argument given."
                  (mkIntList [1, 2, 3]) 
                  (report [] "[Int]" "(1#) [2, 3]")
    ,testGood "Cons data constructor (#), right argument given."
                  (mkIntList [1, 2, 3]) 
                  (report [] "[Int]" "(#[2, 3]) 1")
    ,testGood "Type predicate (:?) (match)."
                  (mk VBool True) 
                  (report ["rec Foo = {}"] "Bool" "(:? Foo) Foo {}")
    ,testGood "Type predicate (:?) (no match)."
                  (mk VBool False) 
                  (report ["rec Foo = {}", "rec Foo > Bar = {}"] "Bool" "(:? Bar) Foo {}")
    ]
    
listEquality = 
    [testGood "Empty list"
                  (mk VBool True) 
                  (report [] "Bool" "[] == []")
    ,testGood "Int List"
                  (mk VBool True) 
                  (report [] "Bool" "[1] == [1]")
    ,testGood "Int List not equal"
                  (mk VBool False) 
                  (report [] "Bool" "[1] == [2]")
    ]

listOrdering = 
    [testGood "Empty list <"
                  (mk VBool False) 
                  (report [] "Bool" "[] < []")
    ,testGood "Empty list >"
                  (mk VBool False) 
                  (report [] "Bool" "[] > []")
    ,testGood "Empty list >="
                  (mk VBool True) 
                  (report [] "Bool" "[] >= []")
    ,testGood "Empty list <="
                  (mk VBool True) 
                  (report [] "Bool" "[] <= []")
    ,testGood "Int List"
                  (mk VBool True) 
                  (report [] "Bool" "[1] < [2]")
    ,testGood "Int List"
                  (mk VBool False) 
                  (report [] "Bool" "[2] < [1]")
    ,testGood "Int List"
                  (mk VBool True) 
                  (report [] "Bool" "[1,2] < [1,3]")
    ,testGood "Int List"
                  (mk VBool False) 
                  (report [] "Bool" "[1,3] < [1,2]")
    ,testGood "Int List"
                  (mk VBool False) 
                  (report [] "Bool" "[1,3] <= [1,2]")
    ]

recordEquality = 
    [testGood "Record from ontologies"
                  (mk VBool True) 
                  (report [] "Bool" "Event{name = \"John Rambo\"} == Event{name = \"John Rambo\"}")
    ,testGood "Record from ontologies"
                  (mk VBool False) 
                  (report [] "Bool" "Event{name = \"John Rambo\"} == Event{name = \"John Kimble\"}")
    ,testGood "User defined record"
                  (mk VBool True) 
                  (report ["rec Foo = {foo: Int}"] "Bool" "Foo{foo = 42} == Foo{foo = 42}")
    ,testGood "User defined record"
                  (mk VBool False) 
                  (report ["rec Foo = {foo: Int}"] "Bool" "Foo{foo = 42} == Foo{foo = 1337}")
    ]

variousFixedBugs = 
    [testBad "Parse till end of file"
                  (ReportUserError "(line 4, column 1):\nunexpected '@'\nexpecting top-level declaration or end of input")
                  (report []
                              "Int" "42\n@!%$:")
    ,testGood "Do not over-simplify"
                  (mk VInt 1)
                  (report ["triple : ((Int, Real), Real)",
                           "triple = (\\a b -> ((a, b), a + b)) 1 1.0"] "Int" "1")
    ]

variousUnfixedBugs = 
    [-- TODO: This seems wrong. For (:? Foo) we get the type Foo < a => a -> Bool, which seems reasonable,
     --       but when applied to the incompatible Bar we get (Foo < a, Bar < a) => Bool, which may be valid
     --       but not useful since a can only be the empty record type and (:? Foo) (Bar {}) is always False.
     testBad "Type predicate (:?) (not a subtype)."
                  (ReportTypeError "not a subtype") 
                  (report ["rec Foo = {foo : Int}", "rec Bar = {bar : [Bool]}"] "Bool" "(:? Foo) (Bar {bar = []})")
    ]


{- Non of these tests pass by now, but are not reflecting things 
   that are considered as bugs. Thay do however reflect features that 
   are often expected for a 'nornal' functional language. -}
particularities = 
    [testGood "Field of field without parenthesis"
                  (mk VInt 42) 
                  (report ["rec A = {a: Int}", "rec B = {b: A}"] 
                       "Int" "let x = B { b = A { a = 42 } } in x.b.a")
    ,testGood "Single line let"
                  (mk VInt 0) 
                  (report [] "Int" "let x = 0 in x")
    ,testGood "Triple"
                  (mk VInt 0) 
                  (report [] "Int" "let x = (1, 2, 3) in 0")
    ,testBad "Pick bad apples"
                  (ReportTypeError "")
                  (report [""] "[Apple]" "pick Apple ["++
                      "Event {name = \"I am not a fruit\"}" ++
                      " ]")
    ]


adhoc = 
    [testGood "List comprehension with 1 generators" 
                  (mkIntList [100, 200]) 
                  (report [] "[Int]" "[a | a <- [100,200]]")
    ]
    

---------------------------------------------------
-------------- Utitily functions ------------------
---------------------------------------------------

type Program = String

evalAssertEqual :: Either ReportError Value -> Program -> Assertion
evalAssertEqual expectedResult program = do
  actualResult <- evalParrot program
  actualResult @?= expectedResult

mkTestCase (description, expectedResult, program) = 
    testCase description (evalAssertEqual expectedResult program)
mkTestCases = map mkTestCase

testGood d r p = (d, Right r, p)
testBad d r p = (d, Left r, p)




mkRecordType :: String -> [(RecordName, Type)] -> 
                Set (Record Type) -> Bool -> Record Type
mkRecordType name fields super isAbstract = 
    Record {recordName = name,
            recordFields = newFieldEnv (map (\(n, t) -> 
                                             Field{fieldName=n,
                                                   fieldType=t,
                                                   fieldAttributes = Set.empty}) 
                                        fields),
            recordExtends = Set.map recordName super,
            recordAttributes = if isAbstract then Set.singleton RecordIsAbstract
                               else Set.empty
           }


evalParrot :: Program -> IO (Either ReportError Value)
evalParrot code  = do
  let event = mkRecordType "Event" [("name", inject TString)] Set.empty False
  let food  = mkRecordType "Food" [("weight", inject TReal)] Set.empty True
  let fruit = mkRecordType "Fruit" [("ripe", inject TBool)] (Set.singleton food) False
  let apple = mkRecordType "Apple" [("color", inject TString)] (Set.singleton fruit) False
  let banana = mkRecordType "Banana" [("length", inject TInt)] (Set.singleton fruit) False
  let env = newRecordEnv [event, food, fruit, apple, banana]
  res <- loadParrotReport 1 env Nothing code "testReport"
  case res of
    Left err -> do print err ; return $ Left (ReportUserError (show err))
    Right ReportModule {
                   reportFunction = repFun,
                   reportType = repType,
                   reportSubType = subType,
                   reportId = id} -> return $ runRM (repFun []) [] (\_ _ -> Left "Entity store is empty") subType

{-| Limits the boilerplate of writing down reports (mandatory comments and naming).
    Arguments are: definitions, report type, report expression.
    Example:
        evalParrot $ report [] "Int" "4 + 7" 
|-}
report :: [String] -> String -> String -> String
report ds t d = "description: None" ++ concatMap ('\n':) ds ++ "\nreport : " ++ t ++ "\nreport = " ++ d 


{-|
  Smart constructor generator
-}                                    

mk :: (a -> (Val (Value))) -> a -> Value
mk constructor = Term . constructor

{-|
  Smart constructor to create a POETS integer list
-}                                    

mkIntList :: [Int] -> Value
mkIntList ints = mk VList (map (mk VInt) ints)

mkRecord :: String -> [(String, Value)] -> Value
mkRecord name fields = 
    let vFields = newFields $ map (uncurry VF) fields
    in mk VRecord (VR{vrecordName = name, vrecordFields = vFields})

runTest test = defaultMain [testGroup "Tests" $ mkTestCases test]

