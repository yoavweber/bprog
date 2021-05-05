import qualified Data.Map.Strict as M
import           Stack.Stack     (executeCode)
import           Test.Hspec
import           Types

tests :: SpecWith ()
tests =
  describe "official tests for non-error programs" $ do
    {-- literals -}
    t "3"                           "3"
    t "121231324135634563456363567" "121231324135634563456363567"
    t "1.0"                         "1.0"
    t "0.0"                         "0.0"
    t "-1"                          "-1"
    t "-1.1"                        "-1.1"
    t "False"                       "False"
    t "True"                        "True"
    t "[ [ ] [ ] ]"                 "[[],[]]"
    t "[ False [ ] True [ 1 2 ] ]"  "[False,[],True,[1,2]]"
    t "\" [ so { not if ] and } \"" "\"[ so { not if ] and }\""
    {-- quotation literals -}
    t "{ 20 10 + }"             "{ 20 10 + }"
    t "[ { + } { 10 + } { 20 10 + } ]"   "[{ + },{ 10 + },{ 20 10 + }]"

    {-- simple arithmetic -}
    t "1 1 +"               "2"
    t "10 20 *"             "200"
    t "20 2 div"            "10"
    t "20 2 /"              "10.0"

    {-- arithmetic with type coercion -}
    t "1 1.0 +"             "2.0"
    t "10 20.0 *"           "200.0"
    t "20 2.0 div"          "10"
    t "20.0 2.0 div"        "10"

    {-- bool operations -}
    t "False False &&"      "False"
    t "False True ||"       "True"
    t "False not"           "True"
    t "True not"            "False"

    {-- comparisons -}
    t "20 10 <"             "False"
    t "20 10 >"             "True"
    t "20 10.0 >"           "True"
    t "20.0 20.0 >"         "False"
    t "10 10 =="            "True"
    t "10 10.0 =="          "True"
    t "True True =="        "True"
    t "True 40 40 == =="    "True"
    t "\" abba \" \" abba \" ==" "True"
    t "[ ] [ ] =="          "True"
    t "[ 1 2 ] [ 1 2 ] =="  "True"
    t " [ [ ] ] [ [ ] ] ==" "True"

    {-- stack operations -}
    t "10 20 swap pop"          "20"
    t "10 dup dup + swap pop"   "20"
    t "10 20 swap dup + div"    "1"

    {-- length -}
    t "\" hello \" length"              "5"
    t "\" hello world \" length"        "11"
    t "[ 1 2 3 [ ] ] length"            "4"
    t "{ 10 20 + } length"              "3"

    {-- String parsing -}
    t "\" 12 \" parseInteger"           "12"
    t "\" 12.34 \" parseFloat"          "12.34"
    t "\" adam bob charlie \" words"    "[\"adam\",\"bob\",\"charlie\"]"

    {-- lists -}
    t "[ 1 2 3 ]"           "[1,2,3]"
    -- this test might not make sense
    t "[ 1 \" bob \" ]"     "[1,\"bob\"]"
    t "[ 1 2 ] empty"       "False"
    t "[ ] empty"           "True"
    t "[ 1 2 3 ] head"      "1"
    t "[ 1 2 3 ] length"    "3"
    t "[ 1 2 3 ] tail"      "[2,3]"
    t "1 [ ] cons"          "[1]"
    t "1 [ 2 3 ] cons"      "[1,2,3]"
    t "[ 1 ] [ 2 3 ] append" "[1,2,3]"
    t "[ 1 2 ] [ ] append"  "[1,2]"
    t "[ 1 ] [ 2 3 ] cons"  "[[1],2,3]"

    {-- list quotations -}
    t "[ 1 2 3 ] { 10 * } map"                              "[10,20,30]"
    t "[ 1 2 3 ] { 1 + } map"                               "[2,3,4]"
    t "[ 1 2 3 4 ] { dup 2 > { 10 * } { 2 * } if } map"     "[2,4,30,40]"
    t "[ 1 2 3 4 ] { 10 * } each + + +"                     "100"
    -- TODO: test this as an error
    -- t "[ 1 2 3 4 ] { 10 * } "each" + + +"                     "100"

      -- TODO: test this as an error 
    -- t "[ 1 2 3 4 ] each { 10 * } + + +"                     "100"
    t "[ 1 2 3 4 ] 0  { + } foldl"                           "10"
    t "[ 2 5 ] 20 { div } foldl"                            "2"
    {-- note no { } needed for 1 instruction code -}
    t "[ \" 1 \" \" 2 \" \" 3 \" ] { parseInteger } each [ ] cons cons cons" "[1,2,3]"
    t "[ \" 1 \" \" 2 \" \" 3 \" ]  parseInteger each [ ] 3 cons times"       "[1,2,3]"
    t "[ 1 2 3 4 ] 0 + foldl "                               "10"
    t "[ 2 5 ] 20 div foldl"                                "2"

    {-- assignments -}
    t "age"                             "age"
    t "age 10 := age"                   "10"
    t "10 age swap := age"              "10"
    t "[ 1 2 3 ] list swap := list"     "[1,2,3]"
    t "age 20 := [ 10 age ]"            "[10,20]"

    t "inc { 1 + } fun 1 inc"           "2"
    t "mul10 { 10 * } fun inc { 1 + } fun 10 inc mul10" "110"

    {-- quotations -}
    t "{ 20 10 + } exec"                "30"
    t "10 { 20 + } exec"                "30"
    t "10 20 { + } exec"                "30"
    t "{ { 10 20 + } exec } exec"       "30"
    t "{ { 10 20 + } exec 20 + } exec"  "50"

    {-- if -}
    t "True { 20 } { } if"               "20"
    t "True { 20 10 + } { 3 } if"        "30"
    t "10 5 5 == { 10 + } { 100 + } if"  "20"
    t "False { } { 45 } if"              "45"
    t "True  { False { 50 } { 100 } if } { 30 } if" "100"

    {-- if without quotation, more ergonomic expressions -}
    t "True 20 { } if"                 "20"
    t "True { 20 10 + } 3 if"          "30"
    t "10 10 5 5 == + { 100 + } if"    "20"
    t "False { } 45 if"                "45"
    t "True { False 50 100 if } 30 if" "100"

    {-- times -}
    t "1 { 100 50 + } times"                               "150"
    t "5 { 1 } times [ ] 5 { cons } times 0  { + } foldl"   "5"
    t "5  1 times [ ] 5 cons times  0 + foldl "            "5"
    t "5 { 10 } times + + + +"                             "50"
    t "5 10 times 4 + times"                               "50"

    -- {-- loop -}
    t "1 { dup 4 > } { dup 1 + } loop [ ] 5 { cons } times"         "[1,2,3,4,5]"
    t "1 { dup 4 > } { dup 1 + } loop [ ] 5 cons times"         "[1,2,3,4,5]"
    t "[ 1 ] { dup length 9 > }  { dup head 1 + swap cons } loop"   "[10,9,8,7,6,5,4,3,2,1]"


    t "odd { dup 2 div swap 2 / == False True if } fun \
     \ 2 odd"                                                        "False"

    t "odd { dup 2 div swap 2 / == False True if } fun \
     \ 3 odd"                                                        "True"

    t "toList { [ ] swap cons times } fun \
     \ 1 2 3 4 \
     \ 4 toList"                                                      "[1,2,3,4]"

    t "gen1toNum { max swap := 1  { dup max > } { dup 1 + } loop } fun \
     \ 3 gen1toNum + + +"                                            "10"

testErrors :: SpecWith ()
testErrors =
  describe "Testing Errors" $ do
    t "5 True + "                    "Error: can only preform arithmic operation on float or int"
    t "3.2 True -"                   "Error: can only preform arithmic operation on float or int"
    t "3.2 \" hey \" *"                "Error: can only preform arithmic operation on float or int"
    t "False 5 &&"               "Error : can only perform logical operation on bool"
    t "True \" 12 \" ||"               "Error : can only perform logical operation on bool"
    t "5 not"            "Error: Can't preform not on not bool type"
    t "10 \" 20 \" *"             "Error: can only preform arithmic operation on float or int"
    t "[ 1 2 "                       "Prasing error: \"bprog parse\" (line 1, column 7):\nunexpected end of input\nexpecting \"]\""
    t "{ 1 2 "                        "Prasing error: \"bprog parse\" (line 1, column 7):\nunexpected end of input\nexpecting error parsing curly brackets or \"}\""
    t "age 10 := age 20 :="                 "Error: can't assign varible twice"
    t "4 0 /"                       "Error: can't devide by zero"
    t "2 { 10 * } map"                              "Error: Expected a list"
    t "2 { 10 * } each + + +"                     "Error: can only preform arithmic operation on float or int"



t :: String -> String -> Spec
t i o = it i $ show (head (snd (executeCode i (M.empty :: AssignmentMap, [])))) `shouldBe` o

f [] = []
f ('\n':' ':a) = f ('\n' : a)
f (a:b) = a : f b


main :: IO()
main = do
    hspec $ do
        tests
        testErrors

