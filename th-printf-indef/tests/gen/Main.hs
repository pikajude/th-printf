module Main where

import Data.Char
import Data.List (find, intercalate)
import System.Environment
import Text.Printf

data Language
    = LC
    | LHaskell

data Field
    = FieldString String
    | FieldChar Char
    | FieldSigned Integer
    | FieldUnsigned Integer
    | FieldPointer Integer
    | FieldSigned64 Integer
    | FieldUnsigned64 Integer
    | FieldPointer64 Integer
    | FieldFloating Double
    | FieldAntiTest
    | FieldBadLanguage String

parseFields :: String -> [Field]
parseFields "" = []
parseFields (' ':cs) = parseFields cs
parseFields ('?':cs) = FieldAntiTest : parseFields cs
parseFields ('!':cs) =
    let langChars = "CH"
        (ls, cs') = span (`elem` langChars) cs
     in FieldBadLanguage ls : parseFields cs'
parseFields ('\'':c:'\'':cs) = FieldChar c : parseFields cs
parseFields cs@(c:_)
    | isNumeric c =
        let (s, cs') = span isNumeric cs
         in if isIntegral s
                then let i =
                             case s of
                                 "" -> error "stray minus sign in input"
                                 _ -> read s :: Integer
                      in if i >= 0
                             then case cs' of
                                      'L':'L':cs'' -> FieldSigned64 i : parseFields cs''
                                      'U':'L':'L':cs'' ->
                                          FieldUnsigned64 i : parseFields cs''
                                      'U':cs'' -> FieldUnsigned i : parseFields cs''
                                      'V':'L':'L':cs'' ->
                                          FieldPointer64 i : parseFields cs''
                                      'V':cs'' -> FieldPointer i : parseFields cs''
                                      _ -> FieldSigned i : parseFields cs'
                             else case cs' of
                                      'U':_ ->
                                          error "refused to parse negative unsigned value"
                                      'L':'L':cs'' -> FieldSigned64 i : parseFields cs''
                                      'V':_ ->
                                          error "refused to parse negative pointer value"
                                      _ -> FieldSigned i : parseFields cs'
                else let d =
                             case reads s :: [(Double, String)] of
                                 [(d', "")] -> d'
                                 _ ->
                                     error
                                         "badly formatted floating point number in input"
                      in case cs' of
                             'F':cs'' -> FieldFloating d : parseFields cs''
                             _ -> FieldFloating d : parseFields cs'
  where
    isNumeric c' = isDigit c' || c' `elem` "-+.eE"
    isIntegral ('-':cs') = all isDigit cs'
    isIntegral cs' = all isDigit cs'
parseFields ('"':cs) =
    let (s, cs') = span (/= '"') cs
     in case cs' of
            '"':cs'' -> FieldString s : parseFields cs''
            _ -> error "unclosed string"
parseFields _ = error "stray character in input"

showL :: Language -> Field -> String
showL LC (FieldString s) = show s
showL LC (FieldChar c) = show c
showL LC (FieldSigned i) = show i
showL LC (FieldSigned64 i) = show i ++ "LL"
showL LC (FieldUnsigned u)
    | u >= 0 = show u ++ "U"
    | otherwise = error "refused to show signed unsigned value"
showL LC (FieldUnsigned64 u)
    | u >= 0 = show u ++ "ULL"
    | otherwise = error "refused to show signed unsigned value"
showL LC (FieldPointer u)
    | u >= 0 = "(void *)" ++ show u ++ "U"
    | otherwise = error "refused to show signed pointer value"
showL LC (FieldPointer64 u)
    | u >= 0 = "(void *)" ++ show u ++ "ULL"
    | otherwise = error "refused to show signed pointer value"
showL LC (FieldFloating d) = show d
showL LC FieldAntiTest = "0"
showL LC (FieldBadLanguage _) = error "bad badlanguage"
showL LHaskell (FieldString s) = show s
showL LHaskell (FieldChar c) = show c
showL LHaskell (FieldSigned i) = printf "(%s :: Int32)" $ show i
showL LHaskell (FieldSigned64 i) = printf "(%s :: Int64)" $ show i
showL LHaskell (FieldUnsigned u)
    | u >= 0 = printf "(%s :: Word32)" $ show u
    | otherwise = error "refused to show signed unsigned value"
showL LHaskell (FieldUnsigned64 u)
    | u >= 0 = printf "(%s :: Word64)" $ show u
    | otherwise = error "refused to show signed unsigned value"
showL LHaskell (FieldPointer _) = "undefined"
showL LHaskell (FieldPointer64 _) = "undefined"
showL LHaskell (FieldFloating d) = printf "(%s :: Double)" $ show d
showL LHaskell FieldAntiTest = error "tried to show antitest"
showL LHaskell (FieldBadLanguage _) = error "bad badlanguage"

genCase :: Language -> String -> IO ()
genCase lang testcase =
    case parseFields testcase of
        FieldBadLanguage ls:rest@(FieldSigned serial:_) ->
            if langChar lang `elem` ls
                then case lang of
                         LC -> printf "    /* %d: excluded for C */\n" serial
                         LHaskell -> printf "  -- %d: excluded for Haskell\n" serial
                else processFields rest
        rest -> processFields rest
  where
    langChar LHaskell = 'H'
    langChar LC = 'C'
    processFields :: [Field] -> IO ()
    processFields (FieldSigned serial:FieldAntiTest:_) =
        case lang of
            LC -> printf "    /* %d: anti-test */\n" serial
            LHaskell -> printf "  -- %d: anti-test\n" serial
    processFields (FieldSigned serial:fields) =
        case map (showL lang) fields of
            result:format:args ->
                case lang of
                    LC -> do
                        _ <- printf "    result |= test(%d, %s, %s" serial result format
                        mapM_ (printf ", %s") args
                        printf ");\n"
                    LHaskell -> do
                        _ <-
                            printf
                                "  it \"test-case #%d\" $ [s|%s|]"
                                serial
                                (tail (init format))
                        mapM_ (printf " %s") args
                        printf " @?= %s" result
                        printf "\n"
            _ -> error $ printf "bad parse for: %s" testcase
    processFields _ = error $ printf "bad serial parse for: %s" testcase

makeComment :: Language -> [String] -> String
makeComment LC [] = error "made empty comment"
makeComment LC (s:ss) =
    "/* " ++ s ++ "\n" ++ intercalate "\n" (map ("   " ++) ss) ++ " */\n"
makeComment LHaskell ss = unlines $ map ("-- " ++) ss

main :: IO ()
main = do
    [langStr] <- getArgs
    let lang =
            case langStr of
                "C" -> LC
                "Haskell" -> LHaskell
                _ -> error "unknown testcase language"
    putStr $
        makeComment
            lang
            [ "XXX This code generated automatically by gen-testcases.hs"
            , "from ../../printf-tests.txt . You probably do not want to"
            , "manually edit this file."
            ]
    putStr "spec = do\n"
    contents <- getContents
    mapM_ (genCase lang) $ filter (\s -> nonblank s && noncomment s) $ lines contents
  where
    nonblank s =
        case find (/= ' ') s of
            Nothing -> False
            _ -> True
    noncomment [] = True
    noncomment ('#':_) = False
    noncomment _ = True
