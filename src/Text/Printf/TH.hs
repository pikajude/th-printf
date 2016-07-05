{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}

module Text.Printf.TH (s, st, lt, sb, lb, sP, stP, ltP, sbP, lbP) where

import           Control.Applicative
import           Control.Monad.IO.Class
import           Data.Attoparsec.Text         hiding (space)
import qualified Data.ByteString              as B
import qualified Data.ByteString.Char8        as B8
import qualified Data.ByteString.Lazy         as LB
import qualified Data.ByteString.Lazy.Char8   as LB8
import           Data.Char                    hiding (Space)
import           Data.Data
import           Data.Maybe
import           Data.Monoid
import           Data.String
import           Data.Text                    (pack, unpack)
import qualified Data.Text                    as T
import qualified Data.Text.IO                 as T
import qualified Data.Text.Lazy               as LT
import qualified Data.Text.Lazy.IO            as LT
import           Data.Word
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           Numeric
import           Prelude                      hiding (lex)
import           Text.ParserCombinators.ReadP (readP_to_S)
import           Text.Read.Lex

data Specifier = SignedDec | Octal | UnsignedHex | UnsignedHexUpper
               | FloatS | FloatUpper | Sci | SciUpper | ShorterFloat | ShorterFloatUpper
               | CharS | Str | Percent | Showable deriving (Eq, Show, Data, Typeable)

data Flag = Minus | Plus | Space | Hash | Zero deriving (Eq, Show, Data, Typeable)

data Width = Width Integer | WidthStar deriving (Data, Show, Typeable, Eq)

data Precision = Precision Integer | PrecisionStar deriving (Data, Show, Typeable, Eq)

data Chunk = Chunk
           { flags     :: [Flag]
           , width     :: Maybe Width
           , precision :: Maybe Precision
           , spec      :: Specifier
           } | Plain String
           deriving (Data, Show, Typeable)

quoterOfType :: Name -> Bool -> QuasiQuoter
quoterOfType m b = QuasiQuoter
                 { quoteExp = \s' -> let lexed = readP_to_S lex $ '"' : concatMap escape s' ++ "\""
                                         escape '"' = "\\\""
                                         escape m' = [m']
                                     in case lexed of
                        [(String str,"")] -> case parseOnly formatP (pack str) of
                            Right r -> chunksToFormatter r m b
                            Left m' -> error $ "Error when parsing format string: " ++ show m'
                        _ -> error "Error when parsing format string"
                 , quotePat = error "printf cannot be used in pattern context"
                 , quoteType = error "printf cannot be used in type context"
                 , quoteDec = error "printf cannot be used in declaration context"
                 }

s, st, lt, sb, lb, sP, stP, ltP, sbP, lbP :: QuasiQuoter

s = quoterOfType ''String False
st = quoterOfType ''T.Text False
lt = quoterOfType ''LT.Text False
sb = quoterOfType ''B.ByteString False
lb = quoterOfType ''LB.ByteString False
sP = quoterOfType ''String True
stP = quoterOfType ''T.Text True
ltP = quoterOfType ''LT.Text True
sbP = quoterOfType ''B.ByteString True
lbP = quoterOfType ''LB.ByteString True

formatP :: Parser [Chunk]
formatP = many1 ( char '%' *> chunkP
              <|> fmap (Plain . unpack) (takeWhile1 (/= '%')) )
              <* endOfInput

chunkP :: Parser Chunk
chunkP = do
    f <- many flagP
    w <- option Nothing (Just <$> widthP)
    p <- option Nothing (Just <$> precisionP)
    m <- specP
    return $ Chunk f w p m

flagP :: Parser Flag
flagP = Minus <$ char '-'
    <|> Plus <$ char '+'
    <|> Space <$ char ' '
    <|> Hash <$ char '#'
    <|> Zero <$ char '0'

widthP :: Parser Width
widthP = ( Width <$> decimal
       <|> WidthStar <$ char '*' )

precisionP :: Parser Precision
precisionP = char '.' *> ( Precision <$> decimal
                       <|> PrecisionStar <$ char '*' )

specP :: Parser Specifier
specP = SignedDec <$ (char 'd' <|> char 'i')
    <|> Octal <$ char 'o'
    <|> UnsignedHex <$ char 'x'
    <|> UnsignedHexUpper <$ char 'X'
    <|> FloatS <$ char 'f'
    <|> FloatUpper <$ char 'F'
    <|> Sci <$ char 'e'
    <|> SciUpper <$ char 'E'
    <|> ShorterFloat <$ char 'g'
    <|> ShorterFloatUpper <$ char 'G'
    <|> CharS <$ char 'c'
    <|> Str <$ char 's'
    <|> Percent <$ char '%'
    <|> Showable <$ char '?'

data PrintfArg = PrintfArg
               { paSpec   :: Chunk
               , widthArg :: Maybe Name
               , precArg  :: Maybe Name
               , valArg   :: Maybe Name
               } deriving Show

collectArgs :: PrintfArg -> [PatQ]
collectArgs (PrintfArg _ n1 n2 n3) = map varP $ catMaybes [n1, n2, n3]

chunksToFormatter :: [Chunk] -> Name -> Bool -> ExpQ
chunksToFormatter cs ty pr = do
    ns <- mapM argify cs
    let processor = if pr then [e|output|] else [e|id|]
    lamE (concatMap collectArgs ns) [e|$(processor) (mconcat $(listE $ map arg ns) :: $(conT ty))|]
    where
        argify p@Plain{} = return $ PrintfArg p Nothing Nothing Nothing
        argify c@Chunk{spec = Percent} = return $ PrintfArg c Nothing Nothing Nothing
        argify c@Chunk{width = w, precision = p} = do
            wa <- if w == Just WidthStar
                      then Just <$> newName "a"
                      else return Nothing
            pa <- if p == Just PrecisionStar
                      then Just <$> newName "a"
                      else return Nothing
            q' <- newName "a"
            return $ PrintfArg c wa pa (Just q')

q :: Data a => a -> Q Exp
q = dataToExpQ (const Nothing)

arg :: PrintfArg -> ExpQ
arg PrintfArg{paSpec = Plain str} = stringE str
arg PrintfArg{paSpec = Chunk{spec = Percent}} = stringE "%"
arg c@PrintfArg{valArg = Just v} = (\n -> dispatch n c v) $
    case spec $ paSpec c of
        SignedDec -> 'showIntegral
        Octal -> 'showOctal
        UnsignedHex -> 'showHexP
        UnsignedHexUpper -> 'showUpperHex
        FloatS -> 'showFloatP
        FloatUpper -> 'showUpperFloat
        Sci -> 'showSci
        SciUpper -> 'showUpperSci
        ShorterFloat -> 'showShorter
        ShorterFloatUpper -> 'showUpperShorter
        CharS -> 'showCharP
        Str -> 'showStringP
        Showable -> 'showShowP
        m -> error $ "Unhandled specifier: " ++ show m
arg m = error $ "Unhandled argument: " ++ show m

dispatch :: Name -> PrintfArg -> Name -> ExpQ
dispatch s' n v = appE (varE 'fromString)
    $ foldl1 appE [ varE s'
                  , q $ paSpec n
                  , normalize True (widthArg n)
                  , normalize False (precArg n)
                  , varE v ]
    where
        normalize b v' = case v' of
            Nothing -> litE . integerL $ if b then calcWidth $ paSpec n else calcPrec $ paSpec n
            Just q' -> varE q'

showIntegralBasic :: Chunk -- ^ options
                  -> Integer -- ^ width
                  -> Bool -- ^ less than 0?
                  -> String -- ^ prefix
                  -> String -- ^ value
                  -> String
showIntegralBasic c w b pre n = space c . plus b c . prefix pre c . pad w c $ n

showIntegral :: (Show a, Integral a) => Chunk -> Integer -> Integer -> a -> String
showIntegral pa w _ n = showIntegralBasic pa w (n >= 0) "" $ show n

showOctal :: (Show a, Integral a) => Chunk -> Integer -> Integer -> a -> String
showOctal pa w _ n = showIntegralBasic pa w (n >= 0) "0" $ showOct n ""

showHexP :: (Show a, Integral a) => Chunk -> Integer -> Integer -> a -> String
showHexP pa w _ n = showIntegralBasic pa w (n >= 0) "0x" $ showHex n ""

showUpperHex :: (Show a, Integral a) => Chunk -> Integer -> Integer -> a -> String
showUpperHex pa w _ n = showIntegralBasic pa w (n >= 0) "0X" . map toUpper $ showHex n ""

showFloatP :: RealFloat a => Chunk -> Integer -> Integer -> a -> String
showFloatP pa w pr n = plus (n >= 0) pa
                     . padDelim '.' w pa
                     $ showFFloat (if pr < 0 then Nothing else Just $ fromIntegral pr) n ""

showUpperFloat :: RealFloat a => Chunk -> Integer -> Integer -> a -> String
showUpperFloat pa w pr n = map toUpper $ showFloatP pa w pr n

showSci :: RealFloat a => Chunk -> Integer -> Integer -> a -> String
showSci pa w pr n = plus (n >= 0) pa
                  . padDelim '.' w pa
                  $ showEFloat (if pr < 0 then Nothing else Just $ fromIntegral pr) n ""

showUpperSci :: RealFloat a => Chunk -> Integer -> Integer -> a -> String
showUpperSci pa w pr n = map toUpper $ showSci pa w pr n

showShorter :: RealFloat a => Chunk -> Integer -> Integer -> a -> String
showShorter pa w pr n = if length f > length e then e else f
    where f = showFloatP pa w pr n
          e = showSci pa w pr n

showUpperShorter :: RealFloat a => Chunk -> Integer -> Integer -> a -> String
showUpperShorter pa w pr n = if length f > length e then e else f
    where f = showUpperFloat pa w pr n
          e = showUpperSci pa w pr n

showCharP :: ToChar a => Chunk -> Integer -> Integer -> a -> String
showCharP _ _ _ c = [asChar c]

showStringP :: ToString a => Chunk -> Integer -> Integer -> a -> String
showStringP pa w _ n = space pa . pad w pa $ toString n

showShowP :: Show a => Chunk -> Integer -> Integer -> a -> String
showShowP pa w _ n = space pa . pad w pa $ show n

space :: Chunk -> String -> String
space c = if Space `elem` flags c && Plus `notElem` flags c then (' ':) else id

plus :: Bool -> Chunk -> String -> String
plus b c = if Plus `elem` flags c
               then if b then ('+':) else ('-':)
               else id

prefix :: String -> Chunk -> String -> String
prefix s' p = if Hash `elem` flags p then (s' ++) else id

padDelim :: Integral a => Char -> a -> Chunk -> String -> String
padDelim c w pa s' = a (replicate (fromIntegral w - len) c') s'
    where len = length $ Prelude.takeWhile (/=c) s'
          a = if Minus `elem` flags pa
                  then flip (++)
                  else (++)
          c' = if Zero `elem` flags pa
                  then '0'
                  else ' '

pad :: Integral a => a -> Chunk -> String -> String
pad w pa s' = a (replicate (fromIntegral w - length s') c) s'
    where a = if Minus `elem` flags pa
                  then flip (++)
                  else (++)
          c = if Zero `elem` flags pa
                  then '0'
                  else ' '

calcWidth :: Chunk -> Integer
calcWidth (Chunk _ (Just (Width n)) _ _) = n
calcWidth _ = -1

calcPrec :: Chunk -> Integer
calcPrec (Chunk _ _ (Just (Precision n)) _) = n
calcPrec _ = -1

class ToString a where toString :: a -> String

instance ToChar a => ToString [a] where toString = map asChar
instance ToString T.Text where toString = T.unpack
instance ToString LT.Text where toString = LT.unpack
instance ToString B.ByteString where toString = map asChar . B.unpack
instance ToString LB.ByteString where toString = map asChar . LB.unpack

class ToChar m where asChar :: m -> Char

instance ToChar Char where asChar = id
instance ToChar Int where asChar = chr
instance ToChar Word8 where asChar = chr . fromIntegral

class Printable a where output :: MonadIO m => a -> m ()

instance Printable String where output = liftIO . putStrLn
instance Printable T.Text where output = liftIO . T.putStrLn
instance Printable LT.Text where output = liftIO . LT.putStrLn
instance Printable B.ByteString where output = liftIO . B8.putStrLn
instance Printable LB.ByteString where output = liftIO . LB8.putStrLn
