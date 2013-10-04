{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE QuasiQuotes #-}

module Text.Printf.TH where

import Control.Applicative
import Data.Attoparsec.Text
import Data.Data
import Data.Maybe
import Data.Text (pack, unpack)
import Language.Haskell.TH
import Language.Haskell.TH.Quote

data Specifier = SignedDec | UnsignedDec | Octal | UnsignedHex | UnsignedHexUpper
               | FloatS | FloatUpper | Sci | SciUpper | ShorterFloat | ShorterFloatUpper
               | HexFloat | HexFloatUpper | CharS | Str | Percent deriving (Eq, Show, Data, Typeable)

data Flag = Minus | Plus | Space | Hash | Zero deriving (Eq, Show, Data, Typeable)

data Width = Width Integer | WidthStar deriving (Data, Show, Typeable, Eq)

data Precision = Precision Integer | PrecisionStar deriving (Data, Show, Typeable, Eq)

data Chunk = Chunk
           { flags :: [Flag]
           , width :: Maybe Width
           , precision :: Maybe Precision
           , spec :: Specifier
           } | Plain String
           deriving (Data, Show, Typeable)

printf :: String -> Q Exp
printf s = do
    let m = parseOnly formatP (pack s)
    case m of
        Right r -> chunksToFormatter r
        Left m' -> error $ show m'

formatP :: Parser [Chunk]
formatP = many1 ( char '%' *> chunkP
              <|> fmap (Plain . unpack) (takeWhile1 (/= '%')))

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
    <|> UnsignedDec <$ char 'u'
    <|> Octal <$ char 'o'
    <|> UnsignedHex <$ char 'x'
    <|> UnsignedHexUpper <$ char 'X'
    <|> FloatS <$ char 'f'
    <|> FloatUpper <$ char 'F'
    <|> Sci <$ char 'e'
    <|> SciUpper <$ char 'E'
    <|> ShorterFloat <$ char 'g'
    <|> ShorterFloatUpper <$ char 'G'
    <|> HexFloat <$ char 'a'
    <|> HexFloatUpper <$ char 'A'
    <|> CharS <$ char 'c'
    <|> Str <$ char 's'
    <|> Percent <$ char '%'

data PrintfArg = PrintfArg
               { paSpec :: Chunk
               , widthArg :: Maybe Name
               , precArg :: Maybe Name
               , valArg :: Maybe Name
               } deriving Show

collectArgs :: PrintfArg -> [PatQ]
collectArgs (PrintfArg _ n1 n2 n3) = map varP $ catMaybes [n1, n2, n3]

chunksToFormatter :: [Chunk] -> ExpQ
chunksToFormatter cs = do
    ns <- mapM argify cs
    lamE (concatMap collectArgs ns) [e|concat $(listE $ map arg ns)|]
    where
        argify p@Plain{..} = return $ PrintfArg p Nothing Nothing Nothing
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
arg PrintfArg{paSpec = Plain s} = stringE s
arg c@(PrintfArg Chunk{..} widthArg precArg (Just v)) = case spec of
    SignedDec -> case length (collectArgs c) of
        3 -> foldl1 appE
                    [ [e|showIntegralWithWidthAndPrecision|]
                    , q $ paSpec c
                    , varE $ fromJust widthArg
                    , varE $ fromJust precArg
                    , varE v ]
        2 -> foldl1 appE
                    [ if isJust widthArg
                          then [e|showIntegralWithWidth|]
                          else [e|showIntegralWithPrecision|]
                    , q $ paSpec c
                    , if isJust widthArg
                          then varE $ fromJust widthArg
                          else varE $ fromJust precArg
                    , varE v ]
        1 -> foldl1 appE [ [e|showIntegral|], q $ paSpec c, varE v ]
        _ -> undefined
arg m = error (show m)

showIntegralWithWidthAndPrecision :: (Show a, Integral a) => Chunk -> Integer -> Integer -> a -> String
showIntegralWithWidthAndPrecision pa w _ n = pad w pa $ show n

pad :: Integral a => a -> Chunk -> String -> String
pad w pa s = a (replicate (fromIntegral w - length s) c) s
    where a = if Minus `elem` flags pa
                  then flip (++)
                  else (++)
          c = if Zero `elem` flags pa
                  then '0'
                  else ' '

showIntegralWithWidth :: (Show a, Integral a) => Chunk -> Integer -> a -> String
showIntegralWithWidth pa w n = showIntegralWithWidthAndPrecision pa w (calcPrec pa) n

showIntegralWithPrecision :: (Show a, Integral a) => Chunk -> Integer -> a -> String
showIntegralWithPrecision pa p n = showIntegralWithWidthAndPrecision pa (calcWidth pa) p n

showIntegral :: (Show a, Integral a) => Chunk -> a -> String
showIntegral pa n = showIntegralWithWidthAndPrecision pa (calcWidth pa) (calcPrec pa) n

calcWidth :: Chunk -> Integer
calcWidth (Chunk _ (Just (Width n)) _ _) = n
calcWidth _ = 0

calcPrec :: Chunk -> Integer
calcPrec (Chunk _ _ (Just (Precision n)) _) = n
calcPrec _ = 0
