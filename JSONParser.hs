module JSONParser   (parseJson, parseFile, foldrJson, parseFileM, parseJsonM, 
                    JSON(..)) where

import Control.Monad
import GHC.Base hiding ((<|>))
import System.IO
import Parser (Parser, runP, pFail, (<|>), item, pSat, pSym, pList, pListP)

-- | parsear una lista de dígitos

-- parsear un dígito y retornar el entero correspondiente
digit :: Parser Int
digit = do c <- pSat isDigit
           return (ord c - ord '0')

isDigit c = (c >= '0') && (c <= '9')

-- parsear una lista de dígitos no vacía
digits :: Parser [Int]
digits = pListP digit

-- sumar la lista de dígitos

sumDigits :: Parser Int
sumDigits = do ds <- digits
               return (sum ds)

-- suma de dígitos es par?
isEvenSumDs :: Parser Bool
isEvenSumDs = do n <- sumDigits
                 return (even n)
-- even n = n `mod` 2 == 0                

-- | reconocer un literal entero (reconoce una lista de dígitos 
-- | y retorna el entero que denota)

-- hacer un parser que tome un texto y lo verifique en el parser
wordP :: String -> Parser String
wordP ""     = do return []
wordP (c:cs) = do 
                c' <- pSym c
                cs' <- wordP cs
                return(c':cs')

number :: Parser Int
number = do d <- digit
            number' d

number' :: Int -> Parser Int
number' n = do d <- digit
               number' (n*10 + d)
            <|>
            return n

isAlpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')

data JSON = JSONList [JSON] 
          | JSONObject [(String, JSON)] 
          | JSONString String
          | JSONNumber Int
          | JSONBool Bool
          | JSONNull
          deriving (Show)

-- isOpenBracket c = c == '[' || c == '{'
-- isCloseBracket '{' c = c == '}'
-- isCloseBracket '[' c = c == ']'

skiP :: Parser Char
skiP = do pSat $ const True

skipSpaces = pList $ pSat isWhiteSpace
            where isWhiteSpace c = c == ' ' || c == ' ' || c == '\t' || c == '\n'

stringP :: Parser String
stringP = do
            pSym '"'
            s <- goToCharP '"'
            return $ s

goToCharP :: Char -> Parser String
goToCharP c = pSatWhile (/=c)

pSatWhile :: (Char -> Bool) -> Parser String
pSatWhile p = do 
                s <- pList (pSat p)
                skiP
                return s

stringJsonP :: Parser JSON
stringJsonP = do
              s <- stringP
              return $ JSONString s

objectPropsP :: Parser [(String, JSON)]
objectPropsP = pList propP

propP :: Parser (String, JSON)
propP = wrapSkipSpaces $ do
        propName <- stringP
        goToCharP ':'
        propVal <- json_parser
        pSatWhile $ not . isNextOrEnd
        return (propName, propVal)
        where isNextOrEnd c = c==',' || c=='}'

objectJsonP :: Parser JSON
objectJsonP = do
              openObjectP
              fmap JSONObject objectPropsP

openObjectP = pSym '{'
closeOjbectP = pSym '}'

boolP :: Parser Bool
boolP =   readP "true"
     <|>  readP "false"
     where  readP = fmap readBool . wordP
            readBool "true" = True
            readBool "false" = False

boolJsonP :: Parser JSON
boolJsonP = fmap JSONBool boolP

numberJsonP :: Parser JSON
numberJsonP = fmap JSONNumber number

nullJsonP :: Parser JSON
nullJsonP = fmap (const JSONNull) $ wordP "null"

listElementP :: Parser JSON
listElementP = wrapSkipSpaces $ do
                json <- json_parser
                pSatWhile $ not . isNextOrEnd
                return json
              where isNextOrEnd c = c==',' || c==']'

listJsonP :: Parser JSON
listJsonP = do
              pSym '['
              json <- fmap JSONList $ pList listElementP
              return json

wrapSkipSpaces :: Parser a -> Parser a
wrapSkipSpaces p = do
                  skipSpaces
                  a <- p
                  skipSpaces
                  return a

json_parser :: Parser JSON
json_parser = wrapSkipSpaces
              $     objectJsonP
             <|>    stringJsonP
             <|>    boolJsonP
             <|>    numberJsonP
             <|>    listJsonP
             <|>    nullJsonP

stringify :: JSON -> String
stringify (JSONBool b) = show b
stringify (JSONNumber n) = show n
stringify (JSONString s) = s
stringify (JSONObject ps) = "{" ++ (show $ map (\(k,v) -> show k ++ " : " ++ stringify v) ps) ++ "}"
stringify (JSONList xs) = show $ map (stringify) xs

foldrJson :: (String -> a) -> (Int -> a) -> (Bool -> a) -> ([(String, a)] -> a) -> ([a] -> a) -> JSON -> a
foldrJson fs fi fb fo fl (JSONBool b) = fb b
foldrJson fs fi fb fo fl (JSONNumber n) = fi n
foldrJson fs fi fb fo fl (JSONString s) = fs s
foldrJson fs fi fb fo fl (JSONObject ps) = fo $ map (\(k,v) -> (k, foldrJson fs fi fb fo fl v)) ps
foldrJson fs fi fb fo fl (JSONList xs) = fl $ map (foldrJson fs fi fb fo fl) xs

getFromParser :: Parser a -> String -> a
getFromParser p s = case runP p s of
                      [(a,[])] -> a
                      [(_,r)] -> error $ "No se parseo toda la entrada: " ++ r
                      [] -> error "No se pudo parsear nada"

parseJson :: String -> IO(JSON)
parseJson = return . (getFromParser json_parser)

parseJsonM :: String -> IO(Maybe JSON)
parseJsonM s = return $ case runP json_parser s of
                          [(a,[])] -> Just a
                          [(_,r)] -> Nothing
                          [] -> Nothing

parseFile :: String -> IO(JSON)
parseFile fileName = do
  handle <- openFile fileName ReadMode
  contents <- hGetContents(handle)
  json <- parseJson contents
  return json

parseFileM :: String -> IO(Maybe JSON)
parseFileM fileName = do
  handle <- openFile fileName ReadMode
  contents <- hGetContents(handle)
  json <- parseJsonM contents
  return json