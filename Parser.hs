module Parser (Parser, runP, pFail, (<|>), item, pSat, pSym, pList, pListP) where

import Control.Monad
import GHC.Base hiding ((<|>))
import System.IO

---------------------------------------------------

-- | Parser type 

newtype Parser a = P {runP :: String -> [(a,String)]}

instance Functor Parser where
  fmap f p = P $ \cs -> [(f a,cs') | (a,cs') <- runP p cs]

instance Applicative Parser where
  pure a =  P (\cs -> [(a,cs)])
  -- (<*>) ::  Parser (a -> b) -> Parser a -> Parser b
  (P p) <*> (P q) = P $ \cs -> [ (f a, cs'')  |  (f , cs')   <- p cs
                                              ,  (a , cs'')  <- q cs']

instance Monad Parser where
  return = pure
  (P p) >>= f = P $ \cs -> concat [runP (f a) cs' | (a,cs') <- p cs]


instance MonadFail Parser where
  fail _ = P $ \cs -> []
-- | Parsers primitivos

pFail :: Parser a
pFail = P $ \cs -> []

(<|>) :: Parser a -> Parser a -> Parser a
(P p) <|> (P q) = P $ \cs -> case p cs ++ q cs of
                              []     -> []
                              (x:xs) -> [x]

item :: Parser Char
item = P $ \cs -> case cs of
                    ""     -> []
                    (c:cs) -> [(c,cs)]

pSat :: (Char -> Bool) -> Parser Char
pSat p = do c <- item
            if p c then return c
                   else pFail



pSym :: Char -> Parser Char
pSym c = pSat (== c)

-- | p* (many) cero o más veces p

pList :: Parser a -> Parser [a]
pList p = do a <- p
             as <- pList p
             return (a:as)
          <|>
          return []

-- | p+ (some) una o más veces p

pListP :: Parser a -> Parser [a]
pListP p = do a <- p
              as <- pList p
              return (a:as)