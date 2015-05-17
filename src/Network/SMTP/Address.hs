module Network.SMTP.Address
    ( address
    , EmailAddress
    , localPart
    , domainPart
    , toText
    )
where

import Data.Monoid
import Data.Maybe
import Control.Applicative
import Data.Char (isAscii)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Attoparsec.Text
import Text.Domain.Parser

-- | Represents an email address.
data EmailAddress = EmailAddress { -- | Extracts the local part of an email address.
                                   localPart :: !Text
                                   -- | Extracts the domain part of an email address.
                                 , domainPart :: !(Either Domain Text)
                                 }
    deriving (Eq, Ord)

instance Show EmailAddress where
    show = show . toText

instance Read EmailAddress where
    readsPrec n s = mapMaybe go $ readsPrec n s where
        go (adr, next) = case parseOnly (address <* endOfInput) adr of
            Right r -> Just (r, next)
            Left _ -> Nothing

-- | Converts an email address back to a Text
toText :: EmailAddress -> Text
toText (EmailAddress loc dom) = loc <> "@" <> dom'
    where dom' = case dom of
                   Left r -> r
                   Right l -> "[" <> l <> "]"

-- | A parser for email addresses (RFC 5321). Domain names are restricted
--   to RFC 5890.
address :: Parser EmailAddress
address = do
    l <- local
    _ <- char '@'
    d <- Left <$> domain <|> Right <$> domainLiteral
    return (EmailAddress l d)

local :: Parser Text
local = dottedAtoms <|> quotedString

-- | Matches quoted string according to RFC 5322.
quotedString :: Parser Text
quotedString =
    T.concat <$>
        (\x -> ["\""] ++ x ++ ["\""]) <$>
            (char '"' *> many quotedContent <* char '"')

quotedContent :: Parser Text
quotedContent = takeWhile1 isQuotedText <|> quotedPair

isQuotedText :: Char -> Bool
isQuotedText x = inClass "\33\35-\91\93-\126" x || isNotAscii x

domainLiteral :: Parser Text
domainLiteral = T.concat <$> (char '[' *> many (takeWhile1 isDomainText) <* char ']')

isDomainText :: Char -> Bool
isDomainText = inClass "\33-\90\94-\126"

-- | Matches quoted pair according to RFC 5321.
quotedPair :: Parser Text
quotedPair = (T.cons '\\' . T.singleton) <$> (char '\\' *> satisfy isQuotedPair)

isQuotedPair :: Char -> Bool
isQuotedPair x = inClass "\32-\126" x || isNotAscii x

-- | Matches dotted atom according to RFC 5321.
dottedAtoms :: Parser Text
dottedAtoms = T.intercalate "." <$> atom `sepBy1` char '.'

-- | Matches atom according to RFC 5321.
atom :: Parser Text
atom = takeWhile1 isAtomText

isAtomText :: Char -> Bool
isAtomText x = inClass "a-zA-Z0-9!#$%&'*+/=?^_`{|}~-" x || isNotAscii x

-- | Selects UTF8 non-ASCII characters according to RFC 6532.
isNotAscii :: Char -> Bool
isNotAscii = not . isAscii
