
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE RebindableSyntax #-}

{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Prelude as P
import qualified Control.Applicative as P

import Definitive
import Data.Attoparsec.ByteString (anyWord8)
import Data.Attoparsec.ByteString.Char8 as A hiding (take, manyTill)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.List (nubBy)
import System.Environment
import Text.Parser.Combinators hiding (between)
import Data.ByteString.Base64 (encode)

-- incorporative instances
instance P.Functor f => Functor f where map = P.fmap
instance P.Applicative f => Unit f where pure = P.pure
instance P.Applicative f => Applicative f where (<*>) = (P.<*>)
instance (P.Applicative m, P.Monad m) => Monad m where (>>=) = (P.>>=)
instance P.Alternative f => Semigroup (f a) where (+) = (P.<|>)

main = do
	files <- getArgs
	case files of
		(x:xs) -> gomain x xs
		_ -> help
help = putStrLn "usage: FromVMG <Address-book>.VCF *.VMG"

gomain x xs = do
	contacts <- parseVCF x
	--mtrace . show $ contacts
	rs <- for xs parseVMG
	for_ (nubBy mailEq $ concat rs)
		(B.putStrLn . showMail . addNames contacts)

parseVMG x = do
	writeHString stderr ("Processing " + x + "\n")
	bs <- B.readFile x
	case parseOnly vmg bs of
		Left err -> error err
		Right a -> return a

vmg = many' vmsg
vmsg = do
	line "BEGIN:VMSG"
	line "VERSION:1.1"
	msgHeaders <- many' header
	vc <- vcard
	let smsFrom = lookup "TEL" vc
	body <- venv $ do
		vc2 <- vcard
		let smsTo = lookup "TEL" vc2
		venv $ vbody $ case (smsFrom, smsTo) of
			(Just "", Just "") -> do
				mailHeaders <- many' header <* crlf
				boundary <- A.takeWhile (/= '\r') <* crlf
				text <- manyTill anyWord8 (string (boundary + "--")) <* crlf
				crlf
				return (msgHeaders ++ mailHeaders, boundary, B.pack text)
			(Just sender, Just recep) -> do
				line "CHARSET=SHIFT_JIS"
				line "ENCODING=QUOTED-PRINTABLE"
				date <- header <* crlf
				let mailHeaders = [date, ("From", sender), ("To", recep)]
				text <- many $ notFollowedBy (string "END:VBODY") >> anyWord8
				return (msgHeaders ++ mailHeaders, "", B.pack text)
	line "END:VMSG"
	return body

crlf = string "\r\n"
line str = string str <* crlf

delim = string "BEGIN:" + string "END:" + string "\r"
header = notFollowedBy delim >> (,)
	 <$> A.takeWhile (/= ':') <* char ':' <* skipOptional (char ' ')
	 <*> A.takeWhile (/= '\r') <* crlf
vcard = line "BEGIN:VCARD" *> many' header <* line "END:VCARD"
venv = between (line "BEGIN:VENV") (line "END:VENV")
vbody = between (line "BEGIN:VBODY") (line "END:VBODY")

mailEq (headers, b, body) (headers', b', body') =
	lookup "Date" headers == lookup "Date" headers
	&& lookup "From" headers == lookup "From" headers
	&& b == b'
	&& body == body'

showHeaders = foldMap (\(k, v) -> k + ": " + v + "\r\n")

showMail :: Mail -> Chunk
showMail (headers, "", body) =
	"From ???@??? Sat Jan 01 00:00:00 2000\r\n"
	+ showHeaders (headers ++ mmk) + "\r\n"
	+ "--mimemk00\r\n"
	+ "Content-Type: text/plain; charset=Shift_JIS\r\n"
	+ "Content-Transfer-Encoding: quoted-printable\r\n\r\n"
	+ body
	+ "--mimemk00--\r\n\r\n"
	where mmk = [("MIME-Version", "1.0"), ("Content-Type", "multipart/mixed;boundary=\"mimemk00\"")]
showMail (headers, boundary, body) =
	"From ???@??? Sat Jan 01 00:00:00 2000\r\n"
	+ showHeaders headers + "\r\n"
	+ boundary + "\r\n" + body + boundary + "--\r\n\r\n"

type Mail = ([(Chunk, Chunk)], Chunk, Chunk)


parseVCF x = do
	writeHString stderr ("Reading " + x + "\n")
	bs <- B.readFile x
	case parseOnly (many' vcard) bs of
		Left err -> error err
		Right a -> return $ foldMap makeDict a

makeDict :: [(Chunk, Chunk)] -> [(Chunk, Chunk)]
makeDict contact =
	[ (item, name) | (key, item) <- contact, C.take 3 key == "TEL" || C.take 5 key == "EMAIL" ]
	where
		Just n = lookup "N;CHARSET=SHIFT_JIS" contact
		name = B.intercalate "" (C.split ';' n)

addNames contacts (headers, boundary, body) =
	(map comp $ foldMap sup headers, boundary, body)
	where
		isSMS = lookup "X-IRMC-TYPE" headers == Just "SMS"
		isMail = not isSMS
		sup ("From", "") | isSMS = [("From", myPhone)]
		sup ("To", "") | isSMS = [("To", myPhone)]
		sup ("From", "") | isMail = [("From", myMail)]
		sup ("From", x) | isMail = [("From", x), ("To", myMail)]
		sup (k, "") | k `elem` ["To", "Cc", "Bcc"] = []
		sup h = [h]
		myPhone = contacts !! 0 & fst
		myMail = contacts !! 3 & fst
		comp (k, x) | k `elem` ["From", "To", "Cc", "Bcc"] =
			case lookup x contacts of
				Nothing -> (k, x)
				Just name ->
					(k, "=?SHIFT_JIS?B?" + encode name + "?= <" + x + ">")
		comp h = h

