{-# LANGUAGE CPP #-}
module Application (formRedirect, kwsoda, ripple) where

import Prelude ()
import BasicPrelude
import Data.Base58Address (rippleAddressPayload)
import Data.Digest.Pure.MD5 (md5)
import Crypto.Util (i2bs_unsized)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LZ

import Network.Wai (Application, Response, queryString)
import Network.HTTP.Types (ok200, badRequest400, seeOther303)
import Network.Wai.Util (queryLookup, string, redirect', textBuilder, stringHeaders)

import Network.URI (URI(..), URIAuth(..), escapeURIString, isUnescapedInURIComponent, parseAbsoluteURI)

import Records
import MustacheTemplates

Just [htmlCT] = stringHeaders [("Content-Type", "text/html; charset=utf-8")]

htmlEscape :: String -> String
htmlEscape = concatMap escChar
	where
	escChar '&' = "&amp;"
	escChar '"' = "&quot;"
	escChar '<' = "&lt;"
	escChar '>' = "&gt;"
	escChar c   = [c]

-- | Append pair to query string
queryAppend :: URI -> (String,String) -> URI
queryAppend uri@(URI {uriQuery = q}) (k,v)
	| q == "" = uri {uriQuery = '?' : pair}
	| otherwise = uri {uriQuery = q ++ "&" ++ pair}
	where
	pair = esc k ++ "=" ++ esc v
	esc = escapeURIString isUnescapedInURIComponent

-- | Append if the value exists
maybeAppend :: String -> Maybe String -> URI -> URI
maybeAppend k (Just x) = (`queryAppend` (k,x))
maybeAppend _ Nothing = id

-- | Redirect back with an error, or just bail out
err :: (Monad m) => Maybe URI -> String -> m Response
err (Just uri) s = redirect' seeOther303 [] (uri `queryAppend` ("error",s))
err Nothing s = string badRequest400 [] s

formRedirect :: URI -> Application
formRedirect _ req =
		case q "_processor" >>= parseAbsoluteURI of
			Just u -> redirect' seeOther303 [] $
				foldl' (\u k -> maybeAppend k (q k) u) u keys
			Nothing -> err (q "redirect_uri" >>= parseAbsoluteURI) "No payment processor specified"
	where
	keys = ["to","name","amount","currency","description","dt","redirect_uri"]
	q k = textToString <$> queryLookup k (queryString req)

kwsoda :: URI -> Application
kwsoda _ req = textBuilder ok200 [htmlCT] $ viewKwsoda htmlEscape SodaView {
		alert = q "error",
		amount = fromMaybe (fromString "2.00") (q "amount")
	}
	where
	q k = queryLookup k (queryString req)

ripple :: URI -> Application
ripple _ req = case (q "to" >>= readMay, q "amount", q "currency", q "redirect_uri") of
	(Nothing, _, _, ruri) -> err (ruri >>= parseAbsoluteURI . textToString)
		"No payment target specified"
	(_, Just _, Nothing, ruri) -> err (ruri >>= parseAbsoluteURI . textToString)
		"Amount specified, but currency omitted"
	(Just to, amount, currency, Just ruri) ->
		textBuilder ok200 [htmlCT] $ viewRipple htmlEscape RippleView {
			base = show uri,
			redirect_uri = ruri,
			to = show to,
			to_md5 = show $ md5 $ LZ.fromChunks
				[BS.singleton 0, i2bs_unsized $ rippleAddressPayload to],
			name = q "name",
			description = q "description",
			amnt = amount,
			currency = currency,
			dt = q "dt"
		}
	_ -> redirect' seeOther303 [] uri
	where
	uri = maybeAppend "amount" amnt $
		foldl' (\u (k,k') -> maybeAppend k' (textToString <$> q k) u)
			(URI "https:" (Just $ URIAuth "" "ripple.com" "") "//send" "" "") keys
	amnt = case (q "amount", q "currency") of
		(Just a, Just c) -> Just $ textToString a ++ "/" ++ textToString c
		_ -> Nothing
	keys = [("to", "to"), ("name", "name"), ("description", "label"), ("dt", "dt")]
	q k = queryLookup k (queryString req)
