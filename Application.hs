{-# LANGUAGE CPP #-}
module Application (formRedirect, kwsoda) where

import Prelude ()
import BasicPrelude

import Network.Wai (Application, Response, queryString)
import Network.HTTP.Types (ok200, badRequest400, seeOther303)
import Network.Wai.Util (queryLookup, string, redirect', textBuilder, stringHeaders)

import Network.URI (URI(..), escapeURIString, isUnescapedInURIComponent, parseAbsoluteURI)

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
	maybeAppend k (Just x) = (`queryAppend` (k,x))
	maybeAppend _ Nothing = id
	q k = textToString <$> queryLookup k (queryString req)

kwsoda :: URI -> Application
kwsoda _ req = textBuilder ok200 [htmlCT] $ viewKwsoda htmlEscape SodaView {
		alert = q "error",
		amount = fromMaybe (fromString "2.00") (q "amount")
	}
	where
	q k = queryLookup k (queryString req)
