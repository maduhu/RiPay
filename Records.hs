module Records where

import BasicPrelude

data SodaView = SodaView {
		alert :: Maybe Text,
		amount :: Text
	}

data RippleView = RippleView {
		base :: Text,
		redirect_uri :: Text,
		to :: Text,
		to_md5 :: Text,
		name :: Maybe Text,
		description :: Maybe Text,
		amnt :: Maybe Text,
		currency :: Maybe Text,
		dt :: Maybe Text
	}
