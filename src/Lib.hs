{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( decodeBSON
    , aesonify
    ) where

import Data.HashMap.Strict as HashMap (fromList, toList)
import Data.Bson as BSON
import Data.Aeson.Types as AESON
import Data.ByteString.Lazy.Internal
import Data.Vector as Vector (toList, fromList)
import Data.Aeson (decode, toJSON, Object)
import Data.Maybe (fromJust)
import Data.Scientific

bsonifyValue :: AESON.Value -> BSON.Value
bsonifyValue (Object obj) = Doc $ toBson obj
bsonifyValue (AESON.Array array) = BSON.Array . map bsonifyValue . Vector.toList $ array
bsonifyValue (AESON.String str) = BSON.String str
bsonifyValue (Number n) = case floatingOrInteger n of
                            Left float -> Float float
                            Right int -> Int64 $ fromIntegral int
bsonifyValue (AESON.Bool b) = BSON.Bool b
bsonifyValue _ = BSON.Null

decode' :: ByteString -> Object
decode' x = fromJust (decode x :: Maybe Object)

toBson :: Object -> Document
toBson = map (\(t, v) -> (t := bsonifyValue v)) . HashMap.toList

decodeBSON = toBson . decode'



-- | Converts a BSON value to JSON.
aesonifyValue :: BSON.Value -> AESON.Value
aesonifyValue (Float f) = toJSON f
aesonifyValue (BSON.String s) = toJSON s
aesonifyValue (Doc doc) = Object $ aesonify doc
aesonifyValue (BSON.Array list) = AESON.Array . Vector.fromList $ map aesonifyValue list
{- aesonifyValue (Bin (Binary binary)) = toJSON binary -}
{- aesonifyValue (Fun (Function function)) = toJSON function -}
{- aesonifyValue (Uuid (UUID uuid)) = toJSON uuid -}
{- aesonifyValue (Md5 (MD5 md5)) = toJSON md5 -}
{- aesonifyValue (UserDef (UserDefined userdef)) = toJSON userdef -}
aesonifyValue (ObjId oid) = toJSON $ show oid -- Relies on bson to show the OID as 24 digit hex.
                                              -- It would be better if BSON exposed a non-show function for this,
                                              -- preferably a fast one.
aesonifyValue (BSON.Bool bool) = toJSON bool
aesonifyValue (UTC utc) = toJSON utc
aesonifyValue (BSON.Null) = AESON.Null
aesonifyValue (RegEx (Regex pattern mods)) = toJSON $ mconcat ["/", show pattern, "/", show mods]
aesonifyValue (JavaScr (Javascript env code)) = object [ "environment" .= aesonify env
                                                       , "code" .= code ]
aesonifyValue (Sym (Symbol sym)) = toJSON sym
aesonifyValue (Int32 int32) = toJSON int32
aesonifyValue (Int64 int64) = toJSON int64
aesonifyValue (Stamp (MongoStamp int64)) = toJSON int64
aesonifyValue (MinMax mm) = case mm of { MinKey -> toJSON (-1 :: Int)
                                       ; MaxKey -> toJSON (1 :: Int)}

-- | Converts a BSON document to an AESON object.
aesonify :: BSON.Document -> AESON.Object
aesonify = HashMap.fromList . map (\(l := v) -> (l, aesonifyValue v))
