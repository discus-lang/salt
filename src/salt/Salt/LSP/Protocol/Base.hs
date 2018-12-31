
module Salt.LSP.Protocol.Base 
        ( module Text.JSON
        , module Control.Monad

        -- * Unpack
        , Unpack (..)

        -- * Flattening
        , flattenJSValue

        -- * Primitive getters.
        , getObject 
        , getField
        , getString,  getStringNull
        , getInteger, getIntegerNull)
where
import Control.Monad
import Text.JSON
import qualified Data.Ratio     as Ratio


---------------------------------------------------------------------------------------------------
class Unpack a where
 unpack :: JSValue -> Maybe a
 
instance Unpack JSValue where
 unpack = Just

instance Unpack String where
 unpack = getString

instance Unpack Integer where
 unpack = getInteger


---------------------------------------------------------------------------------------------------
flattenJSValue :: JSValue -> [([String], JSValue)]
flattenJSValue js0
 = go [] js0
 where
        go fs (JSObject obj)
         = concat [go (fs ++ [f]) jv | (f, jv) <- fromJSObject obj ]

        go fs js 
         = [(fs, js)]         

---------------------------------------------------------------------------------------------------
-- | Get the fields of an object from a `JSValue`.
getObject :: JSValue -> Maybe [(String, JSValue)]
getObject js
 = case js of
       JSObject obj     -> Just $ fromJSObject obj
       _                -> Nothing


-- | Get an object field from `JSValue`.
getField :: JSValue -> String -> Maybe JSValue
getField js name
 = case js of
        JSObject obj    -> lookup name $ fromJSObject obj
        _               -> Nothing


-- | Get a `String` from a `JSValue`.
getString :: JSValue -> Maybe String
getString js
 = case js of
        JSString str    -> Just $ fromJSString str
        _               -> Nothing


-- | Get a `String` from a `JSValue`.
getStringNull :: JSValue -> Maybe (Maybe String)
getStringNull js
 = case js of
        JSNull          -> Just $ Nothing
        JSString str    -> Just $ Just $ fromJSString str
        _               -> Nothing


-- | Get an `Integer` from a `JSValue`.
getInteger :: JSValue -> Maybe Integer
getInteger js
 = case js of
        JSRational _ r  
           | Ratio.denominator r == 1
           -> Just $ Ratio.numerator r
        _  -> Nothing


-- | Get an `Integer` from a `JSValue`.
getIntegerNull :: JSValue -> Maybe (Maybe Integer)
getIntegerNull js
 = case js of
        JSNull -> Just $ Nothing
        JSRational _ r  
           | Ratio.denominator r == 1
           -> Just $ Just $ Ratio.numerator r
        _  -> Nothing
