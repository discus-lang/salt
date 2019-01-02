
module Salt.LSP.Protocol.Base 
        ( module Text.JSON
        , module Control.Monad

        -- * JsonRpcId
        , JsonRpcId (..)

        -- * Unpack
        , Unpack (..)

        -- * Pack
        , Pack (..), jobj
        , JSPackObj(..), JSPackField(..), (?=)

        -- * Flattening
        , flattenJSValue

        -- * Primitive getters.
        , getObject, getField
        , getArray
        , getString,  getStringNull
        , getInteger, getIntegerNull)
where
import Control.Monad
import Text.JSON
import qualified Data.Ratio     as Ratio


---------------------------------------------------------------------------------------------------
data JsonRpcId
        = JsonRpcIdInt    Integer
        | JsonRpcIdString String
        | JsonRpcIdNull
        deriving Show


instance Pack JsonRpcId where
 pack = \case
        JsonRpcIdInt i    -> pack i
        JsonRpcIdString s -> pack s
        JsonRpcIdNull     -> JSNull


instance Unpack JsonRpcId where
 unpack js
  | Just i    <- getInteger js
  = return $ JsonRpcIdInt i

  | Just s    <- getString js
  = return $ JsonRpcIdString s

  | JSNull    <- js
  = return $ JsonRpcIdNull

  | otherwise
  = Nothing


---------------------------------------------------------------------------------------------------
class Unpack a where
 unpack :: JSValue -> Maybe a
 
instance Unpack JSValue where
 unpack = Just

instance Unpack String where
 unpack = getString

instance Unpack Integer where
 unpack = getInteger

instance Unpack [(String, JSValue)] where
 unpack = getObject


---------------------------------------------------------------------------------------------------
class Pack a where
 pack :: a -> JSValue

instance Pack JSValue where
 pack = id

instance Pack String where
 pack   = JSString . toJSString

instance Pack Integer where
 pack i = JSRational False (fromIntegral i)

instance Pack Int where
 pack i = JSRational False (fromIntegral i)

instance Pack [(String, JSValue)] where
 pack fs = JSObject $ toJSObject fs

instance Pack Bool where
 pack b = JSBool b 


-- Helper for constructing literal objects.
data JSPackObj  
        = O [JSPackField]
        | A [JSPackObj]
        | B Bool
        | S String
        | I Integer
        | J Int
        | V JSValue
        deriving Show

data JSPackField
        = (:=) String JSPackObj
        | FM   String (Maybe JSPackObj)
        deriving Show

infixr 0 :=

infixr 0 ?=
(?=) s mo = FM s mo

instance Pack JSPackObj where
 pack (O fs)    
  = let make (s := o)           = Just (s, o)
        make (FM s (Just o))    = Just (s, o)
        make (FM _ (Nothing))   = Nothing
    in  pack [ (s, pack o) | Just (s, o) <- map make fs]

 pack (A js)    = JSArray (map pack js)
 pack (B b)     = JSBool b
 pack (S s)     = JSString $ toJSString s
 pack (I i)     = JSRational False (fromIntegral i)
 pack (J i)     = JSRational False (fromIntegral i)
 pack (V j)     = j

jobj :: [JSPackField] -> JSValue
jobj fs = pack $ O fs


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


-- | Get the elements of an array from a `JSValue`.
getArray  :: JSValue -> Maybe [JSValue]
getArray js
 = case js of
        JSArray jss     -> Just jss
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
