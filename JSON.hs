{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module JSON where

import Control.Arrow (second)

data JValue = JString String
            | JNumber Double
            | JBool Bool
            | JNull
            | JObject (JObj JValue)
            | JArray (JAry JValue)
            deriving (Eq, Show)
                     
newtype JAry a = JAry {
  fromJAry :: [a]
} deriving (Eq, Show)

newtype JObj a = JObj {
  fromJObj :: [(String, a)]
} deriving (Eq, Show)

class JSON a where
  toJValue   :: a -> JValue
  fromJValue :: JValue -> Either JSONError a

type JSONError = String


instance JSON JValue where
  toJValue   = id
  fromJValue = Right
  
instance JSON String where
  toJValue               = JString
  fromJValue (JString s) = Right s
  fromJValue _           = Left "not a JSON string"
  
instance JSON Int where
  toJValue   = JNumber . fromIntegral
  fromJValue = doubleToJValue truncate
  
instance JSON Integer where
  toJValue   = JNumber . fromIntegral
  fromJValue = doubleToJValue truncate
  
instance JSON Double where
  toJValue   = JNumber
  fromJValue = doubleToJValue id
  
instance JSON Bool where
  toJValue             = JBool
  fromJValue (JBool b) = Right b
  fromJValue _         = Left "not a JSON bool"

instance (JSON a) => JSON (JAry a) where
  toJValue   = jaryToJValue
  fromJValue = jaryFromJValue
  
instance (JSON a) => JSON (JObj a) where
  toJValue = JObject . JObj . map (second toJValue) . fromJObj
  fromJValue (JObject (JObj obj)) = whenRight JObj (mapEither unwrap obj)
    where unwrap (k,v) = whenRight ((,) k) (fromJValue v)
  fromJValue _ = Left "not a JSON object"

listToJValue :: (JSON a) => [a] -> JValue
listToJValue = JArray . JAry . map toJValue


doubleToJValue :: (Double -> a) -> JValue -> Either JSONError a
doubleToJValue f (JNumber n) = Right (f n)
doubleToJValue _ _           = Left "not a JSON number"

jaryToJValue :: (JSON a) => JAry a -> JValue
jaryToJValue = JArray . JAry . map toJValue . fromJAry

jaryFromJValue :: (JSON a) => JValue -> Either JSONError (JAry a)
jaryFromJValue (JArray (JAry a)) = whenRight JAry (mapEither fromJValue a)
jaryFromJValue _                 = Left "not a JSON array"

whenRight :: (b -> c) -> Either a b -> Either a c
whenRight f (Right x)  = Right (f x)
whenRight _ (Left err) = Left err

mapEither :: (a -> Either b c) -> [a] -> Either b [c]
mapEither f = foldr fEither (Right [])
  where fEither x (Right ys) = case f x of
          Right y  -> Right (y:ys)
          Left err -> Left err
        fEither _ err = err

  