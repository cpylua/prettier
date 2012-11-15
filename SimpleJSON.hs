module SimpleJSON
       (
         JValue(..),
         getString,
         getInt,
         getDouble,
         getBool,
         getObject,
         getArray,
         isNull
       ) where

data JValue = JString String
            | JNumber Double
            | JBool Bool
            | JNull
            | JObject [(String, JValue)]
            | JArray [JValue]
            deriving (Show, Ord, Eq)
                     
getString :: JValue -> Maybe String
getString (JString s) = Just s
getString _           = Nothing

getInt :: JValue -> Maybe Int
getInt (JNumber x) = Just (truncate x)
getInt _           = Nothing

getDouble :: JValue -> Maybe Double
getDouble (JNumber x) = Just x
getDouble _           = Nothing

getBool :: JValue -> Maybe Bool
getBool (JBool b) = Just b
getBool _         = Nothing

getObject :: JValue -> Maybe [(String, JValue)]
getObject (JObject obj) = Just obj
getObject _             = Nothing

getArray :: JValue -> Maybe [JValue]
getArray (JArray a) = Just a
getArray _          = Nothing

isNull :: JValue -> Bool
isNull = (JNull ==)