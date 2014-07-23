module Shady.CompileEffect.Json where

-- Standard libraries
import Text.Printf
import Data.List

data JsonValue = JVObject JsonObject
               | JVArray  JsonArray
               | JVString String
               | JVNumber Float
               | JVBool   Bool
               | JVNull deriving (Eq, Show)

newtype JsonObject = JsonObject [(String, JsonValue)] deriving (Eq, Show)
newtype JsonArray  = JsonArray [ JsonValue ]          deriving (Eq, Show)


prettyJsonString :: String -> String
prettyJsonString = show -- FIXME: This is wrong. Find out what are legal strings in Javascript

prettyJV :: JsonValue -> String
prettyJV jv = case jv of
  JVObject o -> prettyJO o
  JVArray  a -> prettyJA a
  JVString s -> show s
  JVNumber n -> show n
  JVBool b   -> if b then "true" else "false"
  JVNull     -> "null"

prettyJO :: JsonObject -> String
prettyJO (JsonObject pairs) = printf "{%s}" $ concat . intersperse ", " . map pp $ pairs
  where pp (s,jv) = printf "%s: %s" (prettyJsonString s) (prettyJV jv)

prettyJA :: JsonArray -> String
prettyJA (JsonArray jvs) = printf "[%s]" $ concat . intersperse ", " . map prettyJV $ jvs