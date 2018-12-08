
-- | Strings used for generating moderately readable names and things. 
module Waves.Gen.Corpus where
import Data.String

letters :: IsString a => [a]
letters = map (fromString . (:[])) ['a' .. 'z']

colours :: IsString a => [a]
colours =
  [ "amber"
  , "beige"
  , "black"
  , "blue"
  , "brown"
  , "green"
  , "grey"
  , "indigo"
  , "navy"
  , "ochre"
  , "pink"
  , "purple"
  , "red"
  , "silver"
  , "teal"
  , "violet"
  , "white"
  , "yellow"
  ]

fruits :: IsString a => [a]
fruits =
  [ "apple"
  , "apricot"
  , "avocado"
  , "banana"
  , "cherry"
  , "coconut"
  , "fig"
  , "grapefruit"
  , "guava"
  , "jujube"
  , "kiwifruit"
  , "lime"
  , "lychee"
  , "mango"
  , "orange"
  , "peach"
  , "plum"
  , "quince"
  , "strawberry"
  , "watermelon"
  ]

