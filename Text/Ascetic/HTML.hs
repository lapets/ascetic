----------------------------------------------------------------
--
-- Ascetic
-- 
-- Text/Ascetic/HTML.hs
--   Wrappers for building HTML file represented using the
--   Ascetic data structure.

----------------------------------------------------------------
-- 

module Text.Ascetic.HTML
  where

import Data.String.Utils (join)

import qualified Text.Ascetic as A

----------------------------------------------------------------
-- Data structures specific to HTML files.

type Class = String
type Selector = String
type PseudoClass = Maybe String
type Property = String
type Value = String
type DeclarationBlock = [(Property, Value)]
data CSS = CSS [([Selector], PseudoClass, DeclarationBlock)]

type HTML = A.Ascetic

class ToHTML a where
  html :: a -> HTML

----------------------------------------------------------------
-- Combinators for assembling HTML files.

file :: HTML -> HTML -> HTML
file head body = A.E "html" [head, body]

head :: [HTML] -> HTML
head hs = A.E "head" hs

style :: CSS -> HTML
style (CSS dbs) = 
  A.C "style" $ "\n" ++ join "\n\n"
    [ (join ", " ss) ++ " {\n  " 
        ++ join "\n  " [p ++ ": " ++ v ++ ";" | (p,v) <- pvs] 
        ++ "\n}" 
    | (ss, pc, pvs) <- dbs] ++ "\n"

script :: String -> HTML
script hs = A.C "script" ""

body :: [HTML] -> HTML
body hs = A.E "body" hs

div :: [HTML] -> HTML
div hs = A.E "div" hs

divWith :: [(A.Attribute, A.Value)] -> [HTML] -> HTML
divWith avs hs = A.A "div" avs hs

span :: [HTML] -> HTML
span hs = A.E "span" hs

content :: String -> HTML
content s = A.C "span" s

----------------------------------------------------------------
-- Other useful functions.

--eof
