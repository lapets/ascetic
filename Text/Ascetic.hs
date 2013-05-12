----------------------------------------------------------------
--
-- Ascetic
-- 
-- Text/Ascetic.hs
--   Data structure, combinators, and functions for assembling
--   data and emitting files in any XML-like or HTML-like
--   markup language (consisting of tags, elements, attributes,
--   declarations, and ASCII text content). Trade-offs are made
--   in favor of simplicity and concision of constructors and
--   combinators.

----------------------------------------------------------------
-- 

module Text.Ascetic
  where
  
import Data.String.Utils

----------------------------------------------------------------
-- Data type for simple markup trees and class for data types
-- that can be converted into it.

type Content = String
type Tag = String
type Attribute = String
type Value = String

data Ascetic =
    C Tag Content                        -- Content.
  | E Tag [Ascetic]                      -- Element.
  | A Tag [(Attribute, Value)] [Ascetic] -- Element with attributes.
  | D Tag [(Attribute, Value)] Ascetic   -- Declaration.
  deriving  (Eq)

class ToAscetic a where
  ascetic :: a -> Ascetic

----------------------------------------------------------------
-- Conversion to ASCII string.

instance Show Ascetic where
  show x = to "" x where 
    to ind x = case x of
      C t c  -> ind ++ "<" ++ t ++ ">" ++ c ++ "</" ++ t ++ ">"
      E t [] -> "<" ++ t ++ "/>"
      E t xs -> 
        ind 
        ++ "<" ++ t ++ ">\n" 
        ++ join "\n" [to (ind ++ "  ") x | x <- xs] 
        ++ "\n" ++ ind ++ "</" ++ t ++ ">"
      A t avs [] ->
        ind 
        ++ "<" ++ t ++ " " 
               ++ join " " [a ++ "=\"" ++ v ++ "\"" | (a,v) <- avs] 
        ++ "/>" 
      A t avs xs ->
        ind 
        ++ "<" ++ t ++ " " 
               ++ join " " [a ++ "=\"" ++ v ++ "\"" | (a,v) <- avs] 
        ++ ">\n" 
        ++ join "\n" [to (ind ++ "  ") x | x <- xs] 
        ++ "\n" ++ ind ++ "</" ++ t ++ ">"
      D t avs x ->
        ind 
        ++ "<?" ++ t ++ " " 
                ++ join " " [a ++ "=\"" ++ v ++ "\"" | (a,v) <- avs] 
        ++ "?>\n" 
        ++ (to ind x)

----------------------------------------------------------------
-- Other useful functions.

--eof
