---------------------------------------------------------------------
--
-- | Ascetic
-- 
-- @Text\/Ascetic.hs@
--
--   Data structure, combinators, and functions for assembling
--   data and emitting files in any XML-like or HTML-like
--   markup language (consisting of tags, elements, attributes,
--   declarations, and ASCII text content). Trade-offs are made
--   in favor of simplicity and concision of constructors and
--   combinators.

---------------------------------------------------------------------
-- 

module Text.Ascetic
  where
  
import Data.String.Utils (join)

---------------------------------------------------------------------
-- | Data type for simple markup trees and class for data types
--   that can be converted into it.

type Content = String
type Tag = String
type Attribute = String
type Value = String

data Ascetic =
    C Content                            -- Content.
  | E Tag [Ascetic]                      -- Element.
  | A Tag [(Attribute, Value)] [Ascetic] -- Element with attributes.
  | L [Ascetic]                          -- Undelimited list.
  | D Tag [(Attribute, Value)] Ascetic   -- Declaration.
  deriving  (Eq)

---------------------------------------------------------------------
-- | Type class for data structures that can be converted into the
--   Ascetic representation.
  
class ToAscetic a where
  ascetic :: a -> Ascetic

---------------------------------------------------------------------
-- | Conversion to ASCII string (with indentation for legibility).

ascii x = to "" x where
  showAVs avs = [a ++ "=\"" ++ v ++ "\"" | (a,v) <- avs]
  to ind x = case x of
      C c           -> c
      E t []        -> "<" ++ t ++ ">" ++ "</" ++ t ++ ">"
      E t [C c]     -> ind ++ "<" ++ t ++ ">" ++ c ++ "</" ++ t ++ ">"
      E t xs        -> 
        ind 
        ++ "<" ++ t ++ ">\n" 
        ++ join "\n" [to (ind ++ "  ") x | x <- xs] 
        ++ "\n" ++ ind ++ "</" ++ t ++ ">"
      A t avs []    -> ind ++ "<" ++ t ++ " " ++ join " " (showAVs avs) ++ ">" ++ "</" ++ t ++ ">"
      A t avs [C c] -> ind ++ "<" ++ t ++ " " ++ join " " (showAVs avs) ++ ">" ++ c ++ "</" ++ t ++ ">"
      A t avs xs    ->
        ind 
        ++ "<" ++ t ++ " " ++ join " " (showAVs avs) ++ ">\n" 
        ++ join "\n" [to (ind ++ "  ") x | x <- xs] 
        ++ "\n" ++ ind ++ "</" ++ t ++ ">"
      L xs          -> join "\n" [to ind x | x <- xs]
      D t avs x     ->
        ind 
        ++ "<?" ++ t ++ " " ++ join " " (showAVs avs) ++ "?>\n" 
        ++ (to ind x)

---------------------------------------------------------------------
-- | Conversion to an ASCII string that has no extra indentation or
--   newlines for legibility.

minified x = to x where
  showAVs avs = [a ++ "=\"" ++ v ++ "\"" | (a,v) <- avs]
  to x = case x of
      C c           -> c
      E t []        -> "<" ++ t ++ ">" ++ "</" ++ t ++ ">"
      E t [C c]     -> "<" ++ t ++ ">" ++ c ++ "</" ++ t ++ ">"
      E t xs        -> 
           "<" ++ t ++ ">" 
        ++ join "" [to x | x <- xs] 
        ++ "" ++ "</" ++ t ++ ">"
      A t avs []    -> "<" ++ t ++ " " ++ join " " (showAVs avs) ++ ">" ++ "</" ++ t ++ ">"
      A t avs [C c] -> "<" ++ t ++ " " ++ join " " (showAVs avs) ++ ">" ++ c ++ "</" ++ t ++ ">"
      A t avs xs    ->
           "<" ++ t ++ " " ++ join " " (showAVs avs) ++ ">" 
        ++ join "" [to x | x <- xs] 
        ++ "" ++ "</" ++ t ++ ">"
      L xs          -> join "" [to x | x <- xs]
      D t avs x     ->
           "<?" ++ t ++ " " ++ join " " (showAVs avs) ++ "?>\n" 
        ++ (to x)

---------------------------------------------------------------------
-- | Default rendering uses "min" for HTML whitespace fidelity.

instance Show Ascetic where
  show = minified

--eof