{-# LANGUAGE OverloadedStrings #-}

-- |This module defines original functions of D3.js, as well as some low-level helper functions.
module D3JS.Func where

import D3JS.Type
import D3JS.Syntax

import Data.List
import Data.Text (Text)
import qualified Data.Text as T 

import Prelude hiding ((.),id)
import Control.Category

-- * Selection and data assignment

-- | d3 object in D3.js
d3Root :: Chain () Selection
d3Root = Val "d3"

use :: Var' r -> Chain () r
use = Val''

(.>) :: Var' r -> Chain r b -> Chain () b
v .> chain = Val'' v >>> chain

-- | select() in D3.js
select :: Selector -> Chain Selection Selection
select = funct1 "select"

-- | selectAll()
selectAll :: Sel2 a => Selector -> Chain a Selection
selectAll = funct1 "selectAll"

-- * Data manipulation

-- | data() in D3.js. Assigns new data to selection.
dataD3 :: DataArg r => Var' r -> Chain Selection (SelData r)
dataD3 (Var' d) = func "data" [ParamVar d]

dataD3' :: NumFunc r -> Chain Selection (SelData r)
dataD3' f = func "data" [funcExp f]

-- | insert()
insertD3 :: Text -> Chain (SelData a) (SelData a)
insertD3 = funct1 "append"

-- | enter()
enter :: Chain (SelData r) (SelData r)
enter = func "enter" []

-- | exit()
exit :: Chain (SelData r) (SelData r)
exit = func "exit" []

-- | remove()
remove :: Chain (SelData r) (SelData r)
remove = func "remove" []

datum :: Var' r -> Chain Selection (SelData r)
datum (Var' d) = func "datum" [ParamVar d]

-- | map()
mapD3 :: NumFunc r -> Chain a JSObjArray -- stub
mapD3 f = func "map" [funcExp f]

mapD3' ft = func "map" [funcTxt ft]

-- | filter()
filterD3 :: FuncDef -> Chain (SelData r) (SelData r)
filterD3 f = func "filter" [PFunc f]

-- | sort()
sortD3 :: FuncDef -> Chain (SelData r) (SelData r)
sortD3 f = func "sort" [PFunc f]

-- | order()
order :: Chain (SelData r) (SelData r)
order = func "order" []

-- | append()
appendD3 :: Text -> Chain a a
appendD3 = funct1 "append"

-- * Attributes and styles

attr :: Text -> JSParam -> Chain a a
attr key val = func' "attr" [PText key, val]

attrf :: Text -> NumFunc r -> Chain a a
attrf key val = func' "attr" [PText key, funcExp val]

attrt :: Text -> Text -> Chain a a
attrt key val = attr key (PText val)

attrd :: Text -> Double -> Chain a a
attrd key val = attr key (PDouble val)

attrds :: [(Text,Double)] -> Chain a a
attrds [] = id
attrds ((k,v):xs) = attrd k v >>> attrds xs

attri :: Text -> Int -> Chain a a
attri key val = attr key (PInt val)

style :: Text -> Text -> Chain a a
style key val = func' "style" [PText key, PText val]

-- | classed(). Take a list of classes as an argument.
classed :: [Text] -> Chain a a
classed kls = func' "classed" [PText (T.intercalate " " kls)]

property :: Text -> Text -> Chain a a
property key val = func' "property" [PText key, PText val]

text :: Text -> Chain a a
text val = func' "text" [PText val]

html :: Text -> Chain a a
html val = func' "html" [PText val]

width :: Double -> Chain a a
width v = attr "width" (PDouble v)

height :: Double -> Chain a a
height v = attr "height" (PDouble v)

transform = attrt "transform"

transform' :: Double -> Double -> Double -> Double -> Double -> Chain a a
transform' tx ty sx sy r =
	attrt "transform" $
		T.concat ["translate(",f tx, " ",f ty,") scale(",f sx, " ", f sy, ") rotate(",f r, ")"]
	where
		f v = T.pack $ show v

opacity :: Sel a => Double -> Chain a a
opacity = attrd "fill-opacity"

fill :: Sel a => JSParam -> Chain a a
fill p = func "style" [PText "fill", p]

fill' :: Sel a => Text -> Chain a a
fill' = style "fill"

-- * Color
hsl :: JSParam -> JSParam -> JSParam -> NumFunc Color
hsl h s l = ApplyFunc' "d3.hsl" [h,s,l]

-- * Animation and Interaction

-- | on()
on :: Text -> FuncDef -> Chain a a
on typ f = func "on" [PText typ,PFunc f]

mouse :: Text -> FuncDef
mouse container = FuncTxt $ T.concat ["d3.mouse(",container,")"]

touches :: Text -> FuncDef
touches container = FuncTxt $ T.concat ["d3.touches(",container,")"]

-- | transition()
transition :: (Sel2 a) => Chain a Transition
transition = func "transition" []

-- | trasition().duration(time). time can be any of "JSParam"
transition' :: (Sel2 a) => JSParam -> Chain a Transition
transition' d = transition >>> func "duration" [d]

-- | trasition().duration(time). time can be any of "JSParam"
transition_d :: (Sel2 a) => Double -> Chain a Transition
transition_d d = transition >>> funcd1 "duration" d

interrupt = func "interrupt" []

-- | delay()
delay :: JSParam -> Chain Transition Transition
delay v = func "delay" [v]

-- * Control

-- | each()
each :: FuncDef -> Chain a a
each f = func "each" [PFunc f]

-- | call()
call :: FuncDef -> Chain a a
call f = func "call" [PFunc f]

-- | empty()
empty :: (Sel a) => Chain a Bool
empty = func "empty" []

-- | node()
node :: (Sel a) => Chain a a
node = func "node" []

-- | size()
size :: (Sel a) => Chain a Int
size = func "size" []

-- | size by attributes width, height
size_ w h = attrd "width" w >>> attrd "height" h
sizei_ w h = attrd "width" (fromIntegral w) >>> attrd "height" (fromIntegral h)

-- * Transitions

-- * Arrays
range :: Int -> Chain () Data1D
range to = d3Root >>> funci1 "range" to


-- * Scales

category10 :: Chain () Scale
category10 = Val "d3.scale" >>> func "category10" []

-- * Force

force :: Chain () Force
force = Val "d3.layout.force()"

gravity :: Double -> Chain Force Force
gravity = funcd1 "gravity"

charge :: JSParam -> Chain Force Force
charge v = func "charge" [v]

nodes :: Var' r -> Chain Force Force
nodes (Var' d) = func "nodes" [ParamVar d]

force_size :: (Double,Double) -> Chain Force Force
force_size (w,h) = func "size" [PArray [(PDouble w),(PDouble h)]]
-- * Helper functions for Chain a b type

func :: FuncName -> [JSParam] -> Chain a b
func name params = Func $ JSFunc name params

funct1 name t = func name [PText t]

funci1 name v = func name [PInt v]

funcd1 name v = func name [PDouble v]

field :: Text -> Chain a b
field n = ChainField n

-- |Function that does not change type in a method chain.
func' :: FuncName -> [JSParam] -> Chain a a
func' = func


