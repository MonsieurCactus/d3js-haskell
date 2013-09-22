{-# LANGUAGE OverloadedStrings, GADTs, NoImplicitPrelude, ExistentialQuantification, FlexibleInstances #-}

module D3JS.Type where

import Data.Text (Text)
import qualified Data.Text as T

import Prelude hiding ((.),id)
import Control.Category 

-- * Types

-- |This represents a method chain with an initial type of `a` and a final type of `b`
-- Chains are composable by functions in "Control.Category" module.
-- See "D3JS.Chart" for examples.
data Chain a b where
	Val :: Var -> Chain () b
	Val' :: (Reifiable b) => b -> Chain () b
	Val'' :: Var' b -> Chain () b
	Func :: JSFunc a params b -> Chain a b
	Concat :: Chain c b -> Chain a c -> Chain a b
	Nil :: Chain a a

-- | Chain a b behaves just like (a -> b).
-- Val Var is the starting point of chain (= constant),
-- and Nil is the termination of chain.
instance Category Chain where
	id = Nil
	(.) = Concat

type Var = Text

type Selector = Text

-- data D3Root = D3Root

data Data1D = Data1D [Double]
data Data2D = Data2D [(Double,Double)]

data Selection = Selection

data SelData a = SelData
data Transition = Transition  -- this is just used as a tag for chaining functions with a type.

-- |Instances of Reifiable can generate a JavaScript code fragment.
class Reifiable a where
	reify :: a -> Text

-- |Used just as a tag for typing method chains. Used in "D3JS.Func".
class Sel a
instance Sel Selection
instance Sel (SelData a)

-- |Used just as a tag for typing method chains. Used in "D3JS.Func".
class Sel2 a
instance Sel2 Selection
instance Sel2 (SelData a)
instance Sel2 (Chain () b)



-- * For internal use

-- | Function call
data JSFunc a c b = JSFunc FuncName [JSParam]  -- name and params

type FuncName = Text

-- | Parameter for a function call
data JSParam = ParamVar Var | PText Text | PDouble Double | PFunc FuncDef

-- | Function definition used for a callback.
data FuncDef = FuncTxt Text | forall r. FuncExp (NumFunc r)

-- | Representation of JavaScript function for a callback.
data NumFunc r where
	NInt :: Int -> NumFunc Int
	NDouble :: Double -> NumFunc Double
	Add :: NumFunc r -> NumFunc r -> NumFunc r
	Subt :: NumFunc r -> NumFunc r -> NumFunc r
	Mult :: NumFunc r -> NumFunc r -> NumFunc r
	Div :: NumFunc r -> NumFunc r -> NumFunc r
	Index :: Int -> NumFunc [r] -> NumFunc r
	Field :: Text -> NumFunc a -> NumFunc r
	NVar :: Var -> NumFunc r
	DataParam :: NumFunc r

-- |This should not be used directly by users. Users should use 'assign' to get a variable instead.
data Var' dat = Var' Var  -- typed variables

