I decided to poke through the [d3js](http://hackage.haskell.org/package/d3js) library in Haskell but after it didn't install through Stackage.

    $ stack install d3js
    
    Run from outside a project, using implicit global project config
    Using resolver: lts-5.2 from implicit global project's config file: /home/john/.stack/global-project/stack.yaml
    While constructing the BuildPlan the following exceptions were encountered:
    
    --  Failure when adding dependencies:    
          base: needed (>=4.6 && <4.7), 4.8.2.0 found (latest applicable is 4.6.0.1)
        needed for package d3js-0.1.0.0

Instead I got the more interestin idea of looking through the source of [d3js-haskell](https://github.com/nebuta/d3js-haskell).  If I could install the library this could be one of the simplest examples: a bar chart.

     import Control.Monad
     import qualified Data.Text as T
     import D3JS
    
     test :: Int -> IO ()
     test n = T.writeFile "generated.js" $ reify (box "#div1" (300,300) >>= bars n 300 (Data1D [100,20,80,60,120]))

Even with this simple example I have a number of questions.  **How does `reify` work ?**  I had to look up the word in a dictionary:

> to regard (something abstract) as a material or concrete thing
> 
> ### Did you know?
> *Reify* is a word that attempts to provide a bridge between what is abstract and what is real. Fittingly, it derives from a word that is an ancestor to "real" - the Latin noun *res*, meaning "thing." Both "reify" and the related noun "reification" first appeared in English in the mid-19th century, though "reification" is a few years older and some dictionaries consider "reify" to be a back-formation of the noun. In general use, the words refer to the act of considering or presenting an abstract idea in real or material terms, or of judging something by a concrete example.

That having been said the **reify** function in the d3.js library turns Haskell entities representing d3.js objects into actual d3.js code.  Do we have examples of a reifiable object?  We can fine one:

    reify (box "#div1" (300,300) >>= bars n 300 (Data1D [100,20,80,60,120]))

The object in parentheses is a reifiable object.   A tour of the source code is both enlightening and discouraging:

    -- |Instances of Reifiable can generate a JavaScript code fragment.
    class Reifiable a where
    	reify :: a -> Text

This was taken from [d3js/Type.hs](https://github.com/nebuta/d3js-haskell/blob/master/D3JS/Type.hs) Are there examples of reifiable objects? Let's look at [d3js/reify.hs](https://github.com/nebuta/d3js-haskell/blob/master/D3JS/Reify.hs):

    instance Reifiable Data1D where
    	reify (Data1D ps) = surround $ T.intercalate "," $ map show' ps
    
    instance Reifiable Data2D where
    	reify (Data2D ps) = surround $ T.intercalate "," $ map (\(x,y) -> T.concat ["[",show' x,",",show' y,"]"]) ps
    
    instance Reifiable (JSFunc params r) where
    	reify (JSFunc name params) = T.concat [name,"(",T.intercalate "," $ map reify params,")"]
    
    instance Reifiable JSParam where
    	reify (ParamVar name) = name
    	reify (PText t) = T.concat ["\"",t,"\""]
    	reify (PDouble d) = show' d
    	reify (PInt d) = show' d
    	reify (PFunc (FuncTxt t)) = t
    	reify (PFunc (FuncExp f)) = T.concat["function(d,i){return ",reify f,";}"]
    	reify (PFunc' f) = reify f
    	reify (PArray vs) = T.concat ["[",T.intercalate "," $ map reify vs,"]"]
    	reify (PChainValue v) = reify v

These are examples of reifiable types but these don't tell us **how charts are constructed in haskell-d3js?**

    -- | box parent (w,h) makes an SVG container in a parent element with dimension w x h.
    box :: Selector ->  (Double,Double) -> St (Var' Selection)
    box parent (w,h) = do
    	assign
    		$ ((d3Root
    			>>> select parent
    			>>> func "append" [PText "svg"]
    			>>> width w
    			>>> height h
    			>>> style "background" "#eef") :: Chain () Selection)
    
    bars :: Int -> Double -> Data1D -> Var' Selection -> St ()
    bars n width ps (Var' elem) = do
    	let bar_w = width / (fromIntegral n)
    	v <- assign $ Val' (mkRectData bar_w ps)
    	execute $
    		(Val elem :: Chain () Selection)
    		>>> addRect v
    		>>> fill' "red"

These examples supposedly work.  It looks like we are committed to red bars ( I haven't even seen the chart yet).

---

Let me end by some discouraging footnotes in the source code.  This from [chart.hs](https://github.com/nebuta/d3js-haskell/blob/master/D3JS/Chart.hs)

    -- This modules provides high-level functions for drawing common charts, such as bar charts and scatter plots.
    -- Those functions also exemplify how to compose primitive functions to achieve complex drawing.
    -- This module will be expanded in the near future.
