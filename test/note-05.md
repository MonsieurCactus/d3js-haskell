I have been reading through a Haskell [d3js](http://hackage.haskell.org/package/d3js) library: 

This is the code defining Haskell box:

    box :: Selector ->  (Double,Double) -> St (Var' Selection)
    box parent (w,h) = do
    	assign
    		$ ((d3Root
    			>>> select parent
    			>>> func "append" [PText "svg"]
    			>>> width w
    			>>> height h
    			>>> style "background" "#eef") :: Chain () Selection)

The code actually exporting using the `box` function in d3.js code uses the `>>=` operator like this:

    import Control.Monad
    import qualified Data.Text as T
    import D3JS
    
    test :: Int -> IO ()
    test n = T.writeFile "generated.js" $ reify (box "#div1" (300,300) >>= bars n 300 (Data1D [100,20,80,60,120]))

---

Using the type signatures of `box` and `bar` I was able to deduce:

    
    box   :: Selector ->  (Double,Double) -> St (Var' Selection)
    bars  :: Int -> Double -> Data1D -> Var' Selection -> St ()
    (>>=) :: Monad m => m a -> (a -> m b) -> m b

The bar chart acts like a function which takes variables of type `St (Var' Selection)` and turns it to `St()`.  

    bars n 300 (Data1D [100,20,80,60,120]) :: Var' Selection -> St ()

Except our the box containing our bar chart was chosen to be of type `St (Var' Selection)` so we need the `>>=` arrow.  

Except what does `St (Var' Selection)` even mean?  It's the statement `St` analogue of `Var' Selection` which I think models assigning a variable to a given selection.

    -- St (which means Statement) monad represents JavaScript statements.
    type St r =  RWS () Text Int r
    -- A monad containing an environment of type r, 
    -- output of type w and an updatable state of type s.
    type RWS r w s = RWST r w s Identity

A statement is an instance of the read-write monad, but why is this reifiable? 

---

Lastly `St ()` statements get passed into the function `reify`.  Why??
