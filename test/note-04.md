This is the code defining Haskell box:

```haskell
    box :: Selector ->  (Double,Double) -> St (Var' Selection)
    box parent (w,h) = do
    	assign
    		$ ((d3Root
    			>>> select parent
    			>>> func "append" [PText "svg"]
    			>>> width w
    			>>> height h
    			>>> style "background" "#eef") :: Chain () Selection)
```    			

The type signature says... we take a selector and two numbers ("doubles") and returns an `St` which is a **statement**.  This is part of the **statement monad** which encodes JavaScript statements.

```haskell
    box :: Selector ->  (Double,Double) -> St (Var' Selection)
```

The original d3.js code is not so complicated.  With the **chaining** it is funny-looking code (to me).

```haskell
    d3.select(parent).append("svg").attr("width", w).attr("height", h).style("background", "#eef")
```

that's too much to fit on a line - let's try again.

```haskell
    d3.select(parent)
    .append("svg")
    .attr("width", w)
    .attr("height", h)
    .style("background", "#eef")
```

What kind of statement is `St (Var' Selection)` ?  `Var` and `Var'` and `Selection` are general Haskell types.  

### d3.js --- Chains

Here are some of the rules that github user Nebuta came up with for describing d3.js chains.  Is his theory right?

```haskell
    data Chain a b where
    	Val :: Var -> Chain () b
    	Val' :: (Reifiable b) => b -> Chain () b
    	Val'' :: Var' b -> Chain () b
    	-- Func :: JSFunc params b -> Chain a b
    	Concat :: Chain c b -> Chain a c -> Chain a b
    	Nil :: Chain a a
    	-- ChainField :: Text -> Chain a b
    	Refine :: JSObjClass a => Text -> Chain a b
    	Apply :: forall a b params. [JSParam] -> Chain a (JSFunc params b) -> Chain a b
```

Notice that `Func` and `ChainField` have been commented out.  At least, we have learned that d3.js chains form a **category** indeed: 

```haskell
    import Control.Category 
    -- |This represents a method chain with an initial type of `a` and a final type of `b`
    -- Chains are composable by functions in "Control.Category" module.
    -- See "D3JS.Chart" for examples.
```

And even more clarification.  We have that Chains behave just like arrows:

```haskell
    -- | Chain a b behaves just like (a -> b).
    -- Val Var is the starting point of chain (= constant),
    -- and Nil is the termination of chain.
    instance Category Chain where
    	id = Nil
    	(.) = Concat
```

This seems very much like a mistake to develop all the abstractions at once when your main goal is a chart.  

Why is d3.js chaning like [arrow](https://wiki.haskell.org/Arrow_tutorial) composition:

```haskell
    (>>>) :: (Arrow a) => a b c -> a c d -> a b d
```

Arr builds an arrow out of a function. This function is arrow-specific. Its signature is

```haskell
    arr :: (Arrow a) => (b -> c) -> a b c
```

Selection is an instance of JavaScript Object class

```haskell
    class JSObjClass a
    instance JSObjClass JSObj
    instance JSObjClass Force
    instance JSObjClass Histogram
    instance JSObjClass Selection
```

What could `Var'` mean?? Here is `Val'` and `Val''` but not `Var'`

```haskell
    data Chain a b where
      Val :: Var -> Chain () b
      Val' :: (Reifiable b) => b -> Chain () b
      Val'' :: Var' b -> Chain () b
```
