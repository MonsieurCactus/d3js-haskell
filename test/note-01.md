I wanted to see if it's possible to use the d3.js library from Haskell but there are a few roadblocks.

The [d3js](https://github.com/nebuta/d3js-haskell) library does exist in Haskell but that library hasn't been updated in two years and the build is failing.  It seems didn't get very far beyond a basic bar chart.

The source code was confusing.  What is the minimum I need to put together a nice chart?

I found:

```haskell
    writeToHtml :: (Reifiable a) => FilePath -> a -> IO ()
    writeToHtml path a = T.writeFile path $ T.concat 
    ["<html> <head><body> <div id='body'></div> <script src='",
    d3jsUrl,
    "' charset='utf-8'></script> <script charset='utf-8'>\n",
    reify a,
    "\n</script> </body> </html>"]
    
    d3jsUrl = "./d3.js"
    -- d3jsUrl = "http://d3js.org/d3.v3.min.js"
```

The function `reify` seems to turn Haskell d3js objects into d3.js javascript  code.  It's debatable which is more real,

* are the Haskell objects represent d3js entities? or 
* the Haskell objects are real, and d3.js represents them as strings.

How to draw a chart?

``` haskell
    test1 = do
    	ps <- rand2D 100
    	writeToHtml "test1.html" (graph1 ps)

    graph1 :: [(Double,Double)] -> St ()
    
    graph1 ps = do
    	let dim = (300,300)
    	elem <- box "#div1" dim
    	scatter (mkScatter (Data2D ps)) elem
    	addFrame (Size 300 300) (Size 250 250) elem 
```

Then the chart becomes a combination of the pure haskell objects 

``` haskell
    addFrame :: Sel2 a => Size -> Size -> Var' a -> St ()
    addFrame (Size w h) (Size w2 h2) box = do
    	let dx = (w-w2)/2
    	let dy = (h-h2)/2
    	let sx = w2/w
    	let sy = h2/h
    	execute $
    		Val'' box
    		>>> selectAll ".p"  -- means data points.
    		>>> transform' dx dy sx sy 0
    	v <- assign $ Val' $ RectData [(Scalar dx,Scalar dy,Scalar w2,Scalar h2)]
    	execute $
    		Val'' box
    		>>> addRect v
    		>>> fill' "none"
    		>>> attrt "stroke" "black"
    		>>> attrd "stroke-width" 1
```

Addframe seem really complicated.  Next: 

``` haskell
    data Scatter = Scatter (Range Coord1) (Range Coord1) Ticks Ticks Data2D deriving (Show)
    
    autoTick :: Range Coord1 -> Range Scalar -> Ticks
    autoTick cr@(Range cmin cmax) vr@(Range vmin vmax) =
    	let
    		n = 5  -- stub
    		vint = (vmax-vmin)/Scalar n -- stub
    		vs = map (\i -> vmin + vint * Scalar i) [0..n]
    		cs = map (scaleInRange cr vr) vs -- map (\i -> cmin + cint * Coord1 i) [0..n]
    	in Ticks (zipWith (\v c -> (v, c)) vs cs)
    
    -- make scatter with auto range.
    mkScatter :: Data2D -> Scatter
    mkScatter ps@(Data2D vs) =
    	let
    		xs = map fst vs
    		ys = map snd vs
    		cx = Range 0 300
    		cy = Range 0 300
    		tx = autoTick cx $ Range (Scalar $ minimum xs) (Scalar $ maximum xs)
    		ty = autoTick cy $ Range (Scalar $ minimum ys) (Scalar $ maximum ys)
    	in Scatter cx cy tx ty ps
    
    scaleInRange :: Range Coord1 -> Range Scalar -> (Scalar -> Coord1)
    scaleInRange (Range cmin cmax) (Range vmin vmax) v =
    	cmin + (Coord1 . unScalar)((v-vmin)/(vmax-vmin) * (Scalar . unCoord1) (cmax-cmin))
    
    scatter :: Scatter -> Var' Selection -> St (Var' (SelData Data2D))
    scatter s@(Scatter rx ry tx ty ps) (Var' elem) = do
    	v <- assign $ Val' ps
    	cs <- assign $
    		(Val elem :: Chain () Selection)
    		>>> addCircles v
    	return cs
```

This got rather complicated.  At least we have all the parts.  Maybe I would like to try an even simpler example.
This was supposed to be question on StackOverflow -- I don't have much of a question except what is this??

### [Reify](https://github.com/MonsieurCactus/d3js-haskell/blob/master/D3JS/Reify.hs)

I had to look up in a dictionary what Reify means:

>  **[Reify]()** transitive verb
> :  to regard (something abstract) as a material or concrete thing

and the factoid

> Did You Know?
> Reify is a word that attempts to provide a bridge between what is abstract and what is real. Fittingly, it derives from a word that is an ancestor to "real" - the Latin noun res, meaning "thing." Both "reify" and the related noun "reification" first appeared in English in the mid-19th century, though "reification" is a few years older and some dictionaries consider "reify" to be a back-formation of the noun. In general use, the words refer to the act of considering or presenting an abstract idea in real or material terms, or of judging something by a concrete example.

So there you have it.  `Reify` acts on `Reifiable` types, which a different rule or each type.  `Data2D, Numfunc r, JSParam`, etc.

