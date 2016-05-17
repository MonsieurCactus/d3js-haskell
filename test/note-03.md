At this point I am having my doubts about the Haskell d3.js library, since it tries to achieve so much abstraction at once.  These abstractions -- the ones we still use, like **numbers** and **letters** took thousands of years to develop and refine.  How do we expect to come up with good choices in a couple of weeks?  

Let's look at one example: `box`:

    import Control.Monad
    import qualified Data.Text as T
    --import D3JS
    
    test :: Int -> IO ()
    test n = T.writeFile "generated.js" $ reify (box "#div1" (300,300) >>= bars n 300 (Data1D [100,20,80,60,120]))

I have commented out the `import D3JS` statement so that we can reconstruct the mimimum amount of Haskell necessary to draw our box chart.

To get us started:

    :t (writeFile)
    (writeFile) :: FilePath -> String -> IO ()

secondly let's define `$`

    :t ($)
    ($) :: (a -> b) -> a -> b

If `$` is syntactic sugar, Haskell is an austere drink at best.  Chapter [6] talks about `$` being the "lowest precedence" operator.  Instead I just want:

    ($) :: (a -> b) -> a -> b  
    f $ x = f x  

Then functions are usually **left-associative** `f a b c = (((f a) b) c)` and the `$` is **right-associative** `f $ a b c = (f ((a b) c)) = f (a b c)`

I would like to understand 

    reify (box "#div1" (300,300) >>= bars n 300 (Data1D [100,20,80,60,120]))

`reify` is some  kind of procedure for taking graphics to some kind of Javascript text

* `box "#div1" (300,300)` (a 300 &times; 300 box)
* `>>=` ( join together )
* `bars n 300 (Data1D [100,20,80,60,120])` (some numbers)

How are `box` and `>>=` and `bars` defined.  And we're on our way to making our first chart!
