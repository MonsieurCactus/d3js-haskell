D3.js binding for Haskell

[![Build Status](https://travis-ci.org/nebuta/d3js-haskell.png)](https://travis-ci.org/nebuta/d3js-haskell)

Ver. 0.1.0.0
http://hackage.haskell.org/package/d3js

MIT license

---

A fork has been necessary since I was unable to install with Stack and has not been active in two years.
Certainly [d3.js](https://github.com/mbostock/d3) has moved on since then and this library may no longer be accurate.
My goal (despite **very limited** knowledge of Haskell) is to pick up the baton and writen a working library.

In Clojure such a interoperating library [Strokes](https://github.com/dribnet/strokes) has already been achieve and looks well-maintained.  One may look at the nice [discussion](https://news.ycombinator.com/item?id=11210177) there.

[EDSL](https://wiki.haskell.org/Embedded_domain_specific_language) stands for **E** mbedded **D** omain **S** pecific **L** anguage - in this case we are embedding d3.js into Haskell

    hs-d3js: d3.js --> Haskell
    
This is not embedding of JavaScript into Haskell.  Then - in the contravariant way - we can compile Haskell into a subset of JavaScript that is well-formulated d3.js

    compile: Haskell --> d3.js

Wouldn't it be wonderful if we could represent the d3.js primitives themselves as Haskell objects?  Then we could reason about them!

From one of the comments:

> This modules provides high-level functions for drawing common charts, such as bar charts and scatter plots.
  Those functions also exemplify how to compose primitive functions to achieve complex drawing.
  This module will be expanded in the near future.

It never got expanded.  Now we can add in the low-level functions in a safe, functionl way.
