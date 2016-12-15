[![Travis-CI Build Status](https://travis-ci.org/dtkaplan/checkr.svg?branch=master)](https://travis-ci.org/dtkaplan/checkr)

# checkr: Checking student answers in an R-tutorial system

An R-tutorial system provides facilities for posing R-related questions to students, collecting the code that students write in reply, and providing hints as needed. Examples for R-tutorial systems are [DataCamp.com](DataCamp.com) and the `tutor` package from RStudio.

The `checkr` package supports a framework for the authors of tutorials to specify what is a correct answer, and to give formative feedback when answers fail to meet that specification. Functions are provided to enable `checkr` to be connected to `tutor` documents. 

`tutor` creates stand-alone `Shiny` apps, so using `checkr` with `tutor` enables any instructor to write and deploy tutorials.  

[DataCamp.com](DataCamp.com) provides its own excellent checking system, called [checkwhat](https://github.com/datacamp/testwhat) with a substantially different framework than `checkr`. DataCamp allows instructors to create and deploy free tutorials on the DataCamp servers. The DataCamp system is much more mature than `checkr` and has a very large base of users. 

You can install the latest version of this package from GitHub

```r
devtools::install_github("dtkaplan/checkr")
```

See the package vignette for details and examples about using `checkr`.
