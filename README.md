# `scopr` [![Travis-CI Build Status](https://travis-ci.org/rethomics/scopr.svg?branch=master)](https://travis-ci.org/rethomics/scopr)[![Coverage Status](https://img.shields.io/codecov/c/github/rethomics/scopr/master.svg)](https://codecov.io/github/scopr/behavr?branch=master)

<!-- [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/tidyverse/hms?branch=master&svg=true)](https://ci.appveyor.com/project/tidyverse/hms)  -->

<!-- [![Coverage Status](https://img.shields.io/codecov/c/github/tidyverse/hms/master.svg)](https://codecov.io/github/tidyverse/hms?branch=master) [![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/hms)](https://cran.r-project.org/package=hms) -->

`scopr` is part of the [rethomics framework](todo.html).
This README is a short explanation of the basics of `scopr`.
A [comprehensive documentation](todo.html) of rethomics is also available.

## Ethocsope platform
Ethoscopes are an integrated platform to record behaviour of small animals and perform feed-back loop experiments. More inforation about them is available on the [ethoscope website](http://gilestrolab.github.io/ethoscope/)


## Installation


```r
library(devtools)
install_github("rethomics/behavr")
install_github("rethomics/scopr")
```


## Ethoscope results

Since ethoscopes is designed for high-throughput experiments, it is likely that you will run many machines with several animals in each. You will also run both concurrent and successive experiments.
You may also have to put together data acquiered in several labs.
In this context, it is crucial to be able to retreive each experiment unambiguisly.
Ethoscopes solves this problem by organising files in a canonical directory structure (see the [ethoscope user manual](https://qgeissmann.gitbooks.io/ethoscope-manual/content/administration-and-maintenance/backing-up-data.html)). 
`scopr` offers a transparent way, based on a **query system**, for users load their data without this underlying structure. This means each animal can be associeated with arbitrary metavariables and loaded for subsequent analysis.

## Let's build a query

TODO

## Using the query to load the data

TODO

### The raw data

TODO

### Align to circadian time

TODO

### Analyse data upon loading

TODO

## Listing available files

TODO

## Metadata

TODO

## Going further

* [behavr](https://github.com/rethomics/behavr) -- to manipulate the data (create new variable/meta-variables)
* [ggetho](https://github.com/rethomics/ggetho) -- to plot visualise the data
* [sleepr](https://github.com/rethomics/sleepr) -- to perform sleep and circadian rythm analysis

