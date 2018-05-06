# QuikR [![build-status]][build-link] [![coverage-status]][coverage-link]

Common utility functions for use in R projects.


## Installation

QuikR currently can only be installed from GitHub using devtools.

``` r
# Get devtools package, if not present.
if(!require(devtools)) {
    install.packages("devtools");
}

# Use devtools to install the latest version from GitHub
devtools::install_github("rlonryan/quikr");
```

## Usage

QuikR can be used to easily diff tables, as to see the differences.

``` r
library(quikr);

# Diff mtcars against mtcars.
qdiff(mtcars, mtcars);
#> data frame with 0 columns and 0 rows

# Diff beaver1 against beaver2, and display using View() function.
vqdiff(beaver1, beaver2);
#> viewer showing all differences between beaver1 and bever2 on a cell-by-cell basis.
```

[build-link]:https://travis-ci.org/RlonRyan/QuikR
[build-status]:https://travis-ci.org/QuikMod/QuikR.svg?branch=master "Travis-CI Build Status"

[coverage-link]:https://codecov.io/github/rlonryan/quikr?branch=master
[coverage-status]:https://codecov.io/github/rlonryan/quikr/coverage.svg?branch=master
