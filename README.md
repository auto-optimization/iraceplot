# The iraceplot package

<!-- badges: start -->
[![CRAN
Status](https://www.r-pkg.org/badges/version-last-release/iraceplot)](https://cran.r-project.org/package=iraceplot)
[![R-CMD-check](https://github.com/auto-optimization/iraceplot/workflows/R-CMD-check/badge.svg)](https://github.com/auto-optimization/iraceplot/actions)
[![Codecov test coverage](https://codecov.io/gh/auto-optimization/iraceplot/branch/master/graph/badge.svg)](https://app.codecov.io/gh/auto-optimization/iraceplot?branch=master)
[![r-universe version](https://auto-optimization.r-universe.dev/badges/iraceplot)](https://auto-optimization.r-universe.dev/iraceplot)
<!-- badges: end -->

**Maintainers:** Leslie Pérez Cáceres, [Manuel López-Ibáñez](https://lopez-ibanez.eu)

**Creators:** Pablo Oñate Marín, Leslie Pérez Cáceres, [Manuel López-Ibáñez](https://lopez-ibanez.eu)

**Contact:** <https://groups.google.com/d/forum/irace-package>

---------------------------------------

Introduction
============

The iraceplot package provides different plots to visualize
the data generated by the [irace](https://cran.r-project.org/package=irace) software for automatic algorithm configuration (hyper-parameter optimization).

This package provides visualizations of:

- Parameter configurations using parallel coordinates with  `parallel_coord()`.
- Boxplots of configurations performance (training and testing) with `boxplot_training()` and `boxplot_test()`.
- Sampling distributions with `sampling_frequency()`.
- Overview of all performance data generate by a single run of irace with `plot_experiments_matrix()`.

For more details about these functions, please check the [user guide](https://auto-optimization.github.io/iraceplot/) 
of the package and the documentation of the functions implemented in the package.

The package also provides an  [HTML report](https://auto-optimization.github.io/iraceplot/articles/example/report_example.html), using `report()`, summarizing relevant information obtained during an execution of irace.

The aim of this package is to provide support for the analysis of the best parameter settings found, the assessment of the parameter space explored by irace and the overall performance of the configuration process. Such analysis might lead to insights about the role of algorithmic components their interactions, or to improve the configuration process itself.


**Keywords:** automatic configuration, offline tuning, parameter tuning, parameter visualization, irace.

Requisites
----------

 * R (<https://www.r-project.org>) is required for running irace and to use iraceplot.

User guide
----------

A [user guide](https://auto-optimization.github.io/iraceplot/articles/user_guide/guide.html)
comes with the package. The following is a quick-start guide. The user guide gives more detailed
instructions.


Installing R
============

The official instructions are available at
<https://cran.r-project.org/doc/manuals/r-release/R-admin.html>. We give below
a quick R installation guide that will work in most cases.

GNU/Linux
---------

You should install R from your package manager. On a Debian/Ubuntu system it
will be something like:

    $ sudo apt-get install r-base

Once R is installed, you can launch R from the Terminal and from the R
prompt install the iraceplot package. See instructions below.


OS X
----

You can install R directly from a CRAN mirror
(<https://cran.r-project.org/bin/macosx/>).

Alternatively, if you use homebrew, you can just do
```
    $ brew install --cask r
```

(Using `brew install r` is not recommended because that will build R from source and you will not be able to use any CRAN binary, possibly resulting in annoying build failures). 

Once R is installed, you can launch R from the Terminal (or from your
Applications), and from the R prompt install the iraceplot package. See
instructions below.

Windows
-------

You can install R from a CRAN mirror
(<https://cran.r-project.org/bin/windows/>). Once R is installed, you can
launch the R console and install the iraceplot package from it. See instructions
below.



Installing the iraceplot package
============================

Stable version
--------------

For installing the [stable version from
CRAN](https://cran.r-project.org/package=iraceplot), launch R or Rstudio and
evaluate:
``` r
install.packages("iraceplot")
```

Or you may wish to try the [development version from GitHub](https://github.com/auto-optimization/iraceplot) 

GitHub (Development version)
---------------------------

If you wish to try the development version, you can install it by executing the
following command within the R console:

```r
    install.packages('iraceplot', repos = c('https://auto-optimization.r-universe.dev', 'https://cloud.r-project.org'))
```


Basic Usage
===========

Load the package in the R console:

```r
library(iraceplot)
```

You need the log file generated by irace (`irace.Rdata` or the filename given by the option `logFile` of irace). Then, generate a general-purpose report with:

```r
report("irace.Rdata")
```

This should create a filename `report.html` and open it in your browser (if the browser does not open, try to find the file and open it yourself). The result will look something like this [report example](https://auto-optimization.github.io/iraceplot/articles/example/report_example.html).

There is a lot more functionality in this package. 
Check the [documentation](https://auto-optimization.github.io/iraceplot/reference/index.html) and the [User Guide](https://auto-optimization.github.io/iraceplot/articles/user_guide/guide.html) to identify the plots most suited to your needs.
