## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, include= FALSE----------------------------------------------------
library(iraceplot)

## ---- include= FALSE----------------------------------------------------------
load("../data/iraceResults.rda")

## -----------------------------------------------------------------------------
iboxplot_test(iraceResults, type = "last")

