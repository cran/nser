## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----example------------------------------------------------------------------
# For Nifty 50 stocks
library(nser)
nseopen()

# For F&O stocks 
library(nser)
nseopen("fo")

