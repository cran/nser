## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----example------------------------------------------------------------------
# Live Nifty 50 data
library(nser)
nselive()

# Live F&O data 
library(nser)
nselive("fo")


