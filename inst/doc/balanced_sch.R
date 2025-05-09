## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(days2lessons)

## -----------------------------------------------------------------------------
str(LSS)
str(TPL)

## ----eval = FALSE-------------------------------------------------------------
#  prnTime <- function(S = "")  # va afișa timpul curent
#      cat(strftime(Sys.time(), format="%H:%M:%S"), S)
#  for(i in 1:2) {
#      prnTime(" ")
#      R123 <- mount_days(LSS, TPL)  # Dfh = 2 (valoarea implicită)
#      prnTime("\n")
#      addmargins(table(R123[c('cls','zl')]))["Sum", 1:5] %>%
#          as.vector() %>% print()
#  }

