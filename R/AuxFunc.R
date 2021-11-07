pacman::p_load(tictoc, tidyverse, data.table)
require(tictoc)
require(tidyverse)
require(data.table)
# pacman::p_load_gh('dplyrndas/sourcens')

verbose = T

# Sys.setenv(TZ='America/Los_Angeles')
# Sys.setenv(TZ='PDT') - doesn't work

printmess <- function(x, text = NULL) {
  if(!is.null(text))
    print(text)

  x
}

glprint <- function(x) {
  x %>% print
  invisible(x)
}

gldim <- function(x) {
  x %>% dim %>% print
  invisible(x)
}

glprintv <- function(x, isVerbose=F) {
  if(isVerbose)
    x %>% print
  invisible(x)
}

gldimv <- function(x, isVerbose=F) {
  if(isVerbose)
    x %>% dim %>% print
  invisible(x)
}

glimpsev <- function(x, isVerbose=F) {
  if(isVerbose)
    glimpse(x)
  x
}

naX <- function(df, X) {
  df[is.na(df)] <- X
  df
}

na0 <- function(df) { x %>% naX(0) }

maxNna <- function(x) { max(x, na.rm = T) }

wtictoc <- function(x) {
  tic()
  res <- x
  toc()
  res
}
