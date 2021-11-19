pacman::p_load(qs, fst, tictoc, tidyverse, data.table)
# usethis::use_package("fst") # Defaults to imports
# usethis::use_package("pacman") # Defaults to imports

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


formatPercNum <- function(x) round(x, 3) * 100
formatPercStr <- function(x) paste0(round(x, 3) * 100, '%')
objSize <- function(x) paste(as.numeric(object.size(x) / 1024^3) %>% round(2), 'Gbytes')


# -------- BINOM---
testBinom <- function() {
  value = 35
  groupSize = 51
  binomValueFormat(5, 100)
}

binomSd <- function(value, groupSize) {
  p = value / groupSize
  p

  variance = p * (1-p) / groupSize
  variance #%>% print

  sdValue = sqrt(groupSize * p * (1-p))
  sdValue
}

binomValueSdFormat <- function(value, groupSize) {
  sdValue = binomSd(value, groupSize)
  sdValue

  paste0(value, '±', sdValue)
}


sfTime <- function() {
  pb.date <- as.POSIXct(.POSIXct(Sys.time(), "GMT"),tz="GMT")
  # convert it to PDT time zone
  format(pb.date, tz="America/Los_Angeles",usetz=TRUE)
}
sfDate <- function() {
  sfTime() %>% as.Date()
}
# print(paste0('CUR TIME(PACIFIC): ', sfTime()))
#

# ---------------------SQL func---
getSqlResultCacheFst <- function(conn, cacheFile, sqlQuery, lowercaseCol = T) {
  if(!file.exists(cacheFile)) {
    # source('../_common/connect_snowflake.R')
    tic()
    print('Downloading from db...')
    fishCaughtTimeDat <- dbGetQuery(conn,sqlQuery) %>% data.table
    toc()
    if(lowercaseCol) {
      colns <- colnames(fishCaughtTimeDat)
      fishCaughtTimeDat %>% setnames(colns %>% tolower)
    }
    fishCaughtTimeDat %>% glimpse
    write.fst(fishCaughtTimeDat, cacheFile)
    toc()
  } else {
    fishCaughtTimeDat <- read.fst(cacheFile) %>% data.table %>% glimpse
  }
  objSize(fishCaughtTimeDat) %>% print
  fishCaughtTimeDat
}



# -----
tablec <- function(x) {
  # vec <- c(1, 1, 1, 2, 2, 3)
  colnam <- substitute(x) %>% deparse# %>% glprint
  dtt <- data.table()[, eval(colnam) := x][, .(N = .N), by=colnam][order(-N)]
  # dtt[, colnam] <- x
  # %>% print
  # dtt <- data.table(colnam = x)[, .(N = .N), by=colnam][order(-N)]# %>% rename(colnam = 'colnam')
  # !!sym(x) %>% print
  dtt
}

# dt <- data.table(x1 = sqrt(1:9) %>% round) %>% mutate(x2 = rev(x1))
tabledt <- function(dt, ...) {
  lsArg <- match.call()
  argsNames <- lapply(lsArg, function(x) as.character(x) %>% unlist) %>% unlist
  namesOfArgsNames <- names(argsNames)
  colNames <- argsNames[argsNames != 'tabledt' & namesOfArgsNames == '']
  dt %>% data.table %>% .[, .(N = .N), by=colNames] %>% .[order(-N)] %>%
  # dt %>% data.table %>% .[, .(N = .N), by=colNames] %>% .[order(-N, get(colNames))] %>%
    mutate(Nperc = formatPercNum(N / sum(N)), NpercStr = formatPercStr(N / sum(N)))
  # dt[, .(N = .N), by=colnams]
}
# dt %>% tabledt(x1, x2)
