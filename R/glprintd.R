#
clean_cs <- function(x){
  val <- sapply(x, function(xt){
    z <- strsplit(paste(xt, collapse="\t"), "\t")[[1]]
    switch(z[1],
           "lapply" = z[3],
           "sapply" = z[3],
           "do.call" = z[2],
           "function" = "FUN",
           "source" = "###",
           "eval.with.vis" = "###",
           z[1]
    )
  })
  val[grepl("\\<function\\>", val)] <- "FUN"
  val <- val[!grepl("(###|FUN)", val)]
  val <- head(val, -1)
  paste(val, collapse="|")
}

catw <- function(..., callstack=sys.calls()){
  cs <- callstack
  # csClean <- clean_cs(cs)
  #browser()
  # message(paste(cs, ...))
  # paste(csClean, ...)
  # paste(cs, ...)
  # cs <- c("megaPrintTest()",  "ttt %>% glprv()",  "glprv(.)",         "catw() %>% print", "print(.)"        ,"catw()")
  csClean <- cs[!grepl('glprintd', cs) & !grepl('catw', cs) & !grepl('print', cs)] %>% paste(collapse = '|')
  paste(csClean, ...)
}
catw()


glprintd <- function(x) {
  catwResult <- catw()
  # catwResult %>% glprint
  if(catwResult == '') {
    print(x)
  }
  invisible(x)
}
ttt <- 'TTT'
megaPrintTest <- function() {

  ttt %>% glprintd()
  # fNam <- catw()
  # fNam %>% glprint
  invisible(1)
}
megaPrintTest()
