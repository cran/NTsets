.onLoad <- function(...) {
  ## use .onLoad rather than .First.lib when there is namespace
  #cat("\nUse 'NT()' to restart the programe.\n",fill=TRUE)
  if (interactive()) NT()
}

 


