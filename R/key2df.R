#' Key to data frame
#' 
#' Converts a scoring key to data frame (e.g., `key2df(xs5.key)`)
#'
#' @param x scoring key 
#'
#' @return
#' @export
#'
#' @examples
key2df <- function(x){
  k1 <- factor(x$key, levels=x$subscales)
  k2 <- factor(x$key2, levels=x$domains)
  lims <- x$lims[1:2]
  res <- data.frame(No = 1:length(k1), key=k1)
  if("key2" %in% names(x)) res$key2 <- k2
  if("rev" %in% names(x)) res$rev <- x$rev
  if("items" %in% names(x)) res$items <- x$items
  if("lims" %in% names(x)) {
    res$min <- x$lims[1]
    res$max <- x$lims[2]
  }
  res
}
