#' Convert a \code{nb} object to \code{data.frame}.
#'
#' Convert an object of class \code{nb} in package spdep or a \code{list} of
#' \code{nb} objects to \code{data.frame}.
#'
#' @param nb 	 an object of class \code{nb}
#' @param maxlag maximum lag, see \code{\link[spdep]{nblag}}
#'
#' @return \code{data.frame}
#' @keywords spatial
#' @export neighborsDataFrame_expp
neighborsDataFrame_expp <- function(nb) {
  stopifnot( inherits(nb, 'nb'))

  ks = data.frame(k = unlist( mapply(rep, 1:length(nb), sapply(nb, length), SIMPLIFY = FALSE) ), k_nb = unlist(nb) )

  nams = data.frame(id = attributes(nb)$region.id, k = 1:length(nb) )

  o = merge(ks, nams, by.x = 'k', by.y = 'k')
  o = merge(o, nams, by.x = 'k_nb', by.y = 'k', suffixes = c("","_neigh"))

  o[, c("id", "id_neigh")]

}

#' @rdname neighborsDataFrame_expp
#' @export
higherNeighborsDataFrame <- function(nb, maxlag) {

  stopifnot( inherits(nb, 'nb'))

  n = nblag(nb, maxlag = maxlag)

  names(n) = 1:maxlag
  n = n[sapply(n, function(x) !all ( unlist(x) == 0 ) )]

  new_maxlag = length(n)
  if(maxlag > new_maxlag)
    warning( paste('maxlag of ', maxlag, 'is exceeding the number of possible lags, reverting to', new_maxlag, 'lags.') )

  #remove 0-length neighbours
  n = lapply(n, function(g) subset( g, card(g) > 0 ) )

  n = lapply(n, function(a) neighborsDataFrame_expp(a)  )

  d = mapply(FUN = function(x, rank) cbind(x, rank), x = n, rank = 1:new_maxlag, SIMPLIFY = FALSE)
  d = do.call(rbind, d)
  d


}
