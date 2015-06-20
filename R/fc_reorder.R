#' Reorder clusters in a kcca object.
#'
#' Since running kcca with different seeds will result, at least, in equivalent
#' clusters having different cluster sequence numbers which makes interpretation
#' of repeated runs difficult.
#'
#' \code{fc_reorder} simply rearranges the clusters within the kcca object according
#' to the requested method.
#'
#' @param x A kcca object.
#' @param orderby A string. Specifying the method to order by. Currently only "decending size".
#' @return The kcca object with clusters reordered.
#' @examples
#' \dontrun{
#' fc_reorder(kcca(x, k, save.data = TRUE, control = fc_cont, family = kccaFamily(fc_family)))
#' }
fc_reorder <- function(x, orderby = "decending size") {
  ko <- x
  cl_map <- order(ko@clusinfo$size, decreasing = TRUE)
  ko@second <- cl_map[ko@second]
  ko@clsim <- ko@clsim[cl_map, cl_map]
  ko@centers <- ko@centers[cl_map, ]
  ko@cluster <- cl_map[ko@cluster]
  ko@clusinfo <- ko@clusinfo[cl_map, ]
  # ko@reorder <- cl_map                   add slot with reorder mapping
  return(ko)
}
