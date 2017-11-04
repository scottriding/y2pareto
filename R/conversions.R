

#' Convert any vector into a factor.
#'
#' @param column A vector to convert into a factor.
#' @param levels A vector of levels to accept (optional). In the case of a
#' labelled object, this input is completely ignored since levels come from
#' the value labels defined in the labelled object.
#' @param labels A vector of labels for the levels (optional). Similarly ignored
#' in the case of a labelled object.
#' @param ordered A boolean indicating whether factor is ordered (optional). If
#' levels are specified, it uses the given ordering. If not, uses the order it
#' encounters values in the vector.
#' @return A native R factor, either unordered or ordered.
#' @export
factorize <- function(column, levels, labels, ordered = F) {

  if (is(column, 'labelled')) {
    factorizeLabelled(column,ordered)
  } else {
    factorizeNonLabelled(column, levels, labels, ordered)
  }
}

factorizeLabelled <- function(column, ordered = F) {
  labelled::to_factor(column, ordered = ordered)
}

factorizeNonLabelled <- function(column, levels, labels, ordered = F) {
  if (missing(levels)) {
    levels <- unique(column)
  }
  if (missing(labels)) {
    labels <- levels
  }
  factor(column, levels, labels, ordered = ordered)
}
