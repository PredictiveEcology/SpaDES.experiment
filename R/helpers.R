#' @keywords internal
updateNames <- function(lst, newNames) {
  namesVals <- names(lst)
  emptyChar <- nchar(namesVals) == 0
  if (is.null(namesVals) || any(emptyChar)) {
    if (missing(newNames)) {
      newNames <- unlist(lapply(seq_along(lst), function(x) {
        if (is.name(lst[[x]])) {
          paste(collapse = "_", format(as.character(lst[[x]])))
        } else {
          paste(collapse = "_", format(lst[[x]]))
        }
      }))
    }
    if (any(emptyChar)) {
      namesVals[emptyChar] <- newNames[emptyChar]
      newNames <- namesVals
    }
    names(lst) <- newNames
  }
  lst
}

#' Forward a deprecated SpaDES.experiment call to SpaDES.project
#'
#' Emits a deprecation message and re-dispatches to the identically-named
#' function now living in \pkg{SpaDES.project}.
#'
#' @param fnName Character, the function name (e.g. `"experiment"`).
#' @param args A named list of the supplied arguments to forward.
#' @keywords internal
#' @noRd
.forwardToSpaDESproject <- function(fnName, args) {
  if (!requireNamespace("SpaDES.project", quietly = TRUE)) {
    stop(sprintf(
      paste0("`%s()` has moved to the SpaDES.project package and is no longer ",
             "maintained here. Install SpaDES.project and use `SpaDES.project::%s()`:\n",
             "  remotes::install_github('PredictiveEcology/SpaDES.project')"),
      fnName, fnName), call. = FALSE)
  }
  .Deprecated(
    new = sprintf("SpaDES.project::%s", fnName),
    package = "SpaDES.experiment",
    msg = sprintf(
      paste0("`SpaDES.experiment::%s()` is deprecated; it now lives in ",
             "SpaDES.project. Forwarding to `SpaDES.project::%s()`. ",
             "Please update your code to call SpaDES.project directly."),
      fnName, fnName))
  do.call(get(fnName, envir = asNamespace("SpaDES.project")), args)
}

#' @keywords internal
.objNamesBySimList <- function(simLists) {
  objs <- ls(simLists)
  simLists <- gsub("_.*", "", objs)
  simListsBySimList <- split(objs, f = simLists)
  simListsBySimList <- lapply(simListsBySimList, sort)
}
