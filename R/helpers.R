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

#' @keywords internal
.objNamesBySimList <- function(simLists) {
  objs <- ls(simLists)
  simLists <- gsub("_.*", "", objs)
  simListsBySimList <- split(objs, f = simLists)
  simListsBySimList <- lapply(simListsBySimList, sort)
}
