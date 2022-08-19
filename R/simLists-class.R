#' The `simLists` class
#'
#' This is a grouping of `simList` objects. Normally this class will be
#' made using `experiment2`, but can be made manually if there are
#' existing `simList` objects.
#'
#' @slot paths      Named list of `modulePath`, `inputPath`,
#'                  and `outputPath` paths. Partial matching is performed. These
#'                  will be prepended to the relative paths of each `simList`
#' @slot .xData  Environment holding the `simLists`.
#'
#' @section Accessor Methods:
#'
#' None yet defined:
#' \tabular{ll}{
#'   [simList-accessors-envir()] \tab Simulation environment. \cr
#' }
#'
#'
#' @aliases simLists
#' @author Eliot McIntire
#' @exportClass simLists
#' @importFrom data.table as.data.table data.table
#' @include helpers.R
#' @rdname simLists-class
#'
setClass(
  "simLists",
  contains = "environment",
  slots = list(
    .xData = "environment", paths = "list"
  ),
  validity = function(object) {
    return(object)
  }
)

#' Generate a `simLists` object
#'
#' Given the name or the definition of a class, plus optionally data to be
#' included in the object, `new` returns an object from that class.
#'
#' @param .Object  A `simList` object.
#' @param ... Optional Values passed to any or all slot
#'
#' @export
#' @importFrom SpaDES.core .paths
#' @rdname initialize-method
setMethod("initialize",
          signature(.Object = "simLists"),
          definition = function(.Object, ...) {

            .Object@paths = .paths()

            .Object@.xData <- new.env(parent = emptyenv())

            attr(.Object@.xData, "name") <- "simLists"
            return(.Object)
})

#' Show method for `simLists`
#' @param object  `simLists`
#'
#' @author Eliot McIntire
#' @importFrom utils capture.output ls.str tail
#' @include simLists-class.R
#' @export
setMethod(
  "show",
  signature = "simLists",
  definition = function(object) {
    out <- list()
    out[[1]] <- capture.output(
      cat(rep("=", getOption("width"), sep = ""), "\n", sep = "")
    )

    simListsBySimList <- .objNamesBySimList(object)
    simLists <- unlist(simListsBySimList)
    simLists <- gsub("_.*", "", simLists)

    lengths <- lapply(simListsBySimList, length)
    uniqueLengths <- unique(unlist(lengths))
    out2 <- paste(">> ",length(unique(simLists)),"simLists;")
    out3 <- if (length(uniqueLengths) == 1) {
      paste("with", uniqueLengths, "replicates each")
    } else if (isTRUE(uniqueLengths) == 1) {
      paste0("with only 1 replicate each")
    } #else {
      #paste("with", paste(uniqueLengths, collapse = ", "), "replicates respectively")
    #}
    out[[2]] <- capture.output(cat(out2, out3))
    ll <- lapply(simListsBySimList, function(s) {
      paste0(s[1], ", ..., ", tail(s,1))
    })
    simListChStr <- paste0(names(ll), ": ", ll)
    simListEntries <- (seq_along(unique(simLists)) - 1)*2 + length(out) + 1
    out[simListEntries] <- lapply(simListChStr, function(x) x)
    out[simListEntries + 1] <- lapply(simListsBySimList, function(x) {
      paste("  ", capture.output(ls.str(object[[x[1]]])))
    })

    out[[length(out) + 1]] <- capture.output(cat("\n"))
    ### print result
    cat(unlist(out), fill = FALSE, sep = "\n")
})
