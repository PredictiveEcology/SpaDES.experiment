################################################################################
#' The \code{simLists} class
#'
#' This is a grouping of \code{simList} objects
#'
#' @note The \code{simList} class extends the \code{environment}, by adding
#' several slots that provide information about the metadata for a discrete
#' event simulation. The environment slot, if accessed directly is \code{.xData}
#' and this is where input and output objects from modules are placed.
#' The \code{\link{simList_}} class is similar, but it extends the \code{list}
#' class. All other slots are the same.
#' Thus, \code{simList} is identical to \code{simList_}, except that the former
#' uses an environment for objects and the latter uses a list.
#' The class \code{simList_} is only used internally.
#'
#' @slot paths      Named list of \code{modulePath}, \code{inputPath},
#'                  and \code{outputPath} paths. Partial matching is performed. These
#'                  will be prepended to the relative paths of each \code{simList}
#' @slot .xData  Environment holding the \code{simLists}.
#'
#' @section Accessor Methods:
#'
#' None yet defined:
#' \tabular{ll}{
#'   \code{\link{simList-accessors-envir}} \tab Simulation environment. \cr
#' }
#'
#'
#' @aliases simLists
#' @rdname simLists-class
#' @rdname simLists
#' @importFrom data.table as.data.table data.table
#'
#' @author Eliot McIntire
#' @exportClass simLists
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

#' Generate a \code{simLists} object
#'
#' Given the name or the definition of a class, plus optionally data to be
#' included in the object, \code{new} returns an object from that class.
#'
#' @export
#' @importFrom SpaDES.core .paths
#' @param .Object  A \code{simList} object.
#' @param ... Optional Values passed to any or all slot
#' @rdname initialize-method
setMethod("initialize",
          signature(.Object = "simLists"),
          definition = function(.Object, ...) {

            .Object@paths = .paths()

            .Object@.xData <- new.env(parent = emptyenv())

            attr(.Object@.xData, "name") <- "simLists"
            return(.Object)
          })

#' Show method for \code{simLists}
#' @param object  \code{simLists}
#'
#' @author Eliot McIntire
#' @include simLists-class.R
#' @importFrom utils capture.output ls.str tail
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
    } else {
      paste("with", paste(uniqueLengths, collapse = ", "), "replicates respectively")
    }
    out[[2]] <- capture.output(cat(out2, out3))
    ll <- lapply(simListsBySimList, function(s) {
      paste0(s[1], ", ..., ", tail(s,1))
    })
    simListChStr <- paste0(names(ll), ": ", ll)
    simListEntries <- (seq_along(unique(simLists))-1)*2 + length(out) + 1
    out[simListEntries] <- lapply(simListChStr, function(x) x)
    out[simListEntries + 1] <- lapply(simListsBySimList, function(x) {
      paste("  ", capture.output(ls.str(object[[x[1]]])))
    })

    out[[length(out) + 1]] <- capture.output(cat("\n"))
    ### print result
    cat(unlist(out), fill = FALSE, sep = "\n")
  })

