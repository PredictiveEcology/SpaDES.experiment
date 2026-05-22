#' Run `simInit` and `experiment` in one step (moved to SpaDES.project)
#'
#' @description
#' **Deprecated** -- moved to SpaDES.project.
#'
#' `simInitAndExperiment()` has moved to the \pkg{SpaDES.project} package, which
#' is now its maintained home. The version here is a thin shim: it emits a
#' deprecation message and forwards to [SpaDES.project::simInitAndExperiment()]
#' when that package is installed. Please update your code to call
#' `SpaDES.project::simInitAndExperiment()` directly.
#'
#' @param times,params,modules,objects,paths,inputs,outputs,loadOrder,notOlderThan See [SpaDES.core::simInit()].
#' @param replicates,dirPrefix,substrLength,saveExperiment,experimentFile,clearSimEnv,cl See [SpaDES.project::experiment()].
#' @param ... Passed onward.
#'
#' @export
#' @aliases simInitAndExperiment
#' @rdname simInitAnd
simInitAndExperiment <- function(times, params, modules, objects, paths, inputs, outputs,
                                 loadOrder, notOlderThan, replicates,
                                 dirPrefix, substrLength, saveExperiment,
                                 experimentFile, clearSimEnv, cl, ...)  {
  args <- list(...)
  forNm <- c("times", "params", "modules", "objects", "paths", "inputs", "outputs",
             "loadOrder", "notOlderThan", "replicates", "dirPrefix", "substrLength",
             "saveExperiment", "experimentFile", "clearSimEnv", "cl")
  for (nm in forNm) {
    if (!eval(bquote(missing(.(as.name(nm))))))
      args[[nm]] <- get(nm, inherits = FALSE)
  }
  .forwardToSpaDESproject("simInitAndExperiment", args)
}
