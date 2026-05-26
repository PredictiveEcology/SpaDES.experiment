################################################################################
#' Run an experiment using [SpaDES.core::spades()] (moved to SpaDES.project)
#'
#' @description
#' **Deprecated** -- moved to SpaDES.project.
#'
#' `experiment()` has moved to the \pkg{SpaDES.project} package, which is now
#' its maintained home. The version here is a thin shim: it emits a deprecation
#' message and forwards to [SpaDES.project::experiment()] when that package is
#' installed. Please update your code to call `SpaDES.project::experiment()`
#' directly.
#'
#' @param sim A `simList`.
#' @param replicates Number of replicates. See [SpaDES.project::experiment()].
#' @param params,modules,objects,inputs Alternatives to vary. See
#'   [SpaDES.project::experiment()].
#' @param dirPrefix,substrLength,saveExperiment,experimentFile,clearSimEnv See
#'   [SpaDES.project::experiment()].
#' @param notOlderThan,cl Deprecated; see [SpaDES.project::experiment()].
#' @param ... Passed to [SpaDES.project::experiment()].
#'
#' @return See [SpaDES.project::experiment()].
#'
#' @seealso [SpaDES.project::experiment()], [SpaDES.project::experiment2()]
#' @author Eliot McIntire
#' @export
#' @importClassesFrom SpaDES.core simList
#' @rdname experiment
setGeneric(
  "experiment",
  function(sim, replicates = 1, params, modules, objects = list(), inputs,
           dirPrefix = "simNum", substrLength = 3, saveExperiment = TRUE,
           experimentFile = "experiment.RData", clearSimEnv = FALSE, notOlderThan,
           cl, ...) {
    standardGeneric("experiment")
})

#' @rdname experiment
#' @exportMethod experiment
setMethod(
  "experiment",
  signature(sim = "simList"),
  definition = function(sim, replicates = 1, params, modules, objects = list(), inputs,
                        dirPrefix = "simNum", substrLength = 3, saveExperiment = TRUE,
                        experimentFile = "experiment.RData", clearSimEnv = FALSE,
                        notOlderThan, cl, ...) {
    args <- list(sim = sim, replicates = replicates, objects = objects,
                 dirPrefix = dirPrefix, substrLength = substrLength,
                 saveExperiment = saveExperiment, experimentFile = experimentFile,
                 clearSimEnv = clearSimEnv, ...)
    if (!missing(params)) args$params <- params
    if (!missing(modules)) args$modules <- modules
    if (!missing(inputs)) args$inputs <- inputs
    if (!missing(notOlderThan)) args$notOlderThan <- notOlderThan
    if (!missing(cl)) args$cl <- cl
    .forwardToSpaDESproject("experiment", args)
})
