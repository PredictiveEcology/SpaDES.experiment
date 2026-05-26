################################################################################
#' Run experiment, algorithm 2 (moved to SpaDES.project)
#'
#' @description
#' **Deprecated** -- moved to SpaDES.project.
#'
#' `experiment2()` has moved to the \pkg{SpaDES.project} package, which is now
#' its maintained home. The version here is a thin shim: it emits a deprecation
#' message and forwards to [SpaDES.project::experiment2()] when that package is
#' installed. Please update your code to call `SpaDES.project::experiment2()`
#' directly.
#'
#' @param ... One or more `simList` objects (and any named arguments to forward
#'   to `spades()`, e.g. `events`). See [SpaDES.project::experiment2()].
#' @param replicates,clearSimEnv,createUniquePaths,useCache,debug,drive_auth_account,meanStaggerIntervalInSecs
#'   See [SpaDES.project::experiment2()].
#'
#' @return See [SpaDES.project::experiment2()].
#'
#' @seealso [SpaDES.project::experiment2()], [SpaDES.project::experiment()]
#' @author Eliot McIntire
#' @export
#' @rdname experiment2
setGeneric(
  "experiment2",
  signature = "...",
  function(..., replicates = 1, clearSimEnv = FALSE,
           createUniquePaths = c("outputPath"), useCache = FALSE,
           debug = getOption("spades.debug"), drive_auth_account,
           meanStaggerIntervalInSecs) {
    standardGeneric("experiment2")
})

#' @rdname experiment2
setMethod(
  "experiment2",
  signature("simList"),
  definition = function(..., replicates = 1, clearSimEnv = FALSE,
                        createUniquePaths = c("outputPath"),
                        useCache = FALSE, debug = getOption("spades.debug"),
                        drive_auth_account = NULL, meanStaggerIntervalInSecs = 1) {
    args <- c(list(...),
              list(replicates = replicates, clearSimEnv = clearSimEnv,
                   createUniquePaths = createUniquePaths, useCache = useCache,
                   debug = debug, drive_auth_account = drive_auth_account,
                   meanStaggerIntervalInSecs = meanStaggerIntervalInSecs))
    .forwardToSpaDESproject("experiment2", args)
})
