if (getRversion() >= "3.1.0") {
  utils::globalVariables(c("simName", "objectName", "saveTime"))
}

################################################################################
#' Run experiment, algorithm 2, using [SpaDES.core::spades()]
#'
#' Given one or more `simList` objects, run a series of `spades` calls
#' in a structured, organized way. Methods are available to deal with outputs,
#' such as `as.data.table.simLists` which can pull out simple to complex
#' values from every resulting `simList` or object saved by `outputs`
#' in every `simList` run. This uses `future` internally, allowing
#' for various backends and parallelism.0
#'
#' @param ... One or more `simList` objects
#' @param replicates The number of replicates to run of the same `simList`.
#'                   See details and examples. To minimize memory overhead, currently,
#'                   this must be length 1, i.e., all `...` simList objects will
#'                   receive the same number of replicates.
#'
#' @param clearSimEnv Logical. If TRUE, then the envir(sim) of each simList in the return list
#'                    is emptied. This is to reduce RAM load of large return object.
#'                    Default FALSE.
#' @param createUniquePaths A character vector of the `paths` passed to `simInit`,
#'   indicating which should create a new, unique path, as a sub-path to the original
#'   `paths` of `simInit`. Default, and currently only option, is `"outputPath"`
#'
#' @param useCache Logical. Passed to `spades`. This will be passed with the `simList`
#'   name and replicate number, allowing each replicate and each `simList` to be
#'   seen as a non-cached call to `spades`. This will, however, may prevent `spades`
#'   calls from running a second time during second call to the same
#'   `experiment2` function.
#' @param drive_auth_account Optional character string. If provided, it will be passed
#'    to each worker and run as `googledrive::drive_auth(drive_auth_account)` to allow
#'    a specific user account for googledrive
#' @param meanStaggerIntervalInSecs If used, this will use
#'   `Sys.sleep(cumsum(c(0, rnorm(nbrOfWorkers() - 1, mean = meanStaggerIntervalInSecs,`
#'   `sd = meanStaggerIntervalInSecs/10))))` and distribute these delays to the workers.
#'
#' @inheritParams SpaDES.core::spades
#'
#' @details
#'
#' This function, because of its class formalism, allows for methods to be used. For example,
#' [as.data.table.simLists()] allows user to pull out specific objects (in
#' the `simList` objects or on disk saved in `outputPath(sim)`).
#'
#' The `outputPath` is changed so that every simulation puts outputs in a
#' sub-directory
#' of the original `outputPath` of each `simList`.
#'
#' @note
#' A `simLists` object can be made manually, if, say, many manual `spades` calls
#' have already been run. See example, via `new("simLists")`
#'
#' @return Invisibly returns a `simLists` object. This class
#' extends the `environment` class and
#' contains `simList` objects.
#'
#' @seealso [as.data.table.simLists()],
#'   [simInit()], [SpaDES.core::spades()], [experiment()]
#'
#' @author Eliot McIntire
#' @export
#' @rdname experiment2
#'
#' @example inst/examples/example_experiment2.R
#'
setGeneric(
  "experiment2",
  signature = "...",
  function(..., replicates = 1, clearSimEnv = FALSE,
           createUniquePaths = c("outputPath"), useCache = FALSE,
           debug = getOption("spades.debug"), drive_auth_account,
           meanStaggerIntervalInSecs) {
    standardGeneric("experiment2")
})

#' @importFrom future.apply future_lapply future_mapply
#' @importFrom future nbrOfWorkers
#' @importFrom googledrive drive_auth
#' @importFrom SpaDES.core packages
#' @rdname experiment2
setMethod(
  "experiment2",
  signature("simList"),
  definition = function(..., replicates, clearSimEnv,
                        createUniquePaths = c("outputPath"),
                        useCache = FALSE, debug = getOption("spades.debug"),
                        drive_auth_account = NULL, meanStaggerIntervalInSecs = 1) {
    # determine packages to load in the workers
    if (any(createUniquePaths != "outputPath")) {
      message("createUniquePaths only accepts outputPath, currently",
              ". Setting it to 'outputPath'")
      createUniquePaths <- "outputPath"
    }
    pkg <- c(unique(unlist(lapply(list(...), packages, clean = TRUE))),
             "SpaDES.experiment", "googledrive")
    outSimLists <- new("simLists")
    ll <- list(...)
    possSimNames <- as.character(seq_along(list(...)))

    ll <- updateNames(ll, possSimNames)
    simNames <- names(ll)

    # names(ll) <- simNames
    if (length(replicates) != 1) {
      stop("replicates argument must be length 1")
    }

    if (!missing(replicates)) {
      simNames <- rep(simNames, times = replicates) # keep them alternating for mapply
      repNums <- unlist(lapply(replicates, seq_len))
      repNums <- rep(repNums, each = length(ll))
      namsExpanded <- paste(simNames, paddedFloatToChar(repNums, padL = max(nchar(repNums))),
                     sep = "_rep")
      names(simNames) <- namsExpanded

    } else {
      namsExpanded <- simNames
    }

    # do copy of sim inside workers, so there is only 1 copy per worker,
    # rather than 1 copy per sim
    names(namsExpanded) <- namsExpanded

    staggersInSecs <- cumsum(c(0, rnorm(length(nbrOfWorkers())-1, mean = meanStaggerIntervalInSecs,
                              sd = meanStaggerIntervalInSecs/10)))

    out <- future_mapply(
      name = namsExpanded,
      simName = simNames,
      staggerInSecs = staggersInSecs,
      sim = ll,  # recycled by replicates -- maybe this reduces copying ...?
      MoreArgs = list(clearSimEnv = clearSimEnv,
                      createUniquePaths = createUniquePaths,
                      useCache = useCache,
                      .spades = .spades,
                      debug = debug,
                      drive_auth_account = drive_auth_account),
      FUN = experiment2Inner,
      SIMPLIFY = FALSE,
      future.packages = pkg,
      future.seed = TRUE
    )
    names(out) <- namsExpanded
    list2env(out, envir = outSimLists@.xData)
    return(outSimLists)
})

#' @importFrom SpaDES.core outputPath outputPath<- envir
#' @importFrom reproducible Cache
experiment2Inner <- function(sim, clearSimEnv, staggerInSecs, createUniquePaths,
                             simName, name, useCache = FALSE,
                             debug = getOption("spades.debug"), drive_auth_account,
                             ...) {
  message(paste0("Sleeping ", round(staggerInSecs, 1), " seconds"))
  Sys.sleep(staggerInSecs)
  outputPath(sim) <- checkPath(file.path(outputPath(sim), name),
                                   create = TRUE)
  if (!is.null(drive_auth_account))
    drive_auth(drive_auth_account)

  s <- Cache(.spades, sim, useCache = useCache, simName,
             debug = debug, clearSimEnv = clearSimEnv, ..., omitArgs = "debug")
  s
}

#' @importFrom future plan
#' @importFrom reproducible Copy
.spades <- function(sim, debug = getOption("spades.debug"),
                    clearSimEnv = FALSE, ...) {
  # don't make a copy if it is callr or multisession because future will make the copy
  if (!any(c("callr", "multisession") %in% attr(plan(), "class"))) {
    a <- Sys.time()
    message("Copying simList prior to spades call")
    sim <- Copy(sim, filebackedDir = file.path(outputPath(sim), "rasterFiles"))
    b <- Sys.time()
    message(format(b - a), " to Copy")
  }
  s <- spades(sim, debug = debug, ...)
  if (isTRUE(clearSimEnv))
    rm(list = ls(s, all.names = TRUE), envir = envir(s))
  return(s)
}
