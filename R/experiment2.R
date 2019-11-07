if (getRversion() >= "3.1.0") {
  utils::globalVariables(c("simName", "objectName", "saveTime"))
}


################################################################################
#' Run experiment, algorithm 2, using \code{\link[SpaDES.core]{spades}}
#'
#' Given one or more \code{simList} objects, run a series of \code{spades} calls
#' in a structured, organized way. Methods are available to deal with outputs,
#' such as \code{as.data.table.simLists} which can pull out simple to complex
#' values from every resulting \code{simList} or object saved by \code{outputs}
#' in every \code{simList} run. This uses \code{future} internally, allowing
#' for various backends and parallelism.0
#'
#' @param ... One or more \code{simList} objects
#' @param replicates The number of replicates to run of the same \code{simList}.
#'                   See details and examples. To minimize memory overhead, currently,
#'                   this must be length 1, i.e., all \code{...} simList objects will
#'                   receive the same number of replicates.
#'
#' @param clearSimEnv Logical. If TRUE, then the envir(sim) of each simList in the return list
#'                    is emptied. This is to reduce RAM load of large return object.
#'                    Default FALSE.
#' @param createUniquePaths A character vector of the \code{paths} passed to \code{simInit},
#'   indicating which should create a new, unique path, as a sub-path to the original
#'   \code{paths} of \code{simInit}. Default, and currently only option, is \code{"outputPath"}
#'
#' @param useCache Logical. Passed to \code{spades}. This will be passed with the \code{simList}
#'   name and replicate number, allowing each replicate and each \code{simList} to be
#'   seen as a non-cached call to \code{spades}. This will, however, may prevent \code{spades}
#'   calls from running a second time during second call to the same
#'   \code{experiment2} function.
#'  @param drive_auth_account Optional character string. If provided, it will be passed
#'    to each worker and run as \code{googledrive::drive_auth(drive_auth_account)} to allow
#'    a specific user account for googledrive
#' @inheritParams SpaDES.core::spades
#'
#' @details
#'
#' This function, because of its class formalism, allows for methods to be used. For example,
#' \code{\link{as.data.table.simLists}} allows user to pull out specific objects (in
#' the \code{simList} objects or on disk saved in \code{outputPath(sim)}).
#'
#' The \code{outputPath} is changed so that every simulation puts outputs in a
#' sub-directory
#' of the original \code{outputPath} of each \code{simList}.
#'
#' @note
#' A \code{simLists} object can be made manually, if, say, many manual \code{spades} calls
#' have already been run. See example, via \code{new("simLists")}
#'
#' @return Invisibly returns a \code{simLists} object. This class
#' extends the \code{environment} class and
#' contains \code{simList} objects.
#'
#' @seealso \code{\link{as.data.table.simLists}},
#'   \code{\link{simInit}}, \code{\link[SpaDES.core]{spades}}, \code{\link{experiment}}
#'
#' @author Eliot McIntire
#' @export
#' @rdname experiment2
#' @example inst/examples/example_experiment2.R
setGeneric(
  "experiment2",
  signature = "...",
  function(..., replicates = 1, clearSimEnv = FALSE,
           createUniquePaths = c("outputPath"), useCache = FALSE,
           debug = getOption("spades.debug"), drive_auth_account) {
    standardGeneric("experiment2")
  })

#' @rdname experiment2
#' @importFrom future.apply future_lapply future_mapply
#' @importFrom SpaDES.core packages
setMethod(
  "experiment2",
  signature("simList"),
  definition = function(..., replicates, clearSimEnv,
                        createUniquePaths = c("outputPath"),
                        useCache = FALSE, debug = getOption("spades.debug"),
                        drive_auth_account = NULL) {
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
      #if (length(replicates) == 1) replicates <- rep(replicates, length(ll))

      #simNames <- unlist(Map(x = simNames, times = replicates, rep))
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
    #iters <- seq_along(namsExpanded)
    names(namsExpanded) <- namsExpanded
    #out <- mapply(
      #list2env(
    out <- future_mapply(
      #X = iters,
      name = namsExpanded,
      simName = simNames,
      sim = ll,  # recycled by replicates -- maybe this reduces copying ...?
      MoreArgs = list(clearSimEnv = clearSimEnv,
                      createUniquePaths = createUniquePaths,
                      useCache = useCache,
                      .spades = .spades,
                      debug = debug,
                      drive_auth_account = drive_auth_account),
      FUN = experiment2Inner,
      SIMPLIFY = FALSE,
      future.packages = pkg
    )#,
    names(out) <- namsExpanded
    list2env(out, envir = outSimLists@.xData)
    return(outSimLists)
  })

#' @importFrom SpaDES.core outputPath outputPath<- envir
#' @importFrom reproducible Cache
experiment2Inner <- function(sim, clearSimEnv, createUniquePaths,
                             simName, name, useCache = FALSE,
                             debug = getOption("spades.debug"), drive_auth_account,
                             ...) {
  # a <- rbindlist(inputObjects(sim), fill = TRUE, use.names = TRUE)
  # na <- ls(sim)[ls(sim) %in% a$objectName]
  # names(na) <- na; lapply(na, function(nn) grep(paste0(nn, "$"), names(b), value = TRUE))

  #simName <- simNames[X]
  #name <- names[X]
  outputPath(sim) <- checkPath(file.path(outputPath(sim), name),
                                   create = TRUE)
  if (!is.null(drive_auth_account))
    googledrive::drive_auth(drive_auth_account)
  googledrive::drive_deauth()
  s <- Cache(.spades, sim, useCache = useCache, simName,
             debug = debug, clearSimEnv = clearSimEnv, ..., omitArgs = "debug")
  s
}

#' @importFrom reproducible Copy
#' @importFrom future plan
.spades <- function(sim, debug = getOption("spades.debug"),
                    clearSimEnv = FALSE, ...) {
  # don't make a copy if it is callr or multisession because future will make the copy
  if (!any(c("callr", "multisession") %in% attr(plan(), "class"))) {
    a <- Sys.time()
    message("Copying simList prior to spades call")
    sim <- Copy(sim, filebackedDir = file.path(outputPath(sim), "rasterFiles"))
    b <- Sys.time()
    message(format(b-a), " to Copy")
  }
  s <- spades(sim, debug = debug, ...)
  if (isTRUE(clearSimEnv))
    rm(list = ls(s), envir = envir(s))
  return(s)
}


#' Coerce elements of a simLists object to a data.table
#'
#' This is particularly useful to build plots using the \pkg{tidyverse}, e.g., \pkg{ggplot2}.
#' @importFrom purrr transpose
#' @inheritParams data.table::as.data.table
#' @param vals A character vector or list of object names to extract from each
#'   \code{simList}, or a list of quoted expressions to calculate for each \code{simList},
#'   or a mix of character and quoted expressions.
#' @param byRep Should the \code{data.table} have a column labelled "rep", indicating replicate
#'   number/label. Currently, only \code{TRUE} is accepted.
#' @param objectsFromSim Character vector of objects to extract from the simLists. If
#'   omitted, it will extract all objects from each simList in order to calculate the
#'   \code{vals}. This may have a computational cost.
#' @param objectsFromOutputs Character vector of objects to load from the
#'   \code{outputs(sim)} prior to evaluating \code{vals}. If there already is an object
#'   with that same name in the \code{simList}, then it will be overwritten with
#'   the object loaded from \code{outputs(sim)}. If there are many objects with the
#'   same name, specifically from several \code{saveTime} values in the \code{outputs(sim)},
#'   these will all be loaded, one at a time, \code{vals} evaluated one at a time, and
#'   all the unique values will be returned. A column, \code{saveTime}, will be
#'   part of the returned value.
#' @param ... Additional arguments. Currently unused.
#' @details
#' See examples.
#'
#' @export
#' @importFrom tools file_ext
#' @importFrom data.table as.data.table is.data.table setDT := setnames
#' @importFrom SpaDES.core .fileExtensions
#' @include simLists-class.R
#'
#' @example inst/examples/example_experiment2.R
as.data.table.simLists <- function(x, byRep = TRUE, vals,
                                   objectsFromSim = NULL,
                                   objectsFromOutputs = NULL,  ...) {
  if (!isTRUE(byRep)) stop("byRep must be TRUE, currently")
  objs <- ls(x)
  names(objs) <- objs
  simLists <- gsub("_.*", "", objs)
  if (!is.list(vals)) {
    vals <- if (is.character(vals)) {
      as.list(vals)
    } else {
      list(vals)
    }
  }
  vals <- updateNames(vals)
  # namesVals <- names(vals)
  # emptyChar <- nchar(namesVals) == 0
  # if (is.null(namesVals) || any(emptyChar)) {
  #   valNames <- unlist(lapply(vals, function(x) format(x)))
  #   if (any(emptyChar)) {
  #     namesVals[emptyChar] <- valNames[emptyChar]
  #     valNames <- namesVals
  #   }
  #   names(vals) <- valNames
  # }

  # Evaluate the expression
  reps <- gsub(".*_", "", objs)
  ll <- lapply(objs, vals = vals, function(sName, vals) {
    n <- new.env(parent = .GlobalEnv)
    if (!is.null(objectsFromOutputs)) {
      outpts <- setDT(outputs(x[[sName]]))[objectsFromOutputs == objectName]
      Times <- outpts$saveTime
    } else {
      Times <- SpaDES.core::end(x[[sName]])
    }
    names(Times) <- as.character(Times)

    out <- lapply(Times, function(t) {
      if (!is.null(objectsFromOutputs)) {
        lapply(objectsFromOutputs, function(ob) {
          theLine <- outpts[objectsFromOutputs == objectName & saveTime == t, ]
          theFile <- theLine[, file]
          ext <- file_ext(theFile)
          dt1 <- data.table(exts = ext)
          fun <- setDT(.fileExtensions())[dt1, on = "exts"]$fun
          tmpObj <- get(fun)(theFile)
          assign(theLine$objectName, tmpObj, envir = x[[sName]])
        })
      }
      # get ALL objects from simList -- could be slow -- may need to limit
      if (is.null(objectsFromSim)) {
        objectsFromSim <- ls(x[[sName]])
      }
      list2env(mget(objectsFromSim, envir = envir(x[[sName]])), envir = n)
      lapply(vals, n = n, function(o, n) {
        if (is.call(o)) {
          eval(o, envir = n)
        } else {
          eval(parse(text = o), envir = n)
        }
      })
    })
    if (length(Times) == 1) {
      out <- out[[1]]
    } else {
      ll2 <- purrr::transpose(out)
      labels <- seq_along(ll2)
      names(labels) <- names(ll2)
      ll3 <- lapply(labels, ll2 = ll2, function(n, ll2)  t(rbindlist(ll2[n])))
      dt <- as.data.table(ll3)
      out <- data.table(saveTime = Times, dt)
    }
    out
  })

  if (!all(unlist(lapply(ll, is.data.table)))) {
    ll2 <- purrr::transpose(ll)
    labels <- seq_along(ll2)
    names(labels) <- names(ll2)
    ll3 <- lapply(labels, ll2 = ll2, function(n, ll2)  t(rbindlist(ll2[n])))
    dt <- data.table(simName = rownames(ll3[[1]]), as.data.table(ll3))
  } else {
    dt <- rbindlist(ll, use.names = TRUE, idcol = "simName", fill = TRUE)
  }
  dt[, `:=`(simList = gsub("_.*", "", simName), reps = gsub(".*_", "", simName))]
  varNameOnly <- gsub(".V[[:digit:]]+", "", names(dt))
  changed <- which(varNameOnly != names(dt))
  counts <- table(varNameOnly)
  whichSingleton <- which(counts == 1)

  if (any(changed %in% whichSingleton)) {
    out <- lapply(names(whichSingleton), dt = dt, function(n, dt) {
      setnames(dt, old = grep(n, names(dt), value = TRUE), new = n)
    })
  }
  # dt <- data.table(simList = simLists, reps = reps, dt)
  dt[]
}

.objNamesBySimList <- function(simLists) {
  objs <- ls(simLists)
  simLists <- gsub("_.*", "", objs)
  simListsBySimList <- split(objs, f = simLists)
  simListsBySimList <- lapply(simListsBySimList, sort)
}

updateNames <- function(lst, newNames) {
  namesVals <- names(lst)
  emptyChar <- nchar(namesVals) == 0
  if (is.null(namesVals) || any(emptyChar)) {
    if (missing(newNames))
      newNames <- unlist(lapply(lst, function(x) format(x)))
    if (any(emptyChar)) {
      namesVals[emptyChar] <- newNames[emptyChar]
      newNames <- namesVals
    }
    names(lst) <- newNames
  }
  lst
}
