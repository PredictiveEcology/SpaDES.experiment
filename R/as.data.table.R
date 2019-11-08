#' Coerce elements of a simLists object to a data.table
#'
#' This is particularly useful to build plots using the \pkg{tidyverse}, e.g., \pkg{ggplot2}.
#' @importFrom purrr transpose
#' @inheritParams data.table::as.data.table
#' @param vals A (named) list of object names to extract from each
#'   \code{simList}, or a named list of quoted expressions to calculate for each \code{simList},
#'   or a mix of character and quoted expressions.
#' @param objectsFromSim Character vector of objects to extract from the simLists. If
#'   omitted, it will extract all objects from each simList in order to calculate the
#'   \code{vals}. This may have a computational cost. If \code{NA}, then no objects will be
#'   accessed from the \code{simList}. Objects identified here will only be as they are in
#'   the \code{simList}, i.e., at \code{end(sim)}.
#' @param objectsFromOutputs List of (named) character vectors of objects to load from the
#'   \code{outputs(sim)} prior to evaluating \code{vals}. If there already is an object
#'   with that same name in the \code{simList}, then it will be overwritten with
#'   the object loaded from \code{outputs(sim)}. If there are many objects with the
#'   same name, specifically from several \code{saveTime} values in the \code{outputs(sim)},
#'   these will all be loaded, one at a time, \code{vals} evaluated one at a time, and
#'   each of the values will be returned from each \code{saveTime}.
#'   A column, \code{saveTime}, will be
#'   part of the returned \code{data.table}
#'   For cases where more than one object is required at a given
#'   \code{saveTime}, all should be identified here, without time specified. This function will
#'   take all identified objects from the same time period.
#'
#' @param ... Additional arguments. Currently unused.
#' @details
#' See examples.
#'
#' @export
#' @importFrom tools file_ext
#' @importFrom data.table as.data.table is.data.table setDT := setnames set setkeyv
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

