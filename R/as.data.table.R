#' Coerce elements of a \code{simLists} object to a \code{data.table}
#'
#' This is particularly useful to build plots using the \pkg{tidyverse}, e.g., \pkg{ggplot2}.
#'
#' @inheritParams data.table::as.data.table
#'
#' @param vals A (named) list of object names to extract from each
#'   \code{simList}, or a named list of quoted expressions to calculate for each \code{simList},
#'   or a mix of character and quoted expressions.
#'
#' @param objectsFromSim Character vector of objects to extract from the simLists. If
#'   omitted, it will extract all objects from each simList in order to calculate the
#'   \code{vals}. This may have a computational cost. If \code{NA}, then no objects will be
#'   accessed from the \code{simList}. Objects identified here will only be as they are in
#'   the \code{simList}, i.e., at \code{end(sim)}.
#'
#' @param objectsFromOutputs List of (named) character vectors of objects to load from the
#'   \code{outputs(sim)} prior to evaluating \code{vals}. If there already is an object
#'   with that same name in the \code{simList}, then it will be overwritten with
#'   the object loaded from \code{outputs(sim)}. If there are many objects with the
#'   same name, specifically from several \code{saveTime} values in the \code{outputs(sim)},
#'   these will all be loaded, one at a time, \code{vals} evaluated one at a time, and
#'   each of the values will be returned from each \code{saveTime}.
#'   A column, \code{saveTime}, will be part of the returned \code{data.table}.
#'   For cases where more than one object is required at a given
#'   \code{saveTime}, all should be identified here, without time specified.
#'   This function will take all identified objects from the same time period.
#'
#' @param ... Additional arguments. Currently unused.
#'
#' @details
#' See examples.
#'
#' @export
#' @importFrom data.table := as.data.table is.data.table set setDT setkeyv setnames
#' @importFrom purrr transpose
#' @importFrom SpaDES.core .fileExtensions
#' @importFrom tools file_ext
#' @include simLists-class.R
#'
#' @example inst/examples/example_experiment2.R
as.data.table.simLists <- function(x, vals,
                                   objectsFromSim = NULL,
                                   objectsFromOutputs = NULL,  ...) {
  # if (!isTRUE(byRep)) stop("byRep must be TRUE, currently")
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
  if (!is.null(objectsFromOutputs)) {
    if (!is(objectsFromOutputs, "list")) {
      stop("objectsFromOutputs must be a list of same length as vals")
    }
    if (length(objectsFromOutputs) < length(vals)) { # recycling
      message("objectsFromOutputs is shorter than vals. Recycling values to create same length")
      if (!is.null(names(objectsFromOutputs))) {
        namesMatches <- match(names(objectsFromOutputs), names(vals))
        namesMisMatches <- which(!seq_along(vals) %in% namesMatches)
        if (length(namesMisMatches))
          stop("objectsFromOutputs is shorter than vals, and the name order also does not match")
      }
      objectsFromOutputs <- rep(objectsFromOutputs, length.out = length(vals))
      names(objectsFromOutputs) <- names(vals)
    }
    if (!all(names(objectsFromOutputs) == names(vals))) {
      stop("objectsFromOutputs must be a named list with same length and names as vals")
    }
  }
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

  ll <- lapply(objs, vals = vals, ofos = objectsFromOutputs,
               function(sName, vals, ofos) {
                 #Times <- numeric()
                 # THere will be 2 types -- those that need "Times" via outputs and
                 #   those that don't. Separate them here and deal differently with each
                 valsNoTime <- vals # default is all
                 out <- NULL
                 n <- new.env(parent = .GlobalEnv) # need this to be .GLobalEnv so eval will
                                                   # find functions in search() (including base)
                 if (is.null(objectsFromSim)) {
                   # get ALL objects from simList -- could be slow --
                   objectsFromSim <- ls(x[[sName]])
                 }
                 if (!all(is.na(objectsFromSim))) {
                   list2env(mget(na.omit(objectsFromSim),
                                 envir = envir(x[[sName]])), envir = n)
                 }

                 if (length(ofos)) {
                   needTimes <- unlist(lapply(ofos, function(o) !all(is.na(o))))
                   ofosNoTime <- ofos[!needTimes]
                   valsNoTime <- vals[!needTimes]
                   vals <- vals[needTimes]
                   ofos <- ofos[needTimes]
                   ofos <- unique(unlist(ofos))
                   out <- list()
                   if (!is.null(ofos)) {
                     outpts <- if (is(ofos, "list")) {
                       setDT(outputs(x[[sName]]))[objectName %in% unlist(ofos)]
                     } else {
                       setDT(outputs(x[[sName]]))[objectName %in% ofos]
                     }

                     innerTimes <- outpts$saveTime
                   } else {
                     innerTimes <- SpaDES.core::end(x[[sName]])
                   }
                   innerTimes <- unique(innerTimes)
                   names(innerTimes) <- as.character(innerTimes)
                   Times <- innerTimes

                   #if (length(innerTimes)) {
                     out <- lapply(innerTimes, function(t) {
                       if (!is.null(ofos)) {
                         theLine <- outpts[objectName %in% ofos & saveTime == t, ]
                         theFile <- theLine[, file]
                         ext <- file_ext(theFile)
                         dt1 <- data.table(exts = ext)
                         fun <- setDT(.fileExtensions())[dt1, on = "exts"]$fun
                         tmp <- lapply(seq_along(fun), function(i) {
                           tmpObj <- get(fun[i])(theFile[i])
                           assign(theLine$objectName[i], tmpObj, envir = n)
                         })
                       }


                       # get only some of the objects from x if don't need all
                       out2 <- lapply(vals, function(val) {
                         if (is.call(val)) {
                           eval(val, envir = n)
                         } else {
                           eval(parse(text = val), envir = n)
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
                       out <- data.table(saveTime = Times, dt, stringsAsFactors = FALSE)
                     }

                     # deal with mismatching classes
                     cla <- lapply(out[,!"saveTime"], is);
                     claSame <- all(sapply(cla, identical, cla[[1]]))
                     if (isFALSE(claSame)) {
                       onlyNumerics <- sapply(out[,!"saveTime"], is.numeric)
                       if (!all(onlyNumerics)) {
                         stop("vals produce different class objects; them must all produce same class")
                       } else {
                         message("vals produce columns of classes integer and numeric; converting all to numerics")
                         onlyInteger <- sapply(out[,!"saveTime"], is.integer)
                         namesInteger <- names(out[,!"saveTime"][,..onlyInteger])
                       }
                       tmp <- lapply(namesInteger, function(col) {
                         set(out, NULL, col, as.numeric(out[[col]]))
                       })
                     }
                     out <- data.table::melt(out, id.vars = "saveTime", variable.name = "vals",
                                             variable.factor = FALSE)

                 }

                 if (length(valsNoTime)) {
                   out2 <- lapply(valsNoTime, function(val) {
                     out3 <- if (is.call(val)) {
                       eval(val, envir = n)
                     } else {
                       eval(parse(text = val), envir = n)
                     }
                     dt <- as.data.table(out3)
                     out3 <- data.table(saveTime = end(x[[sName]]), dt, stringsAsFactors = FALSE)
                   })
                   out2 <- rbindlist(out2, idcol = "vals")
                   setnames(out2, old = "out3", new = "value")
                   #out2 <- data.table::melt(out2, id.vars = "saveTime", variable.name = "vals")
                   out <- rbindlist(list(out, out2), use.names = TRUE)

                 }
                 out
  })

  if (!all(unlist(lapply(ll, is.data.table)))) {
    ll2 <- purrr::transpose(ll)
    labels <- seq_along(ll2)
    names(labels) <- names(ll2)
    ll3 <- lapply(labels, ll2 = ll2, function(n, ll2)  t(rbindlist(ll2[n])))
    dt <- data.table(simName = rownames(ll3[[1]]), as.data.table(ll3),
                     stringsAsFactors = FALSE)
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
  set(dt, NULL, "order", match(dt$vals, names(vals)))
  setkeyv(dt, c("simName", "order"))
  set(dt, NULL, "order", NULL)
  dt[]
}
