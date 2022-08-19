#' Run `simInit` and `Experiment` in one step
#'
#' @export
#' @aliases simInitAndExperiment
#' @rdname simInitAnd
#' @importClassesFrom SpaDES.core simList
#' @inheritParams SpaDES.core::simInit
#' @inheritParams experiment
#' @importFrom utils getFromNamespace
#' @include simLists-class.R
#' @details
#' `simInitAndExperiment` cannot pass modules or params to `experiment` because
#' these are also in `simInit`. If the `experiment` is being used
#' to vary these arguments, it must be done separately (i.e., `simInit` then
#' `experiment`).
simInitAndExperiment <- function(times, params, modules, objects, paths, inputs, outputs, loadOrder,
                                 notOlderThan, replicates,
                                 dirPrefix, substrLength, saveExperiment,
                                 experimentFile, clearSimEnv, cl, ...)  {
  list2env(list(...), envir = environment())
  lsAllNames <- ls(all.names = TRUE)
  lsAllNames <- lsAllNames[lsAllNames != "..."]

  objsAll <- mget(lsAllNames, envir = environment())

  objsSimInit <- objsAll[formalArgs(simInit)]

  namesMatchCall <- names(match.call())
  objsSimInit <- getFromNamespace(".fillInSimInit", ns = "SpaDES.core")(objsSimInit, namesMatchCall)

  sim <- simInit(times = objsSimInit$times, params = objsSimInit$params,
                 modules = objsSimInit$modules, objects = objsSimInit$objects,
                 paths = objsSimInit$paths, inputs = objsSimInit$inputs,
                 outputs = objsSimInit$outputs, loadOrder = objsSimInit$loadOrder,
                 notOlderThan = objsSimInit$notOlderThan)
  #sim <- do.call(simInit, objsSimInit)#AndX(scalls, "simInitAndExperiment", ...)

  experimentFormals <- formalArgs(experiment)[formalArgs(experiment) %in% names(objsAll)]
  objsExperiment <- append(list(sim = sim), objsAll[experimentFormals])
  spadesFormals <- formalArgs(spades)[formalArgs(spades) %in% names(objsAll)]
  objsSpades <- append(list(sim = quote(sim)), objsAll[spadesFormals]) # quote is so that entire simList is not serialized in do.call

  # Because there are some arguments in BOTH simInit and Experiment, can't pass them
  #  through, because they have different meaning
  objsExperiment <- objsExperiment[!names(objsExperiment) %in% names(objsSimInit)]
  onlyInSpades <- setdiff(names(objsSpades), names(objsExperiment))
  if (length(onlyInSpades))
    objsExperiment[onlyInSpades] <- objsSpades[onlyInSpades]
  sims <- do.call("experiment", objsExperiment)#AndX(scalls, "simInitAndExperiment", ...)

  return(sims)
}
