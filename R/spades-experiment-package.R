##  SpaDES.experiment/R/SpaDES-experiment-package.R by Alex M Chubaty and Eliot J B McIntire
##  Copyright (C) 2015-2022 Her Majesty the Queen in Right of Canada,
##   as represented by the Minister of Natural Resources Canada
##

#' Categorized overview of the `SpaDES.experiment` package
#'
#' @description
#'
#' \if{html}{\figure{SpaDES.png}{options: width=100 alt="SpaDES logo" style="float: right;"}}
#' \if{latex}{\figure{SpaDES.png}{options: width=0.5in}}
#'
#' **`SpaDES.experiment` is deprecated and no longer maintained.** Its
#' experiment functionality has moved to the
#' [SpaDES.project](https://github.com/PredictiveEcology/SpaDES.project)
#' package, which is now its maintained home: `experiment()`, `experiment2()`,
#' `simInitAndExperiment()`, the `simLists` class and `as.data.table.simLists()`
#' all live there. The same-named functions here are thin shims that forward to
#' `SpaDES.project` (when installed). Please migrate:
#' `remotes::install_github("PredictiveEcology/SpaDES.project")`.
#'
#' Historically, this package provided tools to do simulation experiments within
#' the `SpaDES` ecosystem -- replication, parameter sweeps, scenario analysis,
#' pattern oriented modeling, and simulation experiments -- and introduced the
#' `simLists` class (an environment holding many `simList` objects) plus tools
#' for post hoc analyses of such objects.
#'
#' Bug reports: <https://github.com/PredictiveEcology/SpaDES.experiment/issues>
#'
#' Module repository: <https://github.com/PredictiveEcology/SpaDES-modules>
#'
#' Wiki: <https://github.com/PredictiveEcology/SpaDES/wiki>
#'
#' @import methods
#' @rdname SpaDES.experiment-package
"_PACKAGE"

# Whole-package deprecation notice on attach.
.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "SpaDES.experiment is DEPRECATED and no longer maintained.\n",
    "Its experiment functions -- experiment(), experiment2() and ",
    "simInitAndExperiment() --\n",
    "have moved to SpaDES.project; the versions here simply forward to it.\n",
    "Please migrate: ",
    "remotes::install_github('PredictiveEcology/SpaDES.project')"
  )
}
