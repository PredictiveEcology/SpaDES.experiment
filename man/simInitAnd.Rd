% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simInitAndExperiment.R
\name{simInitAndExperiment}
\alias{simInitAndExperiment}
\title{Run \code{simInit} and \code{Experiment} in one step}
\usage{
simInitAndExperiment(
  times,
  params,
  modules,
  objects,
  paths,
  inputs,
  outputs,
  loadOrder,
  notOlderThan,
  replicates,
  dirPrefix,
  substrLength,
  saveExperiment,
  experimentFile,
  clearSimEnv,
  cl,
  ...
)
}
\arguments{
\item{times}{A named list of numeric simulation start and end times
(e.g., \code{times = list(start = 0.0, end = 10.0)}).}

\item{params}{A list of lists of the form \code{list(moduleName=list(param1=value, param2=value))}.
See details.}

\item{modules}{A named list of character strings specifying the names
of modules to be loaded for the simulation. Note: the module name
should correspond to the R source file from which the module is loaded.
Example: a module named "caribou" will be sourced form the file
\file{caribou.R}, located at the specified \code{modulePath(simList)} (see below).}

\item{objects}{(optional) A vector of object names (naming objects
that are in the calling environment of
the \code{simInit}, which is often the
\code{.GlobalEnv} unless used programmatically
-- NOTE: this mechanism will
fail if object name is in a package dependency), or
a named list of data objects to be
passed into the simList (more reliable).
These objects will be accessible
from the simList as a normal list, e.g,. \code{mySim$obj}.}

\item{paths}{An optional named list with up to 4 named elements,
\code{modulePath}, \code{inputPath}, \code{outputPath}, and \code{cachePath}.
See details. NOTE: Experimental feature now allows for multiple \code{modulePath}s
to be specified in a character vector. The modules will be searched for sequentially
in the first \code{modulePath}, then if it doesn't find it, in the second etc.}

\item{inputs}{A \code{data.frame}. Can specify from 1 to 6
columns with following column names: \code{objectName} (character, required),
\code{file} (character), \code{fun} (character), \code{package} (character),
\code{interval} (numeric), \code{loadTime} (numeric).
See \code{\link[SpaDES.core]{inputs}} and vignette("ii-modules") section about inputs.}

\item{outputs}{A \code{data.frame}. Can specify from 1 to 5
columns with following column names: \code{objectName} (character, required),
\code{file} (character), \code{fun} (character), \code{package} (character),
\code{saveTime} (numeric) and \code{eventPriority} (numeric). If
\code{eventPriority} is not set, it defaults to \code{.last()}. If \code{eventPriority}
is set to a low value, e.g., 0, 1, 2 and \code{saveTime} is \code{start(sim)},
it should give "initial conditions".

See \code{\link[SpaDES.core]{outputs}} and
\code{vignette("ii-modules")} section about outputs.}

\item{loadOrder}{An optional list of module names specifying the order in
which to load the modules. If not specified, the module
load order will be determined automatically.}

\item{notOlderThan}{A time, as in from \code{Sys.time()}. This is passed into
the \code{Cache} function that wraps \code{.inputObjects}.
If the module uses the \code{.useCache} parameter and it is
set to \code{TRUE} or \code{".inputObjects"},
then the \code{.inputObjects} will be cached.
Setting \code{notOlderThan = Sys.time()} will cause the
cached versions of \code{.inputObjects} to be refreshed,
i.e., rerun.}

\item{replicates}{The number of replicates to run of the same \code{simList}.
See details and examples.}

\item{dirPrefix}{String vector. This will be concatenated as a prefix on the
directory names. See details and examples.}

\item{substrLength}{Numeric. While making \code{outputPath} for each spades call, this
is the number of characters kept from each factor level.
See details and examples.}

\item{saveExperiment}{Logical. Should params, modules, inputs, sim, and resulting
experimental design be saved to a file. If TRUE are saved to a single list
called \code{experiment}. Default TRUE.}

\item{experimentFile}{String. Filename if \code{saveExperiment} is TRUE; saved to
\code{outputPath(sim)} in \code{.RData} format. See Details.}

\item{clearSimEnv}{Logical. If TRUE, then the envir(sim) of each simList in the return list
is emptied. This is to reduce RAM load of large return object.
Default FALSE.}

\item{cl}{A cluster object. Optional. This would generally be created using
parallel::makeCluster or equivalent. This is an alternative way, instead
of \code{beginCluster()}, to use parallelism for this function, allowing for
more control over cluster use.}

\item{...}{Passed to \code{spades}. Specifically, \code{debug}, \code{.plotInitialTime},
\code{.saveInitialTime}, \code{cache} and/or \code{notOlderThan}. Caching
is still experimental. It is tested to work under some conditions, but not
all. See details.}
}
\description{
Run \code{simInit} and \code{Experiment} in one step
}
\details{
\code{simInitAndExperiment} cannot pass modules or params to \code{experiment} because
these are also in \code{simInit}. If the \code{experiment} is being used
to vary these arguments, it must be done separately (i.e., \code{simInit} then
\code{experiment}).
}
