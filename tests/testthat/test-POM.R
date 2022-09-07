test_that("test POM", {
  skip_if_not_installed("NLMR") ## required by randomLandscapes module

  testInitOut <- testInit(c("parallel", "raster"),
                          opts = list(spades.moduleCodeChecks = FALSE,
                                      spades.useRequire = FALSE),
                          setPaths = FALSE)

  try(clearCache(tmpdir), silent = TRUE)

  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)

  set.seed(89462)
  mySim <- simInit(
    times = list(start = 0.0, end = 2.0, timeunit = "year"),
    params = list(
      .globals = list(stackName = "landscape", burnStats = "nPixelsBurned"),
      fireSpread = list(nfires = 5),
      randomLandscapes = list(nx = 300, ny = 300)
    ),
    modules = list("randomLandscapes", "fireSpread", "caribouMovement"),
    paths = list(modulePath = system.file("sampleModules", package = "SpaDES.core"))
  )

  # Since this is a made up example, we don't have real data
  #  to run POM against. Instead, we will run the model once,
  #  take the values at the end of the simulation as if they
  #  are real data, then rerun the POM function next,
  #  comparing these "data" with the simulated values
  #  using Mean Absolute Deviation
  outData <- spades(reproducible::Copy(mySim), .plotInitialTime = NA)

  # Extract the "true" data, in this case, the "proportion of cells burned"
  # Function defined that will use landscape$Fires map from simList,
  #  i.e., sim$landscape$Fires
  #  the return value being compared via MAD with propCellBurnedData
  propCellBurnedFn <- function(landscape) {
    sum(getValues(landscape$Fires) > 0) / ncell(landscape$Fires)
  }
  # visualize the burned maps of true "data"
  propCellBurnedData <- propCellBurnedFn(outData$landscape)
  clearPlot()
  if (interactive()) {
    library(quickPlot)

    fires <- outData$landscape$Fires # Plot doesn't do well with many nested layers
    Plot(fires)
  }

  # Example 1 - 1 parameter
  # In words, this says, "find the best value of spreadprob such that
  #  the proportion of the area burned in the simulation
  #  is as close as possible to the proportion area burned in
  #  the "data", using \code{DEoptim()}.

  # Can use cluster if computer is multi-threaded.
  # This example can use parallelType = 1 in DEoptim. For this, you must manually
  #  pass all packages and variables as character strings.
  # cl <- makeCluster(detectCores() - 1) # not implemented yet in DEoptim
  out1 <- POM(mySim, "spreadprob",
              list(propCellBurnedData = propCellBurnedFn), # data = pattern pair
              # lower = 0.2, upper = 0.24,
              optimControl = list(steptol = 1),
              logObjFnVals = TRUE)
  expect_true(is(out1$optim, "list"))
  expect_true(abs(out1$optim$bestmem - 0.23) < 0.1)

})
