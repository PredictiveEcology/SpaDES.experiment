test_that("test cache experiment", {
  skip_if_not_installed("NLMR") ## required by randomLandscapes module
  skip_if_not_installed("RColorBrewer") ## required by sample modules

  testInitOut <- testInit(c("SpaDES.experiment", "SpaDES.core"),
                          opts = list(spades.moduleCodeChecks = FALSE,
                                      spades.useRequire = FALSE,
                                      reproducible.useDBI = TRUE),
                          setPaths = FALSE)

  try(clearCache(tmpdir), silent = TRUE)
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)

  # Example of changing parameter values
  mySim <- simInit(
    times = list(start = 0.0, end = 1.0, timeunit = "year"),
    params = list(
      .globals = list(stackName = "landscape", burnStats = "nPixelsBurned"),
      # Turn off interactive plotting
      fireSpread = list(.plotInitialTime = NA),
      caribouMovement = list(.plotInitialTime = NA),
      randomLandscapes = list(.plotInitialTime = NA)
    ),
    modules = list("randomLandscapes", "fireSpread", "caribouMovement"),
    paths = list(modulePath = system.file("sampleModules", package = "SpaDES.core"),
                 outputPath = tmpdir,
                 cachePath = tmpdir),
    # Save final state of landscape and caribou
    outputs = data.frame(objectName = c("landscape", "caribou"),
                         stringsAsFactors = FALSE)
  )

  set.seed(1123)
  expr <- quote(experiment(Copy(mySim), replicates = 2, cache = TRUE, debug = FALSE,
                           omitArgs = c("progress", "debug", ".plotInitialTime", ".saveInitialTime")))
  sims <- eval(expr)

  out <- showCache(sims[[1]])
  expect_true(NROW(out[tagValue == "spades"]) == 2) # 2 cached copies, one for each "experiment"
  expect_true(NROW(unique(out$cacheId)) == 2) # 2 cached copies
  expect_output(print(out), "cacheId")
  expect_output(print(out), "simList")
  expect_true(NROW(out[!tagKey %in% reproducible:::.ignoreTagKeys()]) ==
                2*reproducible:::.cacheNumDefaultTags()) #
  # 1st - number of slots, minus the "dot" slots
  expect_true(NROW(out[tagKey %in% "preDigest"]) ==
                (length(grep("^\\.", slotNames(sims[[1]]), value = TRUE, invert = TRUE)) * 2 +
                   2 * (length(modules(mySim)) + 1) + 2 * 2)) # 2 args for Cache -- FUN & replicate
  expect_message(sims <- eval(expr),
                 "loading cached result from previous spades call")

  out2 <- showCache(sims[[1]])

  # 2 original times, 2 cached times per spades
  expect_true(NROW(out2[tagKey == "accessed"]) == 4)

  # 2 cached copies of spades
  expect_true(NROW(unique(out2$cacheId)) == 2)

  clearCache(sims[[1]], ask = FALSE)
  out <- showCache(sims[[1]])
  expect_true(NROW(out) == 0)
})


test_that("test cache experiment2", {
  skip_if_not_installed("NLMR") ## required by randomLandscapes module
  skip_if_not_installed("RColorBrewer") ## required by sample modules

  testInitOut <- testInit(c("SpaDES.experiment", "SpaDES.core"),
                          opts = list(spades.moduleCodeChecks = FALSE,
                                      spades.useRequire = FALSE,
                                      reproducible.useDBI = TRUE),
                          setPaths = FALSE)

  try(clearCache(tmpdir), silent = TRUE)
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)

  # Example of changing parameter values
  mySim <- simInit(
    times = list(start = 0.0, end = 1.0, timeunit = "year"),
    params = list(
      .globals = list(stackName = "landscape", burnStats = "nPixelsBurned"),
      # Turn off interactive plotting
      fireSpread = list(.plotInitialTime = NA),
      caribouMovement = list(.plotInitialTime = NA),
      randomLandscapes = list(.plotInitialTime = NA)
    ),
    modules = list("randomLandscapes", "fireSpread", "caribouMovement"),
    paths = list(modulePath = system.file("sampleModules", package = "SpaDES.core"),
                 outputPath = tmpdir,
                 cachePath = tmpdir),
    # Save final state of landscape and caribou
    outputs = data.frame(objectName = c("landscape", "caribou"),
                         stringsAsFactors = FALSE)
  )

  set.seed(1123)
  expr <- quote(experiment2(Copy(mySim), replicates = 2, useCache = TRUE, debug = FALSE))
  sims <- eval(expr)

  out <- showCache(sims[[1]])
  expect_true(NROW(out[tagValue == "spades"]) == 2) # 2 cached copies, one for each "experiment"
  expect_true(NROW(unique(out$cacheId)) == 2) # 2 cached copies
  expect_output(print(out), "cacheId")
  expect_output(print(out), "simList")
  expect_true(NROW(out[!tagKey %in% reproducible:::.ignoreTagKeys()]) ==
                2*reproducible:::.cacheNumDefaultTags()) #
  # 1st - number of slots, minus the "dot" slots
  expect_true(NROW(out[tagKey %in% "preDigest"]) ==
                (length(grep("^\\.", slotNames(sims[[1]]), value = TRUE, invert = TRUE)) * 2 +
                   2 * (length(modules(mySim)) + 1) + 2 * 2)) # 2 args for Cache -- FUN & replicate
  expect_message(sims <- eval(expr),
                 "loading cached result from previous spades call")

  out2 <- showCache(sims[[1]])

  # 2 original times, 2 cached times per spades
  expect_true(NROW(out2[tagKey == "accessed"]) == 4)

  # 2 cached copies of spades
  expect_true(NROW(unique(out2$cacheId)) == 2)

  clearCache(sims[[1]], ask = FALSE)
  out <- showCache(sims[[1]])
  expect_true(NROW(out) == 0)
})
