test_that("experiment2 test 1", {
  skip_on_cran()

  skip_if_not_installed("ggplot2")
  skip_if_not_installed("NLMR") ## required by randomLandscapes module
  skip_if_not_installed("RColorBrewer") ## required by sample modules

  testInitOut <- testInit(c("raster", "future.callr", "future", "ggplot2", "data.table"),
                          smcc = FALSE, opts = list(reproducible.useMemoise = FALSE))
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)

  endTime <- 2
  # Example of changing parameter values
  mySim1 <- simInit(
    times = list(start = 0.0, end = endTime, timeunit = "year"),
    params = list(
      .globals = list(stackName = "landscape", burnStats = "nPixelsBurned"),
      # Turn off interactive plotting
      fireSpread = list(.plotInitialTime = NA, spreadprob = c(0.2), nFires = c(10)),
      caribouMovement = list(.plotInitialTime = NA),
      randomLandscapes = list(.plotInitialTime = NA, .useCache = "init")
    ),
    modules = list("randomLandscapes", "fireSpread", "caribouMovement"),
    paths = list(modulePath = system.file("sampleModules", package = "SpaDES.core"),
                 outputPath = tmpdir,
                 cachePath = tmpCache),
    # Save final state of landscape and caribou
    outputs = data.frame(objectName = c(rep("landscape", endTime), "caribou", "caribou"),
                         saveTimes = c(seq_len(endTime), unique(c(ceiling(endTime/2),endTime))),
                         stringsAsFactors = FALSE)
  )

  mySim2 <- simInit(
    times = list(start = 0.0, end = endTime, timeunit = "year"),
    params = list(
      .globals = list(stackName = "landscape", burnStats = "nPixelsBurned"),
      # Turn off interactive plotting
      fireSpread = list(.plotInitialTime = NA, spreadprob = c(0.2), nFires = c(20)),
      caribouMovement = list(.plotInitialTime = NA),
      randomLandscapes = list(.plotInitialTime = NA, .useCache = "init")
    ),
    modules = list("randomLandscapes", "fireSpread", "caribouMovement"),
    paths = list(modulePath = system.file("sampleModules", package = "SpaDES.core"),
                 outputPath = tmpdir,
                 cachePath = tmpCache),
    # Save final state of landscape and caribou
    outputs = data.frame(objectName = c(rep("landscape", endTime), "caribou", "caribou"),
                         saveTimes = c(seq_len(endTime), unique(c(ceiling(endTime/2),endTime))),
                         stringsAsFactors = FALSE)
  )

  mySim3 <- simInit(
    times = list(start = 0.0, end = endTime, timeunit = "year"),
    params = list(
      .globals = list(stackName = "landscape", burnStats = "nPixelsBurned"),
      # Turn off interactive plotting
      fireSpread = list(.plotInitialTime = NA, spreadprob = c(0.2), nFires = c(30)),
      caribouMovement = list(.plotInitialTime = NA),
      randomLandscapes = list(.plotInitialTime = NA, .useCache = "init")
    ),
    modules = list("randomLandscapes", "fireSpread", "caribouMovement"),
    paths = list(modulePath = system.file("sampleModules", package = "SpaDES.core"),
                 outputPath = tmpdir,
                 cachePath = tmpCache),
    # Save final state of landscape and caribou
    outputs = data.frame(objectName = c(rep("landscape", endTime), "caribou", "caribou"),
                         saveTimes = c(seq_len(endTime), unique(c(ceiling(endTime/2),endTime))),
                         stringsAsFactors = FALSE)
  )

  planTypes <- c("sequential", "multisession")
  planTypes <- if (requireNamespace("future.callr", quietly = TRUE)) c(planTypes, "callr") else planTypes
  # planTypes <- c("sequential")
  for (pl in planTypes) {
    cat(" -- testing future plan when", pl, "                ")
    warn <- capture_warnings(plan(pl, workers = 2)) # just about "workers" not defined in "sequential"
    # Test Caching
    mess <- capture_messages(spades(Copy(mySim1), debug = 2))
    expects <- if (is(plan(), "sequential")) 0 else 1 # sequential has no concurrent spades
    expect_true(sum(grepl("cached", mess)) == expects)
    mess <- capture_messages({
      sims <- experiment2(mySim1, mySim2)
    })
    expects <- if (is(plan(), "sequential")) 2 else 2 # sequential has no concurrent spades
    expect_true(sum(grepl("cached", mess)) == expects) # b/c they are at the same time. If sequential, one would be memoised
    # cap <- capture.output(mess <- capture_messages(sims <- experiment2(mySim1, mySim2,
    #                                                                    mySim3)))
    # expects <- 3 # uses a new session each call
    # expect_true(sum(grepl("cached", cap))==expects[1]) # these are not same session as previous, so can't memoise
    # expect_true(sum(grepl("memoised", cap))==expects[2]) # 2 were old, plus 1 was a redo in one of the workers

    # Test replication
    mySim1Orig <- Copy(mySim1)
    mySim2Orig <- Copy(mySim2)

    repNums <- c(3)
    cap1 <- capture.output({
      mess <- capture_messages({
        sims <- experiment2(sim1 = mySim1, sim2 = mySim2, sim3 = mySim3, replicates = repNums)
      })
    })
    # Test don't need to use Copy
    expect_true(isTRUE(all.equal(mySim1Orig, mySim1))) # can't use identical -- envs are different

    # Test replication -- can be a vector of replicates
    expect_true(length(ls(sims)) == repNums * 3)
    expect_true(sum(grepl("^sim1", sort(ls(sims)))) == repNums)
    expect_true(sum(grepl("^sim2", sort(ls(sims)))) == repNums)
    expect_true(sum(grepl("rep1$", sort(ls(sims)))) == repNums)
    expect_true(sum(grepl("rep2$", sort(ls(sims)))) == repNums)
    expect_true(sum(grepl("rep3$", sort(ls(sims)))) == repNums)
    expect_false(identical(sims$`sim1_rep1`$caribou$x1, sims$`sim1_rep2`$caribou$x1))
    expect_false(identical(sims$`sim1_rep1`$caribou$x1, sims$`sim2_rep2`$caribou$x1))
    expect_false(identical(sims$`sim1_rep1`$caribou$x1, sims$`sim2_rep1`$caribou$x1))
  }

  # stStart <- list()
  # stEnd <- list()
  # for (pl in c("sequential")) {
  # #  for (pl in c("sequential", "multisession", "callr")) {
  #   stStart[[pl]] <- Sys.time()
  #   cat(" -- testing future plan when", pl, "                ")
  #   warn <- capture_warnings(plan(pl, workers = 2)) # just about "workers" not defined in "sequential"
  #   cap1 <- capture.output(mess <- capture_messages(
  #     sims <- experiment2(sim1 = mySim1, sim2 = mySim2, sim3 = mySim3,
  #                         replicates = 3, useCache = FALSE)
  #   ))
  #   stEnd[[pl]] <- Sys.time()
  # }
  # lapply(names(stStart), function(x) print(stEnd[[x]] - stStart[[x]]))

  expect_true(is(sims, "simLists"))

  # test "show" method
  mess4 <- capture.output(sims)
  expect_true(sum(grepl("3 simLists", mess4)) == 1)

  df1 <- as.data.table(sims, vals = c("nPixelsBurned", NCaribou = quote(length(caribou$x1))))
  df2 <- as.data.table(sims, vals = c("nPixelsBurned", NCaribou = "length(caribou$x1)"))
  expect_true(identical(df1, df2))

  #df1 <- as.data.table(sims,
  #                     vals = c("nPixelsBurned", NCaribou = quote(length(caribou$x1))),
  #                     objectsFromOutputs = list(nPixelsBurned = NA, NCaribou = "caribou"))
  expect_error({
    df1 <- as.data.table(sims,
                         vals = c("nPixelsBurned"),
                         objectsFromOutputs = c(nPixelsBurned = NA))
  }, "must be a list")

  expect_error({
    df1 <- as.data.table(sims,
                         vals = c("nPixelsBurned",
                                  caribou2 = quote(NROW(caribou)),
                                  caribou = quote(NROW(caribou))),
                         objectsFromOutputs = list(nPixelsBurned = NA, caribou = "caribou"))
  }, "objectsFromOutputs is shorter than vals, and the name")

  # This gets recycled -- which is wrong behaviour
  mess <- capture_messages({
    df1 <- as.data.table(sims,
                         vals = c("nPixelsBurned",
                                  caribou = quote(NROW(caribou)),
                                  caribou2 = quote(NROW(caribou))),
                         objectsFromOutputs = list("caribou"))
  })
  expect_true(any(grepl("objectsFromOutputs is shorter than vals. Recycling", mess)))
  expect_true(any(grepl("vals produce columns", mess)))

  expect_error({
    df1 <- as.data.table(sims,
                         vals = c(caribou = quote(NROW(caribou)),
                                  caribou2 = quote(as.character(NROW(caribou)))),
                         objectsFromOutputs = list(caribou = "caribou", caribou2 = "caribou"))
  }, "vals produce different class objects; they must all produce")

  df1 <- as.data.table(sims,vals = quote(nPixelsBurned) )
  expect_true(is.data.table(df1))

  df1 <- as.data.table(sims, vals = c("nPixelsBurned"))

  df1[, year := rep(1:2, length.out = NROW(df1))]

  if (interactive()) {
    p <- ggplot(df1, aes(x = year, y = value, group = simList, color = simList)) +
      stat_summary(geom = "point", fun.y = mean) +
      stat_summary(geom = "line", fun.y = mean) +
      stat_summary(geom = "errorbar", fun.data = mean_se, width = 0.2)

    print(p)
  }
  # with an unevaluated string
  df1 <- as.data.table(sims, vals = list(NCaribou = "length(caribou$x1)"))

  if (interactive()) {
    p <- ggplot(df1, aes_string(x = "simList", y = "value", group = "simList", color = "simList")) +
      stat_summary(geom = "point", fun.y = mean) +
      stat_summary(geom = "errorbar", fun.data = mean_se, width = 0.2)
    print(p)
  }

  df1 <- as.data.table(sims,
                       vals = c(meanFireSize = quote(mean(table(landscape$Fires[])[-1]))),
                       objectsFromOutputs = list("landscape"))
  if (interactive()) {
    # with an unevaluated string
    p <- ggplot(df1, aes(x = simList, y = value, group = simList, color = simList)) +
      stat_summary(geom = "point", fun.y = mean) +
      stat_summary(geom = "errorbar", fun.data = mean_se, width = 0.2)
    print(p)

    p <- ggplot(df1, aes(x = saveTime, y = value, group = simList, color = simList)) +
      stat_summary(geom = "point", fun.y = mean) +
      stat_summary(geom = "line", fun.y = mean) +
      stat_summary(geom = "errorbar", fun.data = mean_se, width = 0.2)
    print(p)
  }

  df2 <- as.data.table(sims,
                       vals = c("nPixelsBurned",
                                meanFireSize = quote({
                                  mean(table(landscape$Fires[])[-1]) /
                                    NROW(caribou)
                                })),
                       objectsFromOutputs = list(NA, c("landscape", "caribou")))
  if (interactive()) {
    # with an unevaluated string
    p <- ggplot(df2[vals == "meanFireSize"],
                aes(x = saveTime, y = value, group = simList, color = simList)) +
      stat_summary(geom = "point", fun.y = mean) +
      stat_summary(geom = "line", fun.y = mean) +
      stat_summary(geom = "errorbar", fun.data = mean_se, width = 0.2)
    print(p)
  }

  fn <- quote({
    landscape$Fires[landscape$Fires[] == 0] <- NA;
    a <- boundaries(landscape$Fires, type = "inner");
    a[landscape$Fires[] > 0 & a[] == 1] <- landscape$Fires[landscape$Fires[] > 0 & a[] == 1];
    peri <- table(a[]);
    area <- table(landscape$Fires[]);
    keep <- match(names(area),names(peri));
    mean(peri[keep]/area)
  })

  df1 <- as.data.table(sims,
                       vals = c("nPixelsBurned",
                                perimToArea = fn,
                                meanFireSize = quote(mean(table(landscape$Fires[])[-1])),
                                caribouPerHaFire = quote({
                                  NROW(caribou) /
                                    mean(table(landscape$Fires[])[-1])
                                })),
                       objectsFromOutputs = list(NA, c("landscape"), c("landscape"),
                                                 c("landscape", "caribou")),
                       objectsFromSim = "nPixelsBurned")
  #objectsFromOutputs = c("landscape"))
  if (interactive()) {
    # with an unevaluated string
    p <- ggplot(df1[vals == "perimToArea",],
                aes(x = saveTime, y = value, group = simList, color = simList)) +
      stat_summary(geom = "point", fun.y = mean) +
      stat_summary(geom = "line", fun.y = mean) +
      stat_summary(geom = "errorbar", fun.data = mean_se, width = 0.2)
    warn <- capture_warnings(print(p))
  }
})

test_that("simLists tests 1", {
  testInitOut <- testInit("future", smcc = FALSE, opts = list(reproducible.useMemoise = FALSE))
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)

  s <- simInit()
  mess5 <- capture_messages({
    ss <- experiment2(s, s, s, replicates = 1,
                      createUniquePaths = c("outputPaths", "modulePaths"))
  })
  expect_true(sum(grepl("createUniquePaths only", mess5)) == 1)
  mess4 <- capture.output(ss)
  expect_true(sum(grepl("with 1 replicate", mess4)) == 1)

  expect_error({
    ss <- experiment2(s, s, s, replicates = c(1, 2, 1))
  })
  expect_error({
    ss <- experiment2(s, s, s, replicates = c(1, 2, 1))
  })

  plan("sequential")
  mess6 <- capture_messages(.spades(s))
  expect_true(sum(grepl("Copying simList prior", mess6)) == 1)

  s$hello <- 1
  sClear <- .spades(s, clearSimEnv = TRUE)
  lsOrig <- ls(s, all.names = TRUE)
  lsClear <- ls(sClear, all.names = TRUE)
  expect_true(identical("hello", setdiff(lsOrig, lsClear)))
})

test_that("simLists tests 2", {
  testInitOut <- testInit("parallel", smcc = FALSE, opts = list(reproducible.useMemoise = FALSE))
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)
  mess1 <- capture_messages({
    a <- .optimalClusterNum()
  })
  expect_true(a == 1)
  dc <- detectCores()
  free <- Sys.which("free") ## Linux only
  if (!nzchar(free))
    expect_true(sum(grepl("The OS", mess1)) == 1)
  else
    expect_true(sum(grepl("The OS", mess1)) == 0)
  mess1 <- capture_messages({
    a <- .optimalClusterNum(maxNumClusters = 2, memRequiredMB = 10)
  })
  if (!nzchar(free))
    expect_true(a == 1)
  else
    expect_true(a == 2)

  mess1 <- capture_messages({
    a <- .optimalClusterNum(maxNumClusters = dc + 1, memRequiredMB = 10)
  })
  if (!nzchar(free))
    expect_true(a == 1)
  else
    expect_true(a == dc)
})
