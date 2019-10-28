test_that("Fork cluster does not work correctly for data.table", {
  # THIS ISN"T CORRECTLY FINDING THE PROBLEM YET
  if (interactive() && .Platform$OS.type == "unix")  {
    testInitOut <- testInit(smcc = FALSE, libraries = c("data.table", "parallel"))
    on.exit({
      testOnExit(testInitOut)
    }, add = TRUE)
    cl <- SpaDES.core:::.makeClusterRandom(5)
    on.exit(stopCluster(cl))

    dt <- data.table(a = LETTERS, b = sample(letters))

    # copy -- breaks connection between data.tables

    fun <- function(dt) {
    fun <- function(dt) {
      setkeyv(dt, sample(c("a", "b"), size = 1))
      dt
    }

    clusterExport(cl = cl, c("fun", "dt"), envir = environment())
    clusterEvalQ(cl, {
      require(data.table)
    })
    clusterEvalQ(cl, {
      fun(dt = dt)
    })
  }
}})
