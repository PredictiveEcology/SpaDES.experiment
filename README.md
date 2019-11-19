

[![Build Status](https://travis-ci.org/PredictiveEcology/SpaDES.experiment.svg?branch=master)](https://travis-ci.org/PredictiveEcology/SpaDES.experiment)
[![Appveyor Build status](https://ci.appveyor.com/api/projects/status/2fxqhgk6miv2fytd/branch/master?svg=true)](https://ci.appveyor.com/project/achubaty/spades-experiment/branch/master)
[![Coverage Status](https://coveralls.io/repos/github/PredictiveEcology/SpaDES.experiment/badge.svg?branch=master)](https://coveralls.io/github/PredictiveEcology/SpaDES.experiment?branch=master)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/SpaDES.experiment)](https://cran.r-project.org/package=SpaDES.experiment)
[![Downloads](http://cranlogs.r-pkg.org/badges/grand-total/SpaDES.experiment)](https://cran.r-project.org/package=SpaDES.experiment)

<img align="right" width="80" vspace="10" hspace="10" src="https://github.com/PredictiveEcology/SpaDES/raw/master/docs/images/SpaDES.png">

# SpaDES.experiment

Tools to build simulation experiments within the `SpaDES` ecosystem.
This includes replication, parameter sweeps, scenario analysis, pattern oriented modeling, and simulation experiments.

**Website:** [https://spades-experiment.PredictiveEcology.org](https://SpaDES-experiment.PredictiveEcology.org)

**Wiki:** [https://github.com/PredictiveEcology/SpaDES/wiki](https://github.com/PredictiveEcology/SpaDES/wiki)

## Installation

### Current stable release

**Install from CRAN:**

```r
install.packages("SpaDES.experiment")
```

**Install from GitHub:**

```r
#install.packages("devtools")
library("devtools")
install_github("PredictiveEcology/SpaDES.experiment", dependencies = TRUE) # master
```

### Development version (unstable)

**Install from GitHub:**

```r
#install.packages("devtools")
library("devtools")
install_github("PredictiveEcology/SpaDES.experiment", ref = "development", dependencies = TRUE)
```

## Contributions

Please see `CONTRIBUTING.md` for information on how to contribute to this project.
