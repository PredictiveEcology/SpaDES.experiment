Known issues: https://github.com/PredictiveEcology/SpaDES.experiment/issues

version 0.0.2.9006
=============

* `experiment()`, `experiment2()` and `simInitAndExperiment()` are **deprecated**; they have moved to the `SpaDES.project` package, which is now their maintained home. The functions here forward to `SpaDES.project::*` (when installed) and emit a deprecation message. Please switch to `SpaDES.project::experiment()` / `experiment2()` / `simInitAndExperiment()`.

version 0.0.2.9000
=============

* Drop support for R version < 4.0.
* use `NLMR` instead of `RandomFields` for random landscape generation.

version 0.0.1
=============

* A new package focused on running simulation experiments.
  Replaces and enhances this functionality in `SpaDES.core`.
