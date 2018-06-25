# mlComp - Machine Learning Challenges based on [mlr](https://github.com/mlr-org/mlr) and [OpenML](https://github.com/openml/openml-r)

[![CRAN Status Badge](http://www.r-pkg.org/badges/version/autoxgboost)](https://CRAN.R-project.org/package=mlComp)
[![CRAN Downloads](http://cranlogs.r-pkg.org/badges/autoxgboost)](https://cran.rstudio.com/web/packages/mlComp/index.html)


* Install the development version

    ```splus
    devtools::install_github("ja-thomas/mlComp")
    ```

# General overview

Machine Learning competitions and challenges in R. Create [mlr](https://github.com/mlr-org/mlr) learner to beat performance threshold on [OpenML](https://www.openml.org/) tasks.
Users can set difficulty level and time limit.

```splus
library(mlrComp)
chall = Challenge$new(id = 3, difficulty = "very easy", time.limit = 3600)
lrn = makeLearner("classif.rpart")
chall$submit(lrn)
```

# Data Preprocessing

The dataset can not be changed by the user, but preprocessing can be done via [mlrCPO](https://github.com/mlr-org/mlrCPO)

```splus
library(mlrComp)
library(mlrCPO)
chall = Challenge$new(id = 3, difficulty = "very easy", time.limit = 3600)
lrn = cpoModelMatrix(~ 0 + .^2) %>>%  # interactions
   makeLearner("classif.rpart")
chall$submit(lrn)
```
