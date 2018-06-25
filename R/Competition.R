#' Machine Learning Challenge.
#'
#' Create a Machine Learning challenge objects that the should solve in a given time limit.
#'
#' @format \code{\link{R6Class}} object.
#' @name Challenge
#' @section Usage:
#' \preformatted{
#' chall = Challenge$new(id = 3, difficulty = "easy", time.limit = 3600)
#' lrn = makeLearner("classif.rpart")
#' chall$submit(lrn)
#' }
#' @section Arguments:
#' \strong{For Challenge$new()}:
#' \describe{
#' \item{\code{id}}{[\code{integer(1)} | \code{NULL} | \code{"daily"}]\cr
#'   OpenML task.id used as challenge task. If \code{NULL} a random task from the
#'   \code{OpenML-CC18} benchmarking suit is sampled.
#'   A daily challenge can be selected by setting \code{id} to \code{"daily"}.
#' }
#' \item{\code{difficulty}}{[\code{character(1) | numeric(1)}]\cr
#'   Define the difficulty of the Challenge. \code{very easy} is the 0.5 quantile of runs submitted to OpenML,
#'   \code{easy} the 0.6, \code{medium} the 0.8, \code{hard} the 0.9 and \code{very hard} the 0.99 quantile.
#'   Alternatively the quantile can be directly specified as a numer value between 0 and 1.
#'   Default is \code{easy}.
#' }
#' \item{\code{time.limit}}{[\code{numeric(1)}]\cr
#'   Time limit, starting from the creation of the Challenge object until it is solved.
#'   Default is \code{1800}, i.e., half an hour.
#' }
#' }
#' \strong{For Challenge$submit()}:
#' \describe{
#' \item{\code{lrn}}{[\code{\link[mlr]{Learner}}]\cr
#'   Learner submitted to solved the challenge. Preprocessing on the task can be done with \pkg{mlrCPO}.
#' }
#' }
#' @section Fields:
#' \describe{
#' \item{\code{id} [\code{integer(1)}]}{
#'   id of \pkg{OpenML} task used in challenge.
#' }
#' \item{\code{difficulty} [\code{numeric(1)}]}{
#'   Difficulty of the challenge.
#' }
#' \item{\code{goal} [\code{numeric(1)}]}{
#'   Threshold value to beat with \code{Challenge$submit(lrn)}.
#' }
#' \item{\code{task} [\code{\link[mlr]{Task}}]}{
#'   The Data for the challenge, useful for exploratory analysis.
#' }
#' \item{\code{time.limit}}{[\code{numeric(1)}]\cr
#'   See above.
#' }
#' }
#' @examples
#' chall = Challenge$new(id = 3, difficulty = "easy", time.limit = 3600)
#' lrn = makeLearner("classif.rpart")
#' chall$submit(lrn)
#' @export
Challenge = R6::R6Class("Challenge",
  public = list(
    id = NULL,
    difficulty = NULL,
    goal = NULL,
    task = NULL,
    time.limit = NULL,
    initialize = function(id = NULL, difficulty = "easy", time.limit = 1800) {

      if (is.null(id) | id == "daily") {
        task.ids = listOMLTasks(tag = "OpenML-CC18")$task.id
        if (is.null(id)) {
          id = sample(task.ids, 1)
        } else {
          id = task.ids[as.numeric(Sys.Date()) %% length(task.ids)]
        }
      }

      checkmate::assertIntegerish(id, lower = 1, len = 1, any.missing = FALSE, null.ok = TRUE)

      if (is.numeric(difficulty)) {
        assertNumeric(difficulty, lower = 0, len = 1, any.missing = FALSE, null.ok = FALSE)
      } else {
        difficulty = switch(difficulty,
          "very easy" = 0.5,
          "easy" = 0.7,
          "medium" = 0.8,
          "hard" = 0.9,
          "very hard" = 0.99)
      }
      self$difficulty = difficulty

      private$oml.task = getOMLTask(id)

      runs = listOMLRunEvaluations(task.id = id, limit = 10000)
      self$goal = quantile(runs[, "predictive.accuracy"], difficulty, na.rm = TRUE)
      self$time.limit = time.limit
      cat(sprintf("Welcome to your ML challenge.\n
        Today's data set is %s.\n
        You have to beat an accuracy of %f.\n
        Use the `obj$submit(lrn)` function to solve the problem.\n
        Time limit is %.0f seconds.\n
        Good Luck!\n\n\n",
        private$oml.task$input$data.set$desc$name, self$goal, self$time.limit))

      self$task = convertOMLTaskToMlr(private$oml.task)$mlr.task
      private$start.time = Sys.time()

    },
    submit = function(lrn) {
      lrn = checkLearner(lrn, type = "classif")
      if (difftime(Sys.time(), private$start.time, units = "secs") > self$time.limit) {
        stop("Time limit reached, better luck next time!")
      } else {
        cat(sprintf("%.0f Seconds left!\n", self$time.limit - difftime(Sys.time(), private$start.time, units = "secs")))
      }
      #res = runTaskMlr(private$oml.task, lrn) #FIXME: See https://github.com/openml/openml-r/issues/417
      #perf = getBMRAggrPerformances(res$bmr, drop = TRUE)[1]

      mlr.task = convertOMLTaskToMlr(private$oml.task)
      res = resample(lrn, mlr.task$mlr.task, mlr.task$mlr.rin, measure = acc)
      perf = res$aggr
      sucess = perf > self$goal

      if (!sucess) {
        return(sprintf("Accuracy: %f! Not good enough, keep trying", perf))
      } else {
        return(sprintf("Accuracy: %f! Improved by %f. Solved in %.0f seconds, Congratulations!", perf, perf - self$goal, difftime(Sys.time(), private$start.time, units = "secs")))
      }
    }
    ),
  private = list(
    start.time = NULL,
    oml.task = NULL
    )
)
