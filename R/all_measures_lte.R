#' Test if all measures are nless-than-or-equal to previous measure.
#'
#' The stop condition for the main algorithm loop in missForest (Stekhoven and
#' Buehlmann, 2012) requires that all measures have either decreased or
#' increased. This is a helper function that tests if all measures are less than
#' or equal to the previous value.
#'
#' Given two (vectors of) measures of the 'change' between iterations for the
#' past two iterations of the missForest loop, this should return `TRUE` when
#' the loop should break. Another function, for example
#' [measure_stekhoven_2012()], calculates the measures passed to this test. The
#' missForest measures are, for example;
#'
#' 1.  the _negative_ sum of square differences between the imputed values (of
#'     continuous variables), and;
#' 2.  the proportion of stationary values (for categorical data).
#'
#' Alternatively, we could use;
#'
#' 1.  the mean of the (rank) correlation between the datasets of each ordered
#'     variable, and;
#' 2.  the proportion of stationary values (for non-ordered, categorical data).
#'
#' This treats all ordered data the same way, and should identify when the
#' entropy is close to a local optimum, see [measure_correlation()].
#'
#' @param measures_before numeric; measures between the second most recently
#' completed data set and its predecessor.
#' @param measures_now numeric; measures between the most recently completed
#' dataset and its predecessor.
#' @return logical; `TRUE` if all new values are not less than the previous
#' values.
#'
#' @seealso [measure_correlation()] [measure_stekhoven_2012()]
#'
#' @references
#' -   Stekhoven, D. J. & Buehlmann, P. (2012). MissForest--non-parametric
#'     missing value imputation for mixed-type data. _Bioinformatics_, _28_(1),
#'     112-118. <doi:10.1093/bioinformatics/btr597>
#'
#' @export
#' @md
all_measures_lte <- function(measures_before, measures_now) {

    if (!all(names(measures_before) %in% names(measures_now))) {
        stop('previous measure and measure do not have matching names.')
        measures_before <- measures_before[names(measures_before)]
    }

    test_values <- measures_now <= measures_before
    all(test_values) && length(test_values) == length(measures_now)

}

