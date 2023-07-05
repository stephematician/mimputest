# SPDX-FileCopyrightText: 2023 Stephen Wade <stephematician@gmail.com>
# SPDX-License-Identifier: MIT

#' Test if all measures are less-than-or-equal to previous measure.
#'
#' This function tests if the latest set of measures of sequential imputed
#' data sets are less than or equal to the previous value. This helps assess the
#' stop condition for the main algorithm loop in missForest (Stekhoven and
#' Buehlmann, 2012).
#'
#' Given two (vectors of) measures of the 'change' between iterations for each
#' of the past two iterations of the missForest loop, this helper should return
#' `TRUE` when the loop should break (i.e. when the measures have changed
#' direction). Calculating the change between datasets is performed by other
#' functions , such as [measure_stekhoven_2012()]. The measures used by
#' missForest are;
#'
#' 1.  the _negative_ normalised square root sum of square differences between
#'     the imputed values (of continuous variables), and;
#' 2.  the proportion of stationary values (of all categorical data).
#'
#' Alternatively, we could use the mean over all variables of;
#'
#' 1.  the (rank) correlation between the datasets of each ordered
#'     variable, and;
#' 2.  the Cramer V between data sets for each non-ordered (categorical)
#'     variable.
#'
#' This alternative measure attempts to use correlation and correspondence to
#' identify when entropy is close to a local maximum, see
#' [measure_correlation()]. No evidence is available to support the use of the
#' mean across these two measures, so it is unknown whether the unordered and
#' ordered data are being weighted correctly.
#'
#' For a measure that always breaks the loop after a fixed number of iterations,
#' see [measure_degenerate()].
#'
#' @param measures_before numeric: measures between the second most recently
#' completed data set and its predecessor.
#' @param measures_now numeric: measures between the most recently completed
#' dataset and its predecessor.
#' @return logical: `TRUE` if all new values are not less than the previous
#' values.
#'
#' @seealso [measure_correlation()] [measure_stekhoven_2012()]
#' [measure_degenerate()]
#'
#' @references
#' -   Stekhoven, D. J. & Buehlmann, P. (2012). MissForest--non-parametric
#'     missing value imputation for mixed-type data. _Bioinformatics_, _28_(1),
#'     112-118. \doi{10.1093/bioinformatics/btr597}.
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

