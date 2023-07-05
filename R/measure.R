# SPDX-FileCopyrightText: 2023 Stephen Wade <stephematician@gmail.com>
# SPDX-License-Identifier: MIT

#' Measure the association between sequential complete data sets.
#'
#' The association (or correlation) of the data from one complete data set to
#' another can be used to assess stopping criterion for the missForest procedure
#' (Stekhoven and Buehlmann, 2012). Ordered data associations are measured by
#' rank correlations; while non-ordered data associated are measures by
#' Cramer's V with bias correction
#' <https://en.wikipedia.org/wiki/Cram%C3%A9r%27s_V>. The mean across all
#' variables is returned as a single measure.
#'
#' The missForest algorithm compares two sequential measures, i.e. the
#' association between the most recent and second-most recent data sets versus
#' the association between the second and third-most recent data sets. When the
#' measures have changed direction, the algorithm stops. It is assumed that the
#' measures are initially increasing, and when the measure decreases the
#' algorithm has converged. When using association or correlation measures, this
#' is approximately when the entropy has reached a maximum.
#'
#' The type of (rank) correlation can be changed via the `measure` argument,
#' see [stats::cor()] for details of the correlation measures available. The
#' Pearson measure is not recommended for ordered data but this function will
#' not error or warn if you select it - the ordered variable will be transformed
#' via [xtfrm()] and treated as continuous for the purpose of the Pearson
#' correlation calculation.
#'
#' @inheritParams sampler_loop
#' @param x_sample named list: the imputed values of the missing cases within
#' each variable from one iteration of the missForest algorithm.
#' @param y_sample named list: the imputed values from the following (or
#' preceding) iteration for `x_sample` with the same format as `x_sample`.
#' @param method character: the correlation coefficient to calculate for
#' ordered data passed to [stats::cor()].
#' @return named numeric: `measure` equal to the mean over all variables of the
#' (rank) correlation or Cramer's V for ordered and non-ordered data
#' (respectively).
#'
#' @seealso [sampler_loop()] [stats::cor()]
#'
#' @references
#' -   Stekhoven, D. J. & Buehlmann, P. (2012). MissForest--non-parametric
#'     missing value imputation for mixed-type data. _Bioinformatics_, 28(1),
#'     112-118. \doi{10.1093/bioinformatics/btr597}.
#'
#' @examples
#' \dontrun{
#' # simply pass to mimputest
#' mimputest(iris, stop_measure=measure_correlation)
#' }
#' @importFrom stats na.omit
#' @export
#' @md
measure_correlation <- function(x_sample, y_sample, data, indicator,
                                method='kendall') {

    nm <- names(data)
    ordered <- categorical <- NULL

  # ordered includes continuous, NOTE: used !! to convert to logical
    nm_ordered <- nm[!sapply(data, is.factor) |
                           !!sapply(data, is.ordered)]
    nm_categorical <- setdiff(nm, nm_ordered)

    observed <- mapply('[', data, lapply(indicator, '!'), SIMPLIFY=F)

    if (length(nm_ordered) > 0)
        ordered <- mapply(
            ordered_correlation,
            x=x_sample[nm_ordered], y=y_sample[nm_ordered],
            obs=observed[nm_ordered],
            MoreArgs=list(method=method)
        )

    if (length(nm_categorical) > 0)
        categorical <- mapply(
            cramer_v,
            x=x_sample[nm_categorical], y=y_sample[nm_categorical],
            obs=observed[nm_categorical]
        )

    values <- c(na.omit(categorical), na.omit(ordered))
    if (length(values) == 0) values <- 1

    c(measure=mean(values))

}


#' Measure if reached limit of iterations.
#'
#' A degenerate 'measure' of convergence, which simply returns whether to
#' continue if the limit of the number of iterations has been reached, or not.
#'
#' @inheritParams measure_correlation
#' @param env environment; environment of the calling function, usually
#' [sampler_loop()].
#' @return named numeric; `measure` equal to zero when the limit has been
#' reached or exceeded, otherwise the number (or index) of the current
#' iteration.
#'
#' @seealso [sampler_loop()]
#'
#' @examples
#' \dontrun{
#' # simply pass to mimputest
#' mimputest(iris, stop_measure=measure_degenerate)
#' }
#'
#' @export
#' @md
measure_degenerate <- function(x_sample, y_sample, data, indicator,
                               env=parent.frame()) {

    if (eval(expression(loop_limit < 2L), envir=env)) {
        NULL
    } else
        c(measure=eval(expression(j * (j < loop_limit)), envir=env))

}

#' Measure differences between sequential complete data sets according to
#' missForest stopping condition.
#'
#' The differences between one complete data set to another are used to assess
#' the stopping criterion for the missForest procedure (Stekhoven and
#' Buehlmann, 2012). Up to two measures are calculated, the first is a Frobenius
#' norm-based measure of the distance between continuous data, the second is the
#' proportion of stationary values for categorical data.
#'
#' The missForest algorithm compares sequential sets of measures, i.e. the
#' measured difference between the most recent and second-most recent data sets
#' versus that between the second and third-most recent data sets. When all
#' measures have changed direction, the algorithm stops. It is assumed that the
#' measures are initially increasing, and when the measure decreases the
#' algorithm has converged.
#'
#' The measures per data set, specifically, are:
#'
#' -  the negative of the sum of the differences squares of all continuous
#'    variables divided by the sum of the squares of the continuous data in the
#'    latest data set, and;
#' -  the proportion of stationary values  of categorical data divided by the
#'    total number of missing and non-missing values (of categorical data).
#'
#' These differ slightly from the formulas in the original missForest paper, as
#' we use a decrease between successive measures as the indicator for stopping,
#' and the categorical measure stated in the paper contains a typographical
#' error, see the source code <https://github.com/stekhoven/missForest/>
#' (commit `eb2f018`) for correct formula.
#'
#' @inheritParams measure_correlation
#' @return named numeric; two name values \describe{
#'   \item{`continuous`}{a Frobenius norm-based relative difference of the
#'     continuous data between the two complete data sets, and;}
#'   \item{`categorical`}{the proportion of stationary values of categorical,
#'     including ordered, data between the two complete data sets.}
#' }
#'
#' @seealso [sampler_loop()]
#'
#' @references
#' -   Stekhoven, D. J. & Buehlmann, P. (2012). MissForest--non-parametric
#'     missing value imputation for mixed-type data. _Bioinformatics_, 28(1),
#'     112-118. \doi{10.1093/bioinformatics/btr597}.
#'
#' @examples
#' \dontrun{
#' # simply pass to mimputest
#' mimputest(iris, stop_measure=measure_stekhoven_2012)
#' }
#'
#' @export
#' @md
measure_stekhoven_2012 <- function(x_sample, y_sample, data, indicator) {

    continuous <- categorical <- NULL
    nm <- names(data)

    nm_continuous <- nm[!sapply(data, is.factor)]
  # includes ordered categorical data
    nm_categorical <- setdiff(nm, nm_continuous)

    observed <- mapply('[', data, lapply(indicator, '!'), SIMPLIFY=F)

  # reversed sign here due to form of stop_condition()
    if (length(nm_continuous) > 0)
        continuous <- -sum(
            mapply(ssq_difference,
                   x=x_sample[nm_continuous], y=y_sample[nm_continuous])
        ) / sum(
            mapply(ssq, x=x_sample[nm_continuous], obs=observed[nm_continuous])
        )

  # note correction for denominator compared to paper
    if (length(nm_categorical) > 0)
        categorical <- sum(
            mapply(count_stationary,
                   x=x_sample[nm_categorical], y=y_sample[nm_categorical],
                   obs=observed[nm_categorical])
        ) / sum(sapply(x_sample[nm_categorical], length) +
                    sapply(observed[nm_categorical], length))

    c(categorical=categorical, continuous=continuous)

}

