# Copyright (c) 2018-2023, Stephen Wade. All rights reserved.

#' @importFrom rlang expr_deparse
#' @noRd
msg_if_not <- function(condition, msg=NULL)
    if (!condition) {
        if (is.null(msg)) rlang::expr_deparse(substitute(condition)) else msg
    }

#' Generate rows of a matrix sampled from the first `n` natural numbers.
#
#' In the case of non-replacement; this function is a little less naive than
#' applying `sample.int` to each row - instead if the number of observations is
#' large enough it first generates the possible arrangements for each row, and
#' then draws from the arrangements (see  `utils::combn`).
#'
#' When sampling with replacement, the implementation is a trivial call to
#' `sample.int`.
#'
#' @param n integer (scalar): the number of chains, i.e. the rows of the matrix
#' will be samples from the set
#' \eqn{\{1, \ldots, n\}}{group('{', list(1, ..., n), '}')}.
#' @param k integer (scalar): the number of imputed data sets that will be
#' generated, also the nubmer of columns in the output.
#' @param size integer (scalar): the number of observations with one or more
#' missing value, also the number of rows in the output.
#' @param replace boolean (scalar): whether to sample with replacement or not.
#' @return matrix; a `size` by `k` matrix with each entry being a value between
#' 1 and `n` representing which chain to take imiputed values from for each
#' observation (rows) and imputed data set (column).
#'
#' @importFrom utils combn
#' @noRd
#' @md
row_sample_n <- function(n, k=n, size=0, replace=F) {

    stopifnot(n >= 1 && k >= 1)
    stopifnot(k <= n)
    stopifnot(round(k) == k & round(n) == n)

    if (!replace && round(k) > 1) {
        if (size >= 2 * choose(n, k)) {
          # subset all combinations
            t(combn(x=n, m=k))[
                sample.int(choose(n, k), replace=T, size=size),,drop=F
            ]
        } else {
            # loop is probably faster
            matrix(sapply(rep_len(n, size), sample.int, size=k, replace=F),
                   byrow=T,
                   nrow=size)
        }
    } else {
        matrix(sample.int(n, replace=T, size=size * k), nrow=size)
    }

}


# Match the leaves that the missing cases belong to with those of predicted
# values, then draw once from the matching predicted values for each missing
# case.  See <https://github.com/amices/mice/blob/master/R/mice.impute.rf.R> for
# same procedure.
predict_doove <- function(rf_fit, data, indicator, response) {

  # get the id's of the leaves for the observed and missing cases
    leaves <- predict(rf_fit,
                      newdata=data,
                      prediction_type='nodes')$nodes
    n_tree <- ncol(leaves)
    obs <- data[!indicator, response]

    donor_forest <- sapply(
        seq_len(n_tree),
        function(j) {
            unname(split(obs, leaves[!indicator, j])[
                as.character(leaves[indicator, j])
            ])
        }
    )
    if (sum(indicator) == 1) donor_forest %<>% array(dim=c(1, n_tree))
    apply(donor_forest, 1, function(s) sample(unlist(s), size=1, replace=TRUE))

}

