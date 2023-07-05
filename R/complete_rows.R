# SPDX-FileCopyrightText: 2023 Stephen Wade <stephematician@gmail.com>
# SPDX-License-Identifier: MIT

#' Complete a data set using replicates of incomplete rows.
#'
#' A 'bad' (occasionally useful) approach to analysis with multiply imputed
#' data is to construct a data set with the completely observed rows and
#' replicates of the rows with missing values filled with values from data
#' sets generated via multiple imputation; the replicate rows are then weighted
#' to their original count. This function constructs a data set like this using
#' the output of [mimputest()]. A weighted analysis can then be performed on the
#' newly completed data set.
#'
#' The best practise in analysing multiply imputed data is to analyse each
#' data set separately and pool the results using Rubin's or other rules. This
#' pattern can be done via the [with()] and [mice::pool()] functions. Output
#' from [mimputest()] _can_ be passed to these functions by first converting it
#' via [as_mids()]. On the other hand, some analyses do not fit this pattern, or
#' repeating analyses `n` times is inconvenient, and so a 'wrong' approach might
#' be more practical: generate a new dataset, to be analysed once, where each
#' incompletely observed row is replicated one or more times, assigned some
#' imputed values, and weighted to its original count.
#'
#' This function takes the original data set, extracts the completely and
#' incompletely observed rows, then replicates the incompletely observed rows.
#' The values for each replicate are taken from a randomly selected imputed data
#' set (i.e. sampler), using the value returned by [mimputest()]. Two columns
#' are added to indicate which sampler (or imputed data set) the values were
#' taken from for the row, and which replicate number was assigned to the row.
#' The final data set can be analysed once provided appropriate weighting is
#' assigned to replicate rows by the user (replicate rows can be identified by
#' non-`NA` values for the replicate number).
#'
#' @param result list (named, class 'mimputest'): a collection of imputed data
#' sets of missing values in the format returned by [mimputest()].
#' @param k integer: the number of replications of each incomplete row to
#' generate and then fill missing values in; the default is the number of sets
#' in `result`.
#' @param to_complete character: the names of the variables with missing values
#' to include when identifying rows to replicate and complete; the default is
#' all variables with one or more missing value.
#' @param replace logical: indicator of sampling with replacement (`TRUE`) or
#' not (`FALSE`), default is sample _without_ replacement.
#' @returns Data set in long format with the non-replicated rows (that were
#' complete in the original data) and the newly completed replicated rows. Two
#' columns are added to the data; `.sampler` and `.rep`. Both are integers that
#' describe which sampler the values in the (replicated) row were taken from
#' (i.e. which set in `result` was used for the row) and which replicate data
#' set the row belongs to. The non-replicated rows have values `NA_integer_` for
#' `.rep` and `.sampler`.
#'
#' @seealso [mimputest()] [mice::pool()]
#'
#' @export
#' @md
complete_rows <- function(result, k=result$n,
                          to_complete=rownames(result$model),
                          replace=FALSE) {

    data <- result$data

    if (inherits(data, 'grouped_df'))
        warning('groups in data will be ignored and discarded.')

    not_found <- setdiff(to_complete, rownames(result$model))

    if (length(not_found) > 0)
        stop('\'to_complete\' contains the following name(s) not found ',
             'in \'result\': ', paste(not_found, collapse=', '), '.')

    n <- result$n

    which_imputed <- lapply(result$indicator[to_complete], which)

    any_miss <- unique(unlist(which_imputed))

    J <- row_sample_n(n, k, size=length(any_miss), replace=replace)
    data_ <- data[rep(any_miss, times=k), , drop=FALSE]
    j_from <- lapply(which_imputed[to_complete], match, x=any_miss, nomatch=0L)

  # over the data sets from each sampler run by mimputest()
    for (j_sampler in seq_len(n)) {
      # observations (rows) and 'target' dataset to which values from this
      # sampler are assigned.
        ind_ <- which(J == j_sampler, arr.ind=TRUE)
      # Rows of the target data_ to assign to.
        row_ <- (ind_[,2] - 1L) * length(any_miss) + ind_[,1]
      # Record the sampler 'id' and which 'target' dataset in each (applicable)
      # row.
        data_[row_, '.sampler'] <- j_sampler
        data_[row_, '.rep'] <- ind_[,2]

        for (nm in to_complete) {
            from_ <- j_from[[nm]][ind_[,1]]
            data_[row_[from_ > 0L], nm] <- result$imputed[[j_sampler]][[nm]][from_]
        }
    }

    data[,'.sampler'] <- data[,'.rep'] <- NA_integer_
    rbind(data[-any_miss,], data_)

}

