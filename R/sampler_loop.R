# SPDX-FileCopyrightText: 2023 Stephen Wade <stephematician@gmail.com>
# SPDX-License-Identifier: MIT

#' Generate an imputed data set via iterative procedure.
#'
#' A similar iterative procedure spans the process for either a single chain
#' for the Multiple Imputation by Chained Equations algorithm, a.k.a. 'mice'
#' (van Buuren and Groothuis-Oudshoorn, 2011) and the process of estimation
#' by 'missForest' (Stekhoven and Buehlmann, 2012). Each step of the procedure
#' involves fitting (sequentially) a random forest to each variable and then
#' imputing new values for the missing cases. The two differ by the type of
#' prediction used to generate a new value, and by whether the latest imputed
#' values are used in training or exclusively those of the preceding step.
#'
#' For a full description of the procedure for a single chain of 'mice' using
#' random forests see Doove et al (2014) and Bartlett (2014). In brief, the
#' procedure is:
#'
#' -   For each variable (in a pre-specified order):
#'     -   Train a random forest using the observed values.
#'     -   For each missing case, traverse one or more trees and identify a pool
#'         of candidate values from the leaves (for Doove et al, take the
#'         observed from each leaf, for Bartlett et al, take bootstrap observed
#'         values from one random leaf).
#'     -   Draw once from the candidate values and update the variable.
#'
#' Here, `data` must be complete; i.e. any missing value has already been
#' filled in some naive way using functions like [impute_naive_by_sample()]
#' (the usual for 'mice') or [impute_naive_by_aggregate()] (the original
#' 'missForest' approach). `indicator` will dictate which values are considered
#' missing cases and will be updated by the loop.
#'
#' `model` identifies which variables are included in each random forest. This
#' is either a numeric (with zero and one values) or logical matrix. See
#' [mimputest()] for further details.
#'
#' Training is performed by a call to [literanger::train()] given by
#' `call_lr_train`, which may contain arguments applied to all random
#' forests. Per-variable arguments can be supplied using `overrides` which will
#' replace the 'global' arguments in `call_lr_train`. By default, classification
#' trees are used for factors (including ordered), logical and integers, while
#' regression trees are used for numeric (continuous) data. See also
#' [mimputest()] for information about default values used.
#'
#' `sampler` identifies whether or not a Gibbs-like sampler is employed
#' (`='gibbs'`) over the variables or a 'missForest'-like sampler
#' (`='missforest'`). The Gibbs-like sampler trains on the latest imputed
#' values, whereas the 'missForest'-like trains on the imputed values from the
#' previous iteration's complete data set. `prediction_type` set to `'inbag'`
#' uses Bartlett's prediction (2014), while `'doove'` uses the approach from
#' 'mice' by Doove et al (2014). `'bagged'` can be used if estimating missing
#' values ala 'missForest'.
#'
#' The procedure for drawing random values from a forest for multiple imputation
#' algorithms is either the usual 'mice' procedure, as per Doove et al, 2014, or
#' the procedure given by Bartlett, 2014. For estimation of missing values, the
#' procedure would be the usual 'bagged' approach for a random forest
#' (Breiman 2001). This is specified via `prediction_type` which is either
#' `'inbag'` (Bartlett), `'doove'` (Doove) or `'bagged'` (Breiman).
#'
#' `stop_measure` can be set to one of [measure_correlation()],
#' [measure_stekhoven_2012()], or [measure_degenerate()]. The latter is
#' applicable to 'mice'-like algorithms and stops the loop when it reaches the
#' pre-determined `loop_limit`, while the former two are variations on stopping
#' criteria that can be used in 'missForest'-like estimation.
#'
#' After the missing values have been drawn for a variable a post-processing
#' 'cleaning' step can be applied. These steps are provided on a per-variable
#' basis in the `clean_step` argument. The user provides (for any variable)
#' functions that accept arguments named `data` and `imputed` which,
#' respectively, contain the data-set for the rows with missing values as used
#' in training the random forest and the imputed values drawn from the (trained)
#' random forest. The functions should return a vector of the same length as
#' `imputed` with the clean values. The cleaning step can ensure that known (a
#' priori) constraints on the value are respected.
#'
#' All the imputed values (over all iterations) are returned, along with; a
#' convergence indicator (if applicable); the number of iterations performed;
#' the out-of-bag error on a per-iteration and per-variable basis, the
#' recorded values of the stopping condition measures for each iteration, and
#' optionally; the random forest for each variable from the final iteration
#' (when `keep_forests=TRUE`, default is `FALSE`).
#'
#' @inheritParams mimputest
#' @param indicator list: an indicator of missing status for each (named)
#' variable for each row in the data, equal to 1 (or `TRUE`) for a missing case,
#' otherwise 0 (or `FALSE`).
#' @param call_lr_train call: a 'skeleton' call to [literanger::train()] for
#' fitting random forests, arguments can be over-ridden on a per-variable basis
#' by `overrides`.
#' @param chain_id integer (scalar): the identifier of the current chain to be
#' displayed in the progress bar.
#'list (named, class `mimputest`): The result from each sampling loop
#' is provided, along with; information about the call and the parameters used to
#' run the samplers; the convergence status and other summary statistics from
#' each sampler (chain), and; some record-keeping. The named items are:
#' @returns list (named): the result from each iteration in the sampler loop
#' along with; the number of iterations performs, convergence measures and
#' status, the out-of-bag error during training and the forests (if requested).
#' The named items are:
#' -   `converged` (logical): the indicator of convergence status for the
#'     sampler loop (always `TRUE` when [measure_degenerate()] is used and no
#'     error occured).
#' -   `imputed` (list): the imputed values for each iteration, each item
#'     contains the imputed values of each variable with values in the same
#'     order as the missing cases in the data (For the variable).
#' -   `iterations` (integer): the number of iterations performed.
#' -   `oob_error` data.frame; the out-of-bag error for each random forest by
#'     iteration and variable.
#' -   `measures` data.frame: the values of the criterion measure(s) (as
#'     returned by the function provided as `stop_measure`) with one row per
#'     iteration.
#' -   `forests` (list): the random forests for each variable from the final
#'     iteration of each sampler if `keep_forests=TRUE`, otherwise `NULL`
#'     (default)
#'
#' @seealso [sampler_step()] [literanger::train()]
#'
#' @references
#' -   Bartlett, J. (2014, 6-11 July). _Methodology for multiple imputation for
#'     missing data in electronic health record data_ \[Conference
#'     presentation\]. International Biometric Conference, Florence, TOS, Italy.
#'     [Archived 2019-08-19][bartlett2014_archive].
#' -   Breiman, L. (2001). Random forests. _Machine Learning_, _45_, 5-32.
#'     \doi{10.1023/A:1010933404324}.
#' -   Doove, L.L., Van Buuren, S., & Dusseldorp, E. (2014). Recursive
#'     partitioning for missing data imputation in the presence of interaction
#'     \doi{10.1016/j.csda.2013.10.025}.
#' -   Stekhoven, D. J. & Buehlmann, P. (2012). MissForest--non-parametric
#'     missing value imputation for mixed-type data. _Bioinformatics_, _28_(1),
#'     112-118. \doi{10.1093/bioinformatics/btr597}.
#' -   Van Buuren, S. & Groothuis-Oudshoorn, K. (2011). mice: Multivariate
#'     Imputation by Chained Equations in R. _Journal of Statistical Software_,
#'     _45_(3), 1-67. \doi{10.18637/jss.v045.i03}.
#'
#' [bartlett2014_archive]: https://web.archive.org/web/20190819140612/http://thestatsgeek.com/wp-content/uploads/2014/09/RandomForestImpBiometricsConf.pdf
#'
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @md
sampler_loop <- function(data, model, indicator, call_lr_train, sampler,
                         prediction_type, stop_measure=measure_correlation,
                         loop_limit=10L, overrides=list(), clean_step=list(),
                         keep_forests=FALSE, chain_id=1L, verbose=FALSE) {

    stop_measures <- list(NULL)
    imputed <- list(mapply(
        '[', data[rownames(model)], indicator[rownames(model)], SIMPLIFY=FALSE)
    )
    converged <- FALSE
  # categorical variables are monitored via proportion false-classified, while
  # numeric variables are evaluated by mean square error
    oob_levels <- c('mse', 'pfc')
    oob_error <- data.frame(iteration=integer(), variable=character(),
                            measure=factor(levels=oob_levels),
                            value=numeric())
    oob_measure <- sapply(data[rownames(model)], is.factor) %>%
        replace('mse', ., 'pfc') %>% factor(levels=c('mse', 'pfc'))

    forests <- NULL

    if (nrow(model) == 0)
        return(list(converged=NULL,
                    imputed=imputed,
                    iterations=0L,
                    oob_error=oob_error,
                    stop_measures=stop_measures[-1L],
                    forests=forests))

    if (verbose) {
        pb <- txtProgressBar(paste0('Running chain #', chain_id),
                             min=0, max=loop_limit)
        on.exit(close(pb), add=TRUE)
    }

    for (j in seq_len(loop_limit)) {

        result_j <- sampler_step(
            data=data, model=model, indicator=indicator,
            call_lr_train=call_lr_train, sampler=sampler,
            prediction_type=prediction_type, overrides=overrides,
            clean_step=clean_step, keep_forests=keep_forests
        )

        imputed[[j+1L]] <- result_j$imputed
        for (response_k in rownames(model)) {
            indicator_k <- indicator[[response_k]]
            data[indicator_k,response_k] <- imputed[[j+1L]][[response_k]]
        }
        oob_error <- rbind(oob_error,
                           cbind(data.frame(iteration=j), result_j$oob_error))

        stop_measures[[j+1L]] <- stop_measure(imputed[[j]], imputed[[j+1L]],
                                              data, indicator)

        if (all_measures_lte(stop_measures[[j]], stop_measures[[j+1L]])) {
            if (verbose) setTxtProgressBar(pb, loop_limit)
            forests <- result_j$forests
            converged <- T
            break
        }

        if (verbose) setTxtProgressBar(pb, j)

    }

    list(converged=converged,
         imputed=imputed,
         iterations=length(stop_measures) - 1L,
         oob_error=oob_error,
         measures=do.call(rbind, stop_measures[-1L]),
         forests=forests)

}

