# Copyright (c) Cancer Council NSW, 2018-2023. All rights reserved.

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
#' [smirf()] for further details.
#'
#' Training is performed by a call to [literanger::train()] given by
#' `call_lr_train`, which may contain arguments applied to all random
#' forests. Per-variable arguments can be supplied using `overrides` which will
#' replace the 'global' arguments in `call_lr_train`. By default, classification
#' trees are used for factors, while regression trees are used for continuous
#' data.
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
#' `stop_measure` can be set to one of [measure_correlation()],
#' [measure_stekhoven_2012()], or [measure_degenerate()]. The latter is
#' applicable to 'mice'-like algorithms and stops the loop when it reaches the
#' pre-determined `loop_limit`, while the former two are variations on stopping
#' criteria that can be used in 'missForest'-like estimation.
#'
#' `clean_step` is a per-variable post-processing function called on each
#' variable after it has been imputed. It can be used to ensure that constraints
#' are applied to variables. Each item in the list is a function that accepts
#' `data` and `imputed`; where the former is the data-set prior to imputed
#' values being drawn (or the preceding complete data set if
#' `sampler='missforest'`) restricted to the missing cases (in order). The
#' function should return a vector the same length and type as `imputed` with
#' the clean values.
#'
#' All the imputed values (over all iterations) are returned, along with; a
#' convergence indicator (if applicable); the number of iterations performed;
#' the out-of-bag error on a per-iteration and per-variable basis, and; the
#' recorded values of the stopping condition measures for each iteration.
#'
#' @inheritParams smirf
#' @param call_lr_train call: a 'skeleton' call to [literanger::train()] for
#' fitting random forests, arguments can be over-ridden on a per-variable basis
#' by `overrides`.
#' @param chain_id integer (scalar): the identifier of the current chain to be
#' displayed in the progress bar.
#' @returns named list: the following results from the iterative procedure:
#' \describe{
#'   \item{`converged`}{logical: indicator of convergence.}
#'   \item{`imputed`}{list: each item contains a named list of imputed values
#'     (for each variable) from one completed iteration; values are provided in
#'     the order that missing cases appear in the data (per variable).}
#'   \item{`iterations`}{integer: the number of completed iterations.}
#'   \item{`oob_error`}{data.frame: per-variable and per-iteration out-of-bag
#'     error estimates, with columns `iteration`, `variable`, `measure` (either
#'     'pfc' for categorical data, or 'mse' for continuous), and `value`.}
#'   \item{`stop_measures`}{list: each item contains a named numeric vector with
#'     the stopping criterion measures from one completed iteration.}
#' }
#'
#' @references
#' -   Bartlett, J. (2014, 6-11 July). _Methodology for multiple imputation for
#'     missing data in electronic health record data_ [Conference presentation].
#'     International Biometric Conference, Florence, TOS, Italy.
#'     [Archived 2019-08-19](https://web.archive.org/web/20190819140612/http://thestatsgeek.com/wp-content/uploads/2014/09/RandomForestImpBiometricsConf.pdf).
#' -   Stekhoven, D. J. & Buehlmann, P. (2012). MissForest--non-parametric
#'     missing value imputation for mixed-type data. _Bioinformatics_, 28(1),
#'     112-118. \doi{10.1093/bioinformatics/btr597}.
#' -   Van Buuren, S. & Groothuis-Oudshoorn, K. (2011). mice: Multivariate
#'     Imputation by Chained Equations in R. _Journal of Statistical Software_,
#'     _45_(3), 1-67. \doi{10.18637/jss.v045.i03}.
#'
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @md
sampler_loop <- function(data, model, indicator, call_lr_train, sampler,
                         prediction_type, stop_measure=measure_correlation,
                         loop_limit=10L, overrides=list(), clean_step=list(),
                         chain_id=1L, verbose=FALSE) {

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

    if (nrow(model) == 0)
        return(list(converged=NULL,
                    imputed=imputed,
                    iterations=0L,
                    oob_error=oob_error,
                    stop_measures=stop_measures[-1L]))

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
            clean_step=clean_step
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
            converged <- T
            break
        }

        if (verbose) setTxtProgressBar(pb, j)

    }

    list(converged=converged,
         imputed=imputed,
         iterations=length(stop_measures) - 1L,
         oob_error=oob_error,
         stop_measures=stop_measures[-1L])

}

