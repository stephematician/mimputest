#' Perform missForest iteration
#'
#' Perform the missForest (Stekhoven and Buehlmann, 2012) iterative procedure
#' to impute missing data using random forests. The ranger (Wright and Ziegler,
#' 2017) fast implementation of random forest (training) algorithm is used. Some
#' key alterations to the missForest algorithm may be specified by the user.
#'
#' For a full description of the missForest algorithm, see Stekhoven and
#' Buehlmann (2012). In brief, at each iteration missing values are imputed for
#' each variable (in the order of \code{rownames(model)}) by the predictions of
#' a random forest trained on the observed cases of that variable along with the
#' completed data set of the previous iteration as the value of the predictors.
#' This is repeated until some measure of the relationship between iterations
#' indicates convergence - usually by decreasing from the measure at the
#' previous iteration.
#'
#' Numeric data is treated as continuous and predicted by regression forests
#' while factors are predicted via classification forests. When called from
#' \code{smirf} only numeric (non-integer) and factor and ordered data are
#' present (integer and logical types having been converted to factors).
#'
#' The key modifications to the procedure governed by the arguments \describe{
#'     \item{\code{gibbs}}{use the most recent predictions for each variable
#'         in training and prediction as they become available, like a Gibbs
#'         sampler by setting this to \code{T} (default is \code{F};}
#'     \item{\code{obs.only}}{train on all rows in the data set instead of
#'         observed only by setting this to \code{F} (default is \code{T}),
#'         and;}
#'     \item{\code{tree.imp}}{predict using a randomly selected tree for each
#'         missing value rather than use the whole-of-forest aggregated
#'         prediction by setting this to \code{T} (default is \code{F}).}
#' }
#'
#' Collectively, these three changes make the procedure similar to the
#' Multiple Imputation via Chained Equations of van Buuren and
#' Groothuis-Oudshoorn, (2012).
#'
#' The convergence criterion can be modified by the \code{stop.measure}
#' argument. The default is to measure the mean rank correlation between
#' iterations of the ordered data and the stationary rate of the categorical
#' data (see \code{\link{measure_correlation}}. The procedure halts when both of
#' these values are less than or equal to the previous values (see
#' \code{\link{stop_condition}}). The original Stekhoven and Buehlmann (2012)
#' measure is provided by the \code{\link{measure_stekhoven_2012}} function.
#'
#' @inheritParams smirf
#' @param X_init data.frame;
#'            a data set including any of numeric, logical, integer, factor and
#'            ordered data types, to be used as the initial state of the
#'            missForest procedure.
#' @param indicator named list;
#'            an indicator of the missing (\code{=T}) or not-missing (\code{=F})
#'            status of the columns of \code{X_init}.
#' @param ranger_call call;
#'            skeleton call to \code{\link[ranger]{ranger}} for fitting random
#'            forests during the missForest iterative procedure, arguments can
#'            be over-ridden on a per-variable basis by \code{overrides}.
#' @return named list;
#'             results of the iterative procedure given as; \describe{
#'                 \item{\code{converged}}{logical; indicator of convergence;}
#'                 \item{\code{oob_error}}{data.frame; variable-wise out-of-bag
#'                     error at each iteration described by columns;
#'                     \describe{
#'                         \item{\code{iteration}}{numeric.}
#'                         \item{\code{variable}}{factor; name of column in data
#'                             set.}
#'                         \item{\code{measure}}{factor; one of \code{mse} (mean
#'                             square error) for non-integer numeric data or
#'                             \code{pfc} (proportion falsely classified).}
#'                         \item{\code{value}}{numeric; out of bag error.}
#'                     }
#'                 }
#'                 \item{\code{stop_measures}}{list; containing the value
#'                     returned by \code{stop.measure} at each iteration.}
#'                 \item{\code{imputed}}{list; each item is a named list of
#'                     imputed values at each iteration, in order of appearance
#'                     in X_init.}
#'             }
#'
#' @seealso \code{\link{measure_correlation}} \code{\link{measure_stekhoven_2012}}
#'          \code{\link[missForest]{missForest}} \code{\link[ranger]{ranger}}
#'          \code{\link{stop_condition}}
#'
#' @references
#'
#' Stekhoven, D.J. and Buehlmann, P., 2012. MissForest--non-parametric
#' missing value imputation for mixed-type data. \emph{Bioinformatics, 28}(1),
#' pp. 112-118.
#' \href{https://dx.doi.org/10.1093/bioinformatics/btr597}{doi.1.1093/bioinformatics/btr597}
#'
#' Van Buuren, S. and Groothuis-Oudshoorn, K., 2011. mice: Multivariate
#' Imputation by Chained Equations in R. _Journal of Statistical Software,
#' 45_(3). pp. 1-67.
#' \href{https://dx.doi.org/10.18637/jss.v045.i03}{doi.10.18637/jss.v045.i03}
#'
#' Wright, M. N. and Ziegler, A., 2017. ranger: A fast implementation of random
#' forests for high dimensional data in C++ and R. \emph{Journal of Statistical
#' Software, 77}(i01), pp. 1-17. \href{https://dx.doi.org/10.18637/jss.v077.i01}{doi.10.18637/jss.v077.i01}
#'
#' @keywords internal
sampler_loop <- function(data, model, indicator, call_lr_train, sampler,
                         prediction_type, stop_measure=measure_correlation,
                         loop_limit=10L, overrides=list(), clean_step=list(),
                         chain_id=1, verbose=FALSE) {

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
                    stop_measures=stop_measures[-1]))

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
            clean_step=clean_step, iter_id=j
        )

        imputed[[j+1]] <- result_j$imputed
        for (response_k in rownames(model)) {
            indicator_k <- indicator[[response_k]]
            data[indicator_k,response_k] <- imputed[[j+1]][[response_k]]
        }
        oob_error <- rbind(oob_error, result_j$oob_error)

        stop_measures[[j+1]] <- stop_measure(imputed[[j]], imputed[[j+1]],
                                             data, indicator)

        if (all_measures_lte(stop_measures[[j]], stop_measures[[j+1]])) {
            if (verbose) setTxtProgresBar(pb, loop_limit)
            converged <- T
            break
        }

        if (verbose) setTxtProgresBar(pb, j)

    }

    list(converged=converged,
         imputed=imputed,
         iterations=length(stop_measures)-1L,
         oob_error=oob_error,
         stop_measures=stop_measures[-1])

}

