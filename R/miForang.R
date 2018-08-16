#' Missing data imputation using fast implemention of random forest
#'
#' Missing data (multiple) imputation using the missForest algorithm by
#' Stekhoven and Buehlmann (2012). The ranger (Wright and Ziegler, 2017) fast
#' implementation of random forest (training) algorithm is used. Some
#' key alterations to the missForest algorithm may be specified by the user.
#'
#' For a full description of the missForest algorithm, see Stekhoven and
#' Buehlmann (2012). In brief, at each iteration missing values are imputed for
#' each variable (in the order of \code{order.impute}) by the predictions of a
#' random forest trained on the observed cases of that variable along with the
#' completed data set of the previous iteration as the value of the predictors.
#' This is repeated until some measure of the relationship between iterations
#' indicates convergence - usually by decreasing from the measure at the
#' previous iteration.
#'
#' By default the columns are imputed in the order of least missing to most
#' missing. This can be over-ridden by the \code{order.impute} argument. Columns
#' that are entirely missing are excluded. Non-integer numeric data is treated
#' as continuous and predicted by regression forests while all other data,
#' including integer and logical data, are predicted via classification forests.
#' No special treatment is given to ordered categorical data.
#'
#' The call to \code{\link[ranger]{ranger}} may be modified by the \code{...}
#' arguments, and any variable-specific argument to pass may be specified in the
#' \code{overrides} argument.
#'
#' The key modifications to the missForest procedure governed by the arguments
#' \describe{
#'     \item{\code{gibbs}}{use the most recent predictions for each variable
#'         in training and prediction as they become available, like a Gibbs
#'         sampler by setting this to \code{T} (default is \code{F};}
#'     \item{\code{obs.only}}{train on all rows in the data set instead of
#'         observed only by setting this to \code{F} (default is \code{T}),
#'         and;}
#'     \item{\code{obs.only}}{train on all rows in the data set instead of
#'         observed only by setting this to \code{F} (default is \code{T}),
#'         and;}
#'     \item{\code{tree.imp}}{predict using a randomly selected tree for each
#'         missing value rather than use the whole-of-forest aggregated
#'         prediction by setting this to \code{T} (default is \code{F}).}
#' }
#'
#' Coming the first two changes make the procedure similar to the Multiple
#' Imputation via Chained Equations of Doove et al (2014). Experimentally,
#' adding the final modification makes the procedure mimic van Buuren and
#' Groothuis-Oudshoorn, (2012), however it does not seem advisable to train a
#' random forest on predicted data. The third item follows the suggestion of
#' Bartlett (2014) to improve CI coverage.
#'
#' The convergence criterion can be modified by the \code{stop.measure}
#' argument. The default is to measure the mean rank correlation between
#' iterations of the ordered data and the stationary rate of the categorical
#' data (see \code{\link{measure_correlation}}. The procedure halts when both of
#' these values are less than or equal to the previous values (see
#' \code{\link{stop_condition}}). The original Stekhoven and Buehlmann (2012)
#' measure is provided by the \code{\link{measure_stekhoven_2012}} function.
#'
#' @param X data.frame;
#'            a incomplete data set including any of numeric, logical, integer,
#'            factor and ordered data types.
#' @param n numeric scalar;
#'            the number of imputations - i.e. number of times the missForest
#'            algorithm is used.
#' @param order.impute character vector;
#'            (optional) order to impute columns - default value given by
#'            increasing rate of missing data in \code{X}.
#' @param gibbs logical;
#'            use Gibbs sampling in training steps (\code{T}) rather than the
#'            predictions from the previous iteration (default).
#' @param tree.imp logical;
#'            use a prediction of missing data from single tree in the forest 
#'            when training (\code{T}) rather than the bagged predicted value
#'            (default).
#' @param boot.train logical;
#'            train each forest on a bootstrap sample of the observed data
#'            when \code{T}, rather than the observed data (default).
#' @param obs.only logical;
#'            train on only observed outcomes (default) or use all data
#'            including predicted/sampled values of missing outcomes (\code{T}).
#' @param verbose logical;
#'            print additional output.
#' @param X.init.fn function;
#'            creates a completed data set to be used as the initial state of
#'            the missForest procedure given two arguments; \itemize{
#'                \item a data.frame
#'                \item a list with and item indicating the missing (\code{T})
#'                      or not-missing (\code{F}) status of each column of the
#'                      first argument,
#'            } the default \code{\link{no_information_impute}} serves as an
#'            example.
#' @param stop.measure function;
#'            evaluates the difference or relationship between the two most
#'            recently completed data sets during iteration, must accept the
#'            following arguments;
#'            \describe{
#'                 \item{\code{X}}{named list with imputed values (in order of
#'                     appearance by row) for each column in the data set;}
#'                 \item{\code{Y}}{named list with imputed values (in order of
#'                     appearance by row) for each column in the data set;}
#'                 \item{\code{X_init}}{the original (mised-type) data set with
#'                      missing values replaced as at the starting point of
#'                      missForest;}
#'                 \item{\code{indicator}}{a list with the missing (\code{=T})
#'                      or not missing (\code{=F}) status of the original data
#'                      set;}
#'            } and should return a numeric (vector), the default
#'            \code{\link{measure_correlation}} serves as an example, or see the
#'            original measure proposed by Stekhoven and Buehlmann (2012) in
#'            \code{\link{measure_stekhoven_2012}}.
#' @param loop.limit numeric;
#'            maximum number of iterations within missForest procedure. 
#' @param overrides named list;
#'            (variable-wise) over-rides for arguments passed to
#'            \code{\link[ranger]{ranger}} when training on the response
#'            variable given by the name of the item.
#' @param clean.step named list;
#'            each item is a function to clean or post-process the named imputed
#'            data immediately after it is imputed, taking two arguments;
#'            \itemize{
#'                \item the subset of the data used in the current training step
#'                      which had missing values of the named data,
#'                \item the most recently imputed values of the named data,
#'            } and should return (post-processed) data of the same length and
#'            type as the second argument.
#' @param ... further arguments passed to all calls to 
#'            \code{\link[ranger]{ranger}}, e.g. \code{num.trees} for the number
#'            of trees in each forest.
#' @return list;
#'             containing the following items; \describe{
#'                 \item{call}{the call used to create the multiply imputed
#'                     data sets;}
#'                 \item{results}{list where each item (numbered) is itself a
#'                     named list of the output for an imputed data set;
#'                     \describe{
#'                         \item{\code{converged}}{boolean convergence status;}
#'                         \item{\code{imputed}}{list of imputed data by
#'                             iteration and variable;}
#'                         \item{\code{iterations}}{numeric count of iterations
#'                             before stopping criteria met;}
#'                         \item{\code{oob_error}}{list of oob error by
#'                             iteration and variable;}
#'                         \item{\code{stop_measures}}{output of the call to
#'                             \code{stop.measure} at each iteration;}
#'                     }
#'                 \item{which_imputed}{named list of which rows the imputed
#'                     named data belong to.}
#'             }
#'
#' @section To-do:
#'
#' The post-processing of the output needs to be determined. It may be useful to
#' prepare a \code{\link[mice]{mids}} object so that the powerful tools that
#' have been developed there might be used on data sets generated by miForang.
#'
#' @references
#'
#'
#' Bartlett, J., 2014. 'Methodology for multiple imputation for missing data in
#' electronic health record data', presented to _27th International Biometric
#' Conference_, Florence, July 6-11.
#'
#' Doove, L.L., Van Buuren, S. and Dusseldorp, E., 2014. Recursive partitioning
#' for missing data imputation in the presence of interaction effects. 
#' \emph{Computational Statistics & Data Analysis, 72}, pp. 92-104.
#' \href{https://dx.doi.org/10.1016/j.csda.2013.10.025}{doi.10.1016/j.csda.2013.10.025}
#'
#' Stekhoven, D.J. and Buehlmann, P., 2012. MissForest--non-parametric
#' missing value imputation for mixed-type data. \emph{Bioinformatics, 28}(1),
#' pp. 112-118.
#' \href{https://dx.doi.org/10.1093/bioinformatics/btr597}{doi.1.1093/bioinformatics/btr597}
#'
#' Wright, M. N. and Ziegler, A., 2017. ranger: A fast implementation of random
#' forests for high dimensional data in C++ and R. \emph{Journal of Statistical
#' Software, 77}(i01), pp. 1-17. \href{https://dx.doi.org/10.18637/jss.v077.i01}{doi.10.18637/jss.v077.i01}
#'
#' @seealso \code{\link{measure_correlation}}
#'          \code{\link{measure_stekhoven_2012}}
#'          \code{\link[missForest]{missForest}}
#'          \code{\link{no_information_impute}} \code{\link[ranger]{ranger}}
#'          \code{\link{sample_impute}} \code{\link{stop_condition}}
#'
#' @section To-do:
#' \itemize{
#'   \item consider run-time like tests for \code{stop.measure}
#'   \item document \code{miForang} further
#' }
#'
#' @export
miForang <- function(X,
                     n=5L,
                     order.impute=NULL,
                     gibbs=F,
                     tree.imp=F,
                     boot.train=F,
                     obs.only=T,
                     verbose=F,
                     X.init.fn=no_information_impute,
                     stop.measure=measure_correlation,
                     loop.limit=10L,
                     overrides=list(),
                     clean.step=list(),
                     ...) {

    ranger_args <- enquos(...)

    when_verbose_print <- function(...) if (verbose) print(...)

    check_miForang_args(         X,            n, order.impute,     gibbs,
                          tree.imp,   boot.train,     obs.only,   verbose,
                         X.init.fn, stop.measure,   loop.limit, overrides,
                        clean.step)

    n_obs <- nrow(X)

    if (!identical(floor(n), as.numeric(n)))
        warning('non-integer value of n supplied.')

    if (!identical(floor(loop.limit), as.numeric(loop.limit)))
        warning('non-integer value of loop.limit supplied.')

    if (inherits(X, 'grouped_df'))
        warning('groups in X will be ignored.')

    # store location of missing data as list
    indicator <- lapply(X, is.na)

    # filter completely missing variables
    missing_tally <- sapply(indicator, sum)
    if (any(missing_tally == n_obs))
        warning(paste0('excluding the following entirely missing data in X:',
                       '\n  - ',
                       paste(names(missing_tally)[missing_tally == n_obs], 
                             collapse=', '), '.'))

    v_use <- names(missing_tally)[missing_tally != n_obs]
    indicator <- indicator[v_use]
    missing_tally <- missing_tally[v_use]

    # sort columns by least to most missing
    if (is.null(order.impute)) {
        order.impute <- v_use[order(missing_tally)]
        order.impute <- order.impute[missing_tally[order.impute] > 0]
    }

    # Convert integers and logical values to factors
    to_categories <- get_maps_to_categories(X[v_use])
    to_categorical_functions <- lapply(X[names(to_categories)],
                                       function(j, x)
                                           unname(x[[storage.mode(j)]]),
                                       x=list('integer'=as.ordered,
                                              'logical'=as.factor))

    # invoke as.data.frame here due to ranger not supporting tibble argument
    X_init_call <- call2(X.init.fn,
                         X=as.data.frame(X[v_use]),
                         indicator=indicator)

    ranger_call <- call_modify(call2(ranger::ranger,
                                     write.forest=T,
                                     verbose=verbose),
                               !!! ranger_args)

    res <- list()
    when_verbose_print('miForang: begin imputations')

    which_imputed <- lapply(indicator, which)

    for (j in seq_len(n)) {

        X_init <- eval_tidy(X_init_call)
        msgs <- is_valid_initial_state(X_init, X[v_use])
        if (length(msgs) > 0)
            stop(paste(msgs, collapse='\n'))

        X_init[names(to_categories)] <- mapply(function(f, x) f(x),
                                               to_categorical_functions,
                                               X_init[names(to_categories)],
                                               SIMPLIFY=F)

        res[[j]] <- perform_missforest(      X_init, indicator,  ranger_call,
                                       order.impute,     gibbs,     tree.imp,
                                         boot.train,  obs.only, stop.measure,
                                         loop.limit, overrides,   clean.step)

        # convert back to integer/logical
        res[[j]]$imputed <- lapply(res[[j]]$imputed,
                                   unmap_categories,
                                   to_categories)

        when_verbose_print(paste('  - imputation', j, 'complete.'))
    }

    list(call=match.call(),
         results=lapply(res,
                        post_process_missforest,
                        to_categories=to_categories),
         which_imputed=which_imputed)

}

