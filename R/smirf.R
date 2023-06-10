# Copyright (c) 2018-2023, Stephen Wade. All rights reserved.

#' Single or multiple imputation of missing data using random forests
#'
#' Missing data (multiple) imputation using the missForest algorithm by
#' Stekhoven and Buehlmann (2012) (default) or, alternatively, the MICE with
#' random forest procedure of Doove et al (2014). The ranger (Wright and
#' Ziegler, 2017) fast implementation of random forest (training) algorithm is
#' used.
#'
#' For a full description of the missForest algorithm, see Stekhoven and
#' Buehlmann (2012). In brief, at each iteration missing values are imputed for
#' each variable by the predictions of a random forest trained on the observed
#' cases of that variable using the values of predictors from the completed data
#' set from the previous iteration. This is repeated until some measure of the
#' relationship between iterations indicates convergence - usually by decreasing
#' from the measure at the previous iteration.
#'
#' By default the columns are imputed in the order of least missing to most
#' missing. This can be over-ridden by the \code{model} argument. Columns that
#' are entirely missing are excluded. Non-integer numeric data is treated as
#' continuous and predicted by regression forests while all other data,
#' including integer and logical data, are predicted via classification forests.
#' No special treatment is given to ordered categorical data.
#'
#' The call to \code{\link[ranger]{ranger}} may be modified by the \code{...}
#' arguments, and any variable-specific argument to pass may be specified in the
#' \code{overrides} argument.
#'
#' The key modifications to the missForest procedure governed by the arguments:
#' \describe{
#'     \item{\code{gibbs}}{use the most recent predictions for each variable
#'         in training and prediction as they become available, like a Gibbs
#'         sampler by setting this to \code{T} (default is \code{F});}
#'     \item{\code{tree.imp}}{predict using a randomly selected tree for each
#'         missing value rather than use the whole-of-forest aggregated
#'         prediction by setting this to \code{T} (default is \code{F});}
#'     \item{\code{boot.train}}{train on a boot-strapped resample of the data,
#'         (default is \code{F}), and;}
#'     \item{\code{obs.only}}{train on all rows in the data set by setting to
#'         \code{F} or train on observed data only (default is \code{T}).}
#' }
#'
#' Switching the first two to \code{T} invokes a similar procedure to Multiple
#' Imputation via Chained Equations of Doove et al (2014). The third option
#' can be used to improve CI coverage (Bartlett 2014). The final option (along
#' with changes to the first two) will mimic van Buuren and
#' Groothuis-Oudshoorn (2012), except for the process for drawing values from
#' leaf nodes.
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
#' @param model matrix;
#'            logical matrix which indicates inclusion of a predictor (named
#'            column) in the model of an imputed value (named row), with the
#'            order of imputation being the row order, default is a matrix of
#'            ones with rows for each partially but not-completely missing
#'            variable (in order of least to most missing), and columns
#'            for every partially complete variable.
#' @param n numeric scalar;
#'            the number of imputations - i.e. number of times the missForest
#'            algorithm is used.
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
#'                 }
#'                 \item{which_imputed}{named list of which rows the imputed
#'                     named data belong to.}
#'             }
#'
#' @seealso \code{\link{measure_correlation}}
#'          \code{\link{measure_stekhoven_2012}}
#'          \code{\link{stop_condition}}
#'          \code{\link{no_information_impute}} \code{\link{sample_impute}}
#'          \code{\link[missForest]{missForest}}
#'          \code{\link[ranger]{ranger}}
#'
#' TODO: check CALIBERrfimpute - some settings differ!
#' also get up to speed with: https://academic.oup.com/aje/article/179/6/764/107562
#'
#' @references
#'
#' -   Bartlett, J. (2014). Methodology for multiple imputation for missing data
#'     in electronic health record data, presented to _27th International
#'     Biometric Conference_, Florence, July 6-11.
#' -   Doove, L.L., Van Buuren, S., & Dusseldorp, E. (2014). Recursive
#'     partitioning for missing data imputation in the presence of interaction
#'     effects. _Computational Statistics & Data Analysis_, 72, 92-104.
#'     \doi{10.1016/j.csda.2013.10.025}.
#' -   Shah, A. D., Bartlett, J. W., Carpenter, J., Nicholas, O., & Hemingway,
#'     H. (2014). Comparison of random forest and parametric imputation models
#'     for imputing missing data using MICE: a CALIBER study. _American journal
#'     of epidemiology_, 179(6), 764-774. \doi{10.1093/aje/kwt312}.
#' -   Stekhoven, D. J. & Buehlmann, P. (2012). MissForest--non-parametric
#'     missing value imputation for mixed-type data. _Bioinformatics_, 28(1),
#'     112-118. \doi{10.1093/bioinformatics/btr597}.
#' -   Wright, M. N., & Ziegler, A. (2017). ranger: A fast implementation of
#'     random forests for high dimensional data in C++ and R. _Journal of
#'     Statistical Software_, 77(i01), 1-17. \doi{10.18637/jss.v077.i01}
#'
#'
#' @importFrom rlang call2 call_modify eval_tidy
#' @importFrom magrittr %<>% %>%
#' @importFrom literanger train
#' @export
#' @md
smirf <- function(data,
                  model=NULL,
                  n=5L,
                  sampler=c('gibbs', 'missforest'),
                  prediction_type=c('inbag', 'bagged'),
                  fn_init=impute_naive_by_sample,
                  stop_measure=measure_degenerate,
                  loop_limit=10L,
                  overrides=list(),
                  clean_step=list(),
                  verbose=FALSE,
                  ...) {

    lr_train_args <- modifyList(
        list(), # default arguments to literanger::train
        enquos(...)
    )
    # TODO: warn if not named

    sampler <- match.arg(sampler)
    prediction_type <- match.arg(prediction_type)

    check_args(
        data=data, model=model, n=n, use_imputed=use_imputed, fn_init=fn_init,
        stop_measure=stop_measure, loop_limit=loop_limit, overrides=overrides,
        clean_step=clean_step, verbose=verbose
    )

    n_obs <- nrow(data)

    if (!identical(round(n), as.numeric(n)))
        warning('Non-integer value of \'n\' supplied.')

    if (!identical(round(loop_limit), as.numeric(loop_limit)))
        warning('Non-integer value of \'loop_limit\' supplied.')

    if (inherits(data, 'grouped_df'))
        warning('Groups in \'data\' will be ignored.')

  # store location of missing data as list
    indicator <- lapply(data, is.na)

  # filter completely missing variables
    n_miss <- sapply(indicator, sum)
    if (any(n_miss == n_obs))
        warning('Excluding the following entirely missing data in \'data\': ',
                paste(names(n_miss)[n_miss == n_obs], collapse=', '), '.')

    included <- names(n_miss)[n_miss != n_obs]
    indicator <- indicator[included]
    n_miss <- n_miss[included]

  # default model includes all covariates, and imputes in the order from least
  # to most missing values
    if (is.null(model)) {
        model <- matrix(1, nrow=length(included), ncol=length(included),
                        dimnames=list(included, included))
        diag(model) <- 0
        model <- model[order(n_miss)[n_miss > 0], , drop=FALSE]
    }
  # NOTE: Haven't checked here that data is included
  # convert integers and logical values to factors or ordered
    to_categorical <- sapply(data, is.integer) | sapply(data, is.logical)
    inv_tform_categorical <- lapply(data[to_categorical],
                                    make_inv_tform_categorical)
  # fn_init(data, indicator) will construct the initial values for missing data
  # for a single chain
    call_init <- rlang::call2(
        fn_init, data=data[included], indicator=indicator
    )
    call_lr_train <- rlang::call_modify(
        rlang::call2(literanger::train, verbose=verbose),
        !!! lr_train_args
    )

    result <- list()

    for (j in seq_len(n)) {

        data_j <- eval(call_init)
        msgs <- is_valid_initial_state(data_j, data[included])
        if (length(msgs) > 0)
            stop('The following initial data conditions failed: ',
                 paste(msgs, collapse='; '), '.')

        data_j[to_categorical] %<>% lapply(tform_categorical)

        result[[j]] <- sampler_loop(
            data=data_j, model=model, indicator=indicator,
            call_lr_train=call_lr_train, sampler=sampler,
            prediction_type=prediction_type,
            stop_measure=stop_measure, loop_limit=loop_limit,
            overrides=overrides, clean_step=clean_step, chain_id=j,
            verbose=verbose
        )

      # convert imputed values in each chain back to integer/logical
        result[[j]]$imputed <- lapply(
            res[[j]]$imputed, apply_inv_tform_categorical,
            inv_tform_categorical=inv_tform_categorical
        )

    }

    list(call=match.call(), # TODO: need a better approach to this
         model=model,
         results=lapply(result,
                        post_process_missforest,
                        to_categories=to_categories),
         which_imputed=lapply(indicator, which))

}

