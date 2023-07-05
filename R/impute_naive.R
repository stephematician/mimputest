# SPDX-FileCopyrightText: 2023 Stephen Wade <stephematician@gmail.com>
# SPDX-License-Identifier: MIT

#' Impute missing data naively using complete cases.
#'
#' Drawing imputed values without using the fully conditional specification (or
#' other joint model) is performed during the first step of both the 'mice'
#' imputation algorithm (van Buuren and Groothuis-Oudshoorn, 2012) and the
#' 'missForest' estimation algorithm (Stekhoven and Buehlmann, 2012). For the
#' former, sampling from complete cases is used, whereas for the latter, the
#' most-frequent value or mean of the complete cases is used.
#'
#' Two wrappers are provided for this function so that it can be passed as
#' `fn_init` to [mimputest()], these are [impute_naive_by_sample()] and
#' [impute_naive_by_aggregate()]. In either case, the function will modify each
#' column (in order) by replacing missing values with either a sample from the
#' complete cases, or an aggregate of the complete cases (such as the most
#' frequent value). The former is for 'mice'-like imputation, the latter is for
#' 'missForest'.
#'
#' Sampling generates data with higher entropy than given by the aggregate
#' approach. When using a stopping condition, care must be taken that the choice
#' of initial state doesn't introduce a false positive 'stop' in early
#' iterations. We suspect that the 'aggregate' approach will results in better
#' specificity of the stop condition.
#'
#' @inheritParams sampler_loop
#' @param mode character: whether to draw randomly from observed cases
#' (`='sample'`) or to use aggregates such as the mean or most-frequent-value
#' (`='aggregate'`).
#' @return data.frame: an imputed data set with the same structure as the `data`
#' argument.
#'
#' @seealso [impute_naive_by_sample()] [impute_naive_by_aggregate()]
#'
#' @references
#' -   Stekhoven, D.J. & Buehlmann, P. (2012). MissForest--non-parametric
#'     missing value imputation for mixed-type data. _Bioinformatics_, _28_(1),
#'     112-118. \doi{10.1093/bioinformatics/btr597}.
#' -   Van Buuren, S. & Groothuis-Oudshoorn, K. (2011). mice: Multivariate
#'     Imputation by Chained Equations in R. _Journal of Statistical Software_,
#'     _45_(3), 1-67. \doi{10.18637/jss.v045.i03}.
#'
#' @examples
#' impute_naive(data.frame(x=c(0,1,NA)), mode='sample')
#' impute_naive(data.frame(x=c(0,1,NA)), mode='aggregate')
#'
#' @export
#' @md
impute_naive <- function(data,
                         indicator=lapply(data, is.na),
                         mode=c('sample', 'aggregate')) {

    attr_ <- attributes(data)

    data_ <- data

    mode <- match.arg(mode)

    impute_each <- switch(mode,
                          'sample'=sample_complete,
                          'aggregate'=aggregate_complete)

    values <- mapply(impute_each, x=data_, indicator=indicator, SIMPLIFY=F)

    data_[] <- mapply('[<-', data_, indicator, value=values, SIMPLIFY=F)

    attributes(data_) <- attr_

    invisible(data_)

}


#' Impute missing data naively by sampling from complete cases.
#'
#' Drawing imputed values without using the fully conditional specification (or
#' other joint model) is performed during the first step of the 'mice'
#' imputation algorithm (van Buuren and Groothuis-Oudshoorn, 2012). In this
#' case sampling from complete cases is used.
#'
#' This function can be passed directly to [mimputest()] as the value of the
#' `fn_init` argument. This function will iterate over each column in the data
#' and replace missing values by sampling (with replacement) from the complete
#' cases.
#'
#' This approach may improve the sensitivity (but lower the specificity) in some
#' stopping conditions used by the main algorithm loop during early iterations,
#' owing to the entropy being closer to that of the converged process.
#'
#' @inheritParams impute_naive
#' @return data.frame: an imputed data set with the same structure as the `data`
#' argument.
#'
#' @seealso [impute_naive()]
#'
#' @references
#' -   Van Buuren, S. & Groothuis-Oudshoorn, K. (2011). mice: Multivariate
#'     Imputation by Chained Equations in R. _Journal of Statistical Software_,
#'     _45_(3), 1-67. \doi{10.18637/jss.v045.i03}.
#'
#' @examples
#' \dontrun{
#' mimputest(iris, fn_init=impute_naive_by_aggregate)
#' }
#' impute_naive_by_sample(data.frame(x=c(0,1,NA)))
#'
#' @export
#' @md
impute_naive_by_sample <- function(data, indicator=lapply(data, is.na))
    impute_naive(data, indicator, mode='sample')


#' Impute missing data naively using aggregate of complete cases.
#'
#' Drawing imputed values without using the fully conditional specification (or
#' other joint model) is performed during the first step of the 'missForest'
#' estimation algorithm (Stekhoven and Buehlmann, 2012). In this case, the
#' the most-frequent value or mean of the complete cases is used.
#'
#' This function can be passed directly to [mimputest()] as the value of the
#' `fn_init` argument. This function will iterate over each column in the data
#' and replace missing values in one of two ways depending on the type of the
#' data;
#'
#' -   if the data are continuous then the mean of the complete cases is used;
#' -   if the data are categorical (including logical, integers, etc) then the
#'     most frequent value is used, in the case of ties a single values is
#'     randomly selected from the set of most frequent values.
#'
#' This approach has lowest possible entropy, this may improve the specificity
#' (but reduce the sensitivity) of some stopping conditions in the main
#' algorithm loop during the earlier iterations.
#'
#' @examples
#' \dontrun{
#' mimputest(iris, fn_init=impute_naive_by_aggregate)
#' }
#' impute_naive_by_aggregate(data.frame(x=c(0,1,NA)))
#'
#' @inheritParams impute_naive
#' @return data.frame; an imputed data set with the same structure as the `data`
#' argument.
#'
#' @seealso [impute_naive()]
#'
#' @references
#' -   Stekhoven, D.J. & Buehlmann, P. (2012). MissForest--non-parametric
#'     missing value imputation for mixed-type data. _Bioinformatics_, _28_(1),
#'     112-118. \doi{10.1093/bioinformatics/btr597}.
#'
#' @export
#' @md
impute_naive_by_aggregate <- function(data, indicator=lapply(data, is.na))
    impute_naive(data, indicator, mode='aggregate')


sample_complete <- function(x, indicator)
    sample(x[which(!indicator)], size=sum(indicator), replace=T)


aggregate_complete <- function(x, indicator) {

    if (is.numeric(x) && !is.integer(x)) {
      # treat as continuous
        if (!all(indicator)) mean(x[!indicator]) else numeric()
    } else {
      # treat as categorical
        mfv <- most_frequent_values(x[!indicator])
        n_mfv <- length(mfv)
        if (n_mfv) mfv[sample.int(n_mfv, size=1)] else mfv[0]
    }

}

