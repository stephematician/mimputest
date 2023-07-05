# SPDX-FileCopyrightText: 2023 Stephen Wade <stephematician@gmail.com>
# SPDX-License-Identifier: MIT

#' Convert imputed datasets to `mids` object.
#'
#' To take advantage of generics and other functions available in 'mice' (and
#' other packages) the output from [mimputest()] can be converted into a `mids`
#' object. Some functionality may be broken due to non-equivalence of
#' 'mimputest' and 'mice' syntax.
#'
#' See [mice::mids()] for details on the `mids` class. This function uses
#' several 'mice' functions to construct a skeleton `mids` object that reflects
#' a near-equivalent call in 'mice' to generate the imputed data. Some
#' functionality for `mids` objects may be missing because an exact equivalent
#' is not possible. For example `blots` and `post` are provided but not using
#' syntax that [mice::mice()] would accept. `seed` is recorded as per
#' [mimputest()] and `lastSeedValue` is set to `NULL`. The call stored is the
#' original call to [mimputest()].
#'
#' @param x list (named, class 'mimputest'): imputed datasets as returned by
#' [mimputest()].
#' @returns a mids object, see [mice::mids] for details.
#'
#' @seealso [mice::mids]
#'
#' @export
#' @md
as_mids <- function(x) {

    stopifnot(inherits(x, 'mimputest'))

    blocks <- mice::make.blocks(x$data, partition='scatter')[rownames(x$model)]

  # Construct a 'skeleton' mids object using [mice()].
    result <- mice::mice(
        data=x$data, m=x$n,# predictorMatrix=x$model, # predictorMatrix,
        where=do.call(cbind, x$indicator),
        blocks=blocks, visitSequence=rownames(x$model),
        formulas=mice::make.formulas(x$data, blocks=blocks,
                                     predictorMatrix=x$model), #predictorMatrix),
        method='rf', maxit=0,
      # from as.mids and edit.setup
        remove.collinear=FALSE, remove.constant=FALSE, allow.na=TRUE
    )

    stopifnot(x$n_missing == result$nmis[names(x$n_missing)])

    statistics_as_array <- function(x, measure='mean') {
        df <- x$statistics[
            x$statistics$measure %in% measure & x$statistics$iteration != 0L,
            c('iteration', 'variable', 'value')
        ] %>%
        stats::reshape(direction='wide', idvar='variable', timevar='iteration')
        dn <- c(unname(df[1]), list(sub('value\\.', '', names(df)[-1])))
        as.matrix(df[,-1]) %>% 'dimnames<-'(dn)
    }

    iteration <- sapply(x$measures, nrow)

    sampler_means <- lapply(x$statistics, statistics_as_array, 'mean')
    sampler_vars <- lapply(x$statistics, statistics_as_array, 'var')

    dim_means <- c(nrow(sampler_means[[1]]), max(iteration), x$n)
    dn_means <- list(rownames(sampler_means[[1]]),
                     as.character(1:max(iteration)),
                     NULL)
    dim_vars <- dim_means
    dim_vars[[1]] <- nrow(sampler_vars[[1]])
    dn_vars <- dn_means
    dn_vars[[1]] <- rownames(sampler_vars[[1]])

    chainMean <- array(NA_real_, dim=dim_means, dimnames=dn_means)
    chainVar <- array(NA_real_, dim=dim_vars, dimnames=dn_vars)

    for (j in seq_len(x$n)) {
        j_means <- sampler_means[[j]]
        j_vars <- sampler_vars[[j]]
        chainMean[1:nrow(j_means), 1:ncol(j_means),j] <- j_means
        chainVar[1:nrow(j_vars), 1:ncol(j_vars),j] <- j_vars
    }

    bind_imputed <- function(x, y) mapply(c, x, y, SIMPLIFY=F)

    n_miss <- x$n_miss[rownames(x$model)]
    dn <- list(NULL, as.character(1:x$n))
  # Now incorporate the [mimputest()] result given by `x`.
    modifyList(
        result,
        list(imp=Reduce(bind_imputed, x$imputed) %>%
                     mapply(matrix, ., nrow=n_miss,
                            MoreArgs=list(dimnames=dn), SIMPLIFY=F) %>%
                     lapply(as.data.frame),
             m=x$n,
             call=x$call,
             post=x$call$clean_step, blots=x$call$overrides,
             seed=x$seed,
             iteration=iteration,
             lastSeedValue=NULL,
             chainMean=chainMean,
             chainVar=chainVar,
             version=x$package_version, date=as.Date(x$timestamp)
        )
    )

}

