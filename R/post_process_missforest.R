# Post process a single imputation
post_process_missforest <- function(res, to_categories) {

    iterations_ <- unname(mapply(list,
                                 iteration=seq_len(1L + res$iterations) - 1L,
                                 SIMPLIFY=F))

    aggregates <- mapply(post_process_iteration,
                         imputed=res$imputed,
                         iteration=iterations_,
                         MoreArgs=list(to_categories=to_categories),
                         SIMPLIFY=F)

    # bind all the iterations together
    c(res,
      list(frequency=do.call(rbind,
                             lapply(aggregates, getElement, 'frequency')),
           statistics=do.call(rbind,
                              lapply(aggregates, getElement, 'statistics'))))

}

