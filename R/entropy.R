#' Calculate entropy of categorical data
#'
#' Calculate entropy of categorical data using tally of observations in each
#' category.
#'
#' Calculates the entropy of categorical data using a tally of observations in
#' each category, using the following formula: \deqn{
#'    H = - \frac{1}{\sum_j f_j} \sum_k f_k \log_2 \frac{f_k}{\sum_j f_j},
#' }
#' where \eqn{f_k} is the frequency in category indexed by \eqn{k}.
#'
#' @param x ordered;
#'            vector of categorical data.
#' @param freq numeric;
#'            count of observations in each group.
#' @return numeric;
#'             scalar value of entropy.
#'
#' @keywords internal
entropy <- function(x, freq=table(x))
    -sum(freq * log2(freq / sum(freq)), na.rm=T) / sum(freq)

