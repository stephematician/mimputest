# SPDX-FileCopyrightText: 2023 Stephen Wade <stephematician@gmail.com>
# SPDX-License-Identifier: MIT

#' Calculate entropy of categorical data.
#'
#' Calculates the entropy of categorical data using a tally of observations in
#' each category, using the following formula: \deqn{%
#'    H = - \frac{1}{\sum_j f_j} \sum_k f_k \log_2 \frac{f_k}{\sum_j f_j},%
#' }{ H = -\sum [f[k] log2 (f[k] / \sum f[j])] / sum f[j], }
#'
#' where \eqn{f_k}{f[k]} is the frequency in category indexed by \eqn{k}.
#'
#' @param x ordered; vector of categorical data.
#' @param freq numeric: count of observations in each group.
#' @return numeric (scalar): value of entropy.
#'
#' @noRd
#' @md
entropy <- function(x, freq=table(x))
    -sum(freq * log2(freq / sum(freq)), na.rm=T) / sum(freq)

#' Calculate Leik's D of ordered data
#'
#' Calculates the dispersion via the Leik's D statistic for ordinal data using
#' the following formula; \deqn{%
#'    D = \frac{2}{n - 1} \left(%
#'            \sum_{k : F_k < 0.5} F_k + \sum_{k : F_k \geq 0.5} 1 - F_k%
#'        \right),%
#' }{ D = 2 \sum (F[k[] if F[k] < 0.5 or 1 - F[k] if F[k] > 0.5) / (n - 1) }
#'
#' where \eqn{n} is the number of categories, and \eqn{F_k}{F[k]} is the
#' cumulative distribution for category indexed by \eqn{k}, except in the case
#' of one category, where the value 0 is returned.
#'
#' @param x ordered: ordered data or data that can be coerced into ordered via
#'`as.factor()` and `factor()` with `ordered=TRUE`.
#' @return numeric: scalar value of Leik's D
#'
#' @noRd
#' @md
leiks_D <- function(x) {

    x <- as.factor(x)
    freq <- table(factor(x, levels=levels(x), ordered=T))
    cumfreq <- cumsum(freq)
    n <- sum(freq)
    cumfreq <- cumfreq[-length(cumfreq)]

    2 * sum(cumfreq + (n - 2 * cumfreq) * as.numeric(cumfreq >= (n / 2))) /
        (max(length(cumfreq), 1) * n)

}

# returns type-respecting most frequent values
most_frequent_values <- function(x, table_x=table(x)) {
    if (any(table_x > 0))
        x[sapply(which((table_x) == max(table_x)),
                 function(j) match(names(table_x)[j], x))]
    else
        x[0]
}

#' @param x a sample of imputed values of a variable
#' @param y a sample of imputed values of a variable
#' @param obs observed values of a variable
#' @noRd
#' @importFrom stats cor
ordered_correlation <- function(x, y, obs, method='spearman')
    cor(xtfrm(c(x, obs)), xtfrm(c(y, obs)), method=method)


#' @inheritParams ordered_correlation
#' @param correct Apply bias correction <https://en.wikipedia.org/wiki/Cram%C3%A9r%27s_V>
#' @importFrom stats chisq.test
#' @noRd
#' @md
cramer_v <- function(x, y, obs, correct=TRUE) {

    if (length(unique(c(x, obs))) > 1) {
        n <- length(x) + length(obs)
        k <- nlevels(obs)
        chi2 <- suppressWarnings(
            chisq.test(c(x, obs), c(y, obs), correct=FALSE)
        )$statistic
        if (correct) {
            chi2_transformed <- pmax(0, (n - 1) * chi2 / n - (k - 1)^2)
            sqrt(chi2_transformed / ((k - 1) * (n - k)))
        } else {
            sqrt(chi2 / (n * (k - 1)))
        }
    } else {
        NA
    }

}


#' @inheritParams ordered_correlation
#' @noRd
ssq_difference <- function(x, y, obs) sum((x - y)^2)


#' @inheritParams ordered_correlation
#' @noRd
ssq <- function(x, y, obs) sum(c(x, obs)^2)


#' @inheritParams ordered_correlation
#' @noRd
count_stationary <- function(x, y, obs) sum(x == y) + length(obs)

