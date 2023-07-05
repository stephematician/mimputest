# SPDX-FileCopyrightText: 2023 Stephen Wade <stephematician@gmail.com>
# SPDX-License-Identifier: MIT

#' \pkg{mimputest}: Single or multiple imputation of missing data using random
#' forests.
#'
#' This code is licensed under the MIT license
#' \url{https://www.r-project.org/Licenses/MIT}, and you may use this package
#' strictly under those terms
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
#'     effects. _Computational Statistics & Data Analysis_, 72, 92-104.
#'     \doi{10.1016/j.csda.2013.10.025}.
#' -   Shah, A. D., Bartlett, J. W., Carpenter, J., Nicholas, O., & Hemingway, H.
#'     (2014). Comparison of random forest and parametric imputation models for
#'     imputing missing data using MICE: a CALIBER study. _American Journal of
#'     Epidemiology_, _179_(6), 764-774. \doi{10.1093/aje/kwt312}.
#' -   Stekhoven, D. J. & Buehlmann, P. (2012). MissForest--non-parametric
#'     missing value imputation for mixed-type data. _Bioinformatics_, _28_(1),
#'     112-118. \doi{10.1093/bioinformatics/btr597}.
#' -   Van Buuren, S. & Groothuis-Oudshoorn, K. (2011). mice: Multivariate
#'     Imputation by Chained Equations in R. _Journal of Statistical Software_,
#'     _45_(3), 1-67. \doi{10.18637/jss.v045.i03}.
#' -   Wright, M. N., & Ziegler, A. (2017). ranger: A fast implementation of
#'     random forests for high dimensional data in C++ and R. _Journal of
#'     Statistical Software_, _77_(i01), 1-17.
#'     \doi{10.18637/jss.v077.i01}.
#'
#' [bartlett2014_archive]: https://web.archive.org/web/20190819140612/http://thestatsgeek.com/wp-content/uploads/2014/09/RandomForestImpBiometricsConf.pdf
#'
#' @author Stephen Wade <stephematician@gmail.com>
#'
#' @importFrom literanger train
#' @importFrom magrittr %<>% %>%
#' @importFrom utils globalVariables
#'
#' @keywords internal
#' @docType package
#' @aliases mimputest-package
#' @md
"_PACKAGE"

utils::globalVariables('.')

