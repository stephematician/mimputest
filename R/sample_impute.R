#' Impute missing data using sample from complete cases
#'
#' Imputes missing data in a data.frame by sampling with replacement from the
#' complete cases in each column.
#'
#' This is a similar initial guess as that employed by Multiple Imputations
#' by Chained Equation (van Buuren and Groothuis-Oudshoorn, 2012). This has a
#' higher entropy than the initial state which would be given by that of
#' missForest (Stekhoven, 2012).
#'
#' @inheritParams miForang
#' @param indicator named list;
#'            indicator of missing (\code{=T}) and not-missing (\code{=F})
#'            status for each column in \code{X}.
#' @return data.frame; the same as \code{X} except for missing values in each
#'             column being replaced by a random (with replacement) sample of
#'             the complete cases.
#'
#' @seealso \code{\link{miForang}} \code{\link[missForest]{missForest}}
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
#' @examples
#' \dontrun{
#' # simply pass to miForang
#' miForang(iris, X.init.fn=sample_impute)
#' }
#' sample_impute(data.frame(x=c(0,1,NA)))
#' @export
sample_impute <- function(X, indicator=lapply(X, is.na)) {

    indices <- mapply(sample,
                      x=lapply(indicator, function(x) which(!x)),
                      size=lapply(indicator, sum),
                      MoreArgs=list(replace=T),
                      SIMPLIFY=F)

    X[] <- mapply(`[<-`,
                  X,
                  indicator,
                  value=mapply(`[`, X, indices, SIMPLIFY=F),
                  SIMPLIFY=F)
    X
 
}

