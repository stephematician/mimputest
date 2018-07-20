#' Impute missing data using mean or mode of complete cases
#'
#' Imputes missing data in a data.frame using either the complete cases' mean
#' or most frequent value for non-integer numeric and factor columns
#' respectively.
#'
#' This is the same imputation procedure used to determine the initial state
#' of the missForest procedure (Stekhoven and Buehlmann, 2012). In the case of
#' tied most frequent values in a (factor) column, a single value is selected
#' at random from the tied values.
#'
#' @inheritParams miForang
#' @param indicator named list;
#'            indicator of missing (\code{=T}) and not-missing (\code{=F})
#'            status for each column in \code{X}.
#' @return data.frame; the same as \code{X} except for missing values in each
#'             column being replaced by either \itemize{
#'                 \item the mean of the column if the column is non-integer 
#'                       numeric, or;
#'                 \item a randomly selected most frequent value if the column
#'                       is a factor.
#'             }
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
#' @examples
#' \dontrun{
#' # simply pass to miForang
#' miForang(iris, X.init.fn=no_information_impute)
#' }
#' no_information_impute(data.frame(x=c(0,1,NA)))
#' @export
no_information_impute <- function(X, indicator=lapply(X, is.na)) {

    # treat integer values as  factors. used !! to convert empty lists
    continuous_data <- names(X)[!!sapply(X, is.numeric) &
                                    !sapply(X, is.integer)]
    categorical_data <- setdiff(names(X), continuous_data)

    if (length(continuous_data) > 0)
        X[continuous_data] <- mapply(`[<-`, 
                                     X[continuous_data],
                                     indicator[continuous_data],
                                     value=colMeans(X[continuous_data],
                                                    na.rm=T),
                                     SIMPLIFY=F)

    # Original missForest selects a single value (at random) from the
    # most frequently observed values and assigns it to ever missing case.
    if (length(categorical_data) > 0)
        X[categorical_data] <- mapply(
                                   `[<-`,
                                   X[categorical_data],
                                   indicator[categorical_data],
                                   value=lapply(find_most_frequent_values(
                                                    X[categorical_data]
                                                ),
                                                function(x)
                                                    if (length(x) > 1) {
                                                        sample(x, 1)
                                                    } else
                                                        x[length(x)]),
                                   SIMPLIFY=F
                               )

    X

}

