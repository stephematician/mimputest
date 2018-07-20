# Returns type-respecting most frequent values
find_most_frequent_values <- function(X)
    lapply(X,
           function(x_, table_x_=table(x_))
               if (length(table_x_[table_x_ > 0]) > 0)
                   x_[sapply(which(table_x_ == max(table_x_)),
                             function(j) match(names(table_x_)[j], x_))]
               else
                   x_[0])

