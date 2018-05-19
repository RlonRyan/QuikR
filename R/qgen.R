# ==================================================
# qgen.r
# --------------------------------------------------
#
# Quick data generation functions.
#
# --------------------------------------------------
# Author: RlonRyan
# Date  : 5/18/2018
# ==================================================

#' @title Creates a value to be used in a matric.
#'
#' @description
#' Creates a value for (row, col) such that value = row * ncol + col.
#'
#' @return a value representing the row * ncol + col.
#'
#' @name qgen.matrix.element.default
#' @export
#'
qgen.matrix.element.default <- function(row, col, nrow, ncol) {
    # Return the row * number of columns plus the column number.
    return(((row - 1) * ncol) + col);
}

#' @title Creates a value to be used in a matric.
#'
#' @description
#' Creates a value for (row, col) such that value = row * ncol + col.
#'
#' @return a value representing the row * ncol + col.
#'
#' @name qgen.matrix.element.default
#' @export
#'
qgen.matrix.element.diagonal <- function(row, col, nrow, ncol, value=1, orelse=0) {
    # If row = col then 1
    if (row == col) {
        return(value);
    } else {
        return(orelse);
    }
}

#'
#' @title Create a nrow by ncol matrix.
#'
#' @description
#' Creates a matrix using the given generator function.
#'
#' @param nrow the number of rows that will be in the resulting matrix.
#' @param ncol the number of columns that will be int the resulting matrix.
#' @param func the function used to generate values.
#'
#' @return a matrix generated with the given generator function.
#'
#' @name qgen
#' @export
#'
qgen.matrix <- function(nrow, ncol, func=qgen.matrix.element.default, ...) {
    # Allocate the matrix.
    res <- matrix(nrow=nrow, ncol=ncol);

    # Loop, although probably not best option.
    for (r in 1:nrow) {
        for (c in 1:ncol) {
            res[r, c] <- func(row=r, col=c, nrow=nrow, ncol=ncol, ...);
        }
    }

    # Return the generated matrix.
    return(res);
}

#'
#' @title Create a nrow by ncol matrix.
#'
#' @description
#' Creates a matrix using the given generator function.
#'
#' @name qgen
#' @export
#'
qgen.data.frame <- function(nrow, ncol, func=qgen.matrix.element.default, ...) {
    # Create a matrix.
    res <- qgen.matrix(nrow, ncol, func, ...);

    # Convert to a dataframe.
    res <- as.data.frame(res);

    # Return the generated matrix.
    return(res);
}
