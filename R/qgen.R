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

#'
#' @name qgen.element
#' @rdname qgen.element
#'
#' @title
#' Creates a value to be used in a matric.
#'
#' @description
#' Creates a value for (row, col) such that value = row * ncol + col.
#'
#' @details
#' The default implementation returns the row * number of columns plus the column number.
#'
#' The diagonal implementation returns a matrix with the given value on the diagonal.
#'
#' @return a value representing the row * ncol + col.
#'
NULL

#' @name qgen.element
#' @rdname qgen.element
#' @export
qgen.element.default <- function(row, col, nrow, ncol) {
    # Return the row * number of columns plus the column number.
    return(((row - 1) * ncol) + col);
}

#' @name qgen.matrix.element
#' @rdname qgen.matrix.element
#' @param value the value to be placed on the diagonal.
#' @param orelse the value to be placed everwhere else.
#' @export
qgen.element.diagonal <- function(row, col, nrow, ncol, value=1, orelse=0) {
    # If row = col then 1
    if (row == col) {
        return(value);
    } else {
        return(orelse);
    }
}

#'
#' @name qgen
#' @rdname qgen
#' @aliases qgen.matrix
#' @aliases qgen.data.frame
#'
#' @title
#' Generates a Table
#'
#' @description
#' Creates a table with dimensions \code{nrow} x \code{ncol} using the given generator function.
#'
#' @details
#' Generates a table (either a matrix or a dataframe), using the given generator function, passing any \code{...} arguments to the generator function.
#'
#' @param nrow the number of rows that will be in the resulting matrix.
#' @param ncol the number of columns that will be int the resulting matrix.
#' @param func the function used to generate values.
#'
#' @return a table generated with the given generator function.
#'
NULL;

#' @name qgen
#' @rdname qgen
#' @export
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

#' @name qgen
#' @rdname qgen
#' @export
qgen.data.frame <- function(nrow, ncol, func=qgen.matrix.element.default, ...) {
    # Create a matrix.
    res <- qgen.matrix(nrow, ncol, func, ...);

    # Convert to a dataframe.
    res <- as.data.frame(res);

    # Return the generated matrix.
    return(res);
}
