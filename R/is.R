# ==================================================
# is.R
# --------------------------------------------------
#
# Misc. R is.blank functions
#
# --------------------------------------------------
# Author: RlonRyan
# Date  : 5/3/2018
# ==================================================

#'
#' Boolean Value Test
#'
#' The following methods were added as to increase consistency with other is.something methods.
#'
#' @param x the value to test.
#'
#' @return \code{is.boolean(x)} \code{TRUE} if the value is a boolean, \code{FALSE} otherwise.
#'
#' @name is.boolean
#' @export
#'
is.boolean <- function(x) {
    return(is.logical(x));
}

#'
#' Test if a Value is TRUE
#'
#' @param x the value to test.
#'
#' @return \code{is.true(x)} \code{TRUE} if the value is true, \code{FALSE} otherwise.
#'
#' @name is.boolean
#' @export
#'
is.true <- function(x) {
    return(isTRUE(x));
}


#'
#' Test if a Value is FALSE
#'
#' @param x the value to test.
#'
#' @return \code{is.false(x)} \code{TRUE} if the value is false, \code{FALSE} otherwise.
#'
#' @name is.boolean
#' @export
#'
is.false <- function(x) {
    return(!isTRUE(x));
}

#'
#' Determines if a value is a number, na, or null.
#'
#' Uses is.number(), is.na(), and is.null() internally.
#'
#' @param x the value to test.
#'
#' @name is.number.na.null
#' @export
#'
is.number.na.null <- function(x) {
    return(
        is.null(x)
        || is.na(x)
        || is.number(x)
    );
}

#'
#' Determines if a value is numeric, na, or null.
#'
#' Uses is.numeric(), is.na(), and is.null() internally.
#'
#' Have to give export name due to oddity with roxygen export.
#'
#' @param x the value to test.
#'
#' @name is.numeric.na.null
#' @export is.numeric.na.null
#'
is.numeric.na.null <- function(x) {
    return(
        is.null(x)
        || is.na(x)
        || is.numeric(x)
    );
}
