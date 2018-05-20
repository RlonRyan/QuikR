# ==================================================
# util.R
# --------------------------------------------------
#
# Misc. R utility functions.
#
# --------------------------------------------------
# Author: RlonRyan
# Date  : 5/3/2018
# ==================================================





#' @export
view <- utils::View;





#'
#' @name printf
#' @rdname printf
#'
#' @title
#' Formatted Print Function
#'
#' @description
#' The missing printf function.
#'
#' @details
#' Uses \code{\link[base]{sprintf}} internally.
#'
#' @param format the format string to use.
#' @param ... the arguments for the given format string.
#'
#' @seealso \code{\link[base]{sprintf}}
#'
#' @export
#'
printf <- function(format, ...) {
  invisible(print(sprintf(format, ...)));
}





#'
#' @name yesno
#' @rdname yesno
#'
#' @title
#' Convert a Boolean to a String
#'
#' @description
#' Returns 'yes' or 'no' based off of the provided boolean input.
#'
#' @details
#' If \code{yes} is specified, the value of \code{yes} is used instead of the default value \code{'yes'}.
#'
#' If \code{no} is specified, the value of \code{no} is used instead of the default value \code{'no'}.
#'
#' @param bool the boolean value to test.
#' @param yes the value to return on a true  input.
#' @param no  the value to return on a false input.
#'
#' @return 'yes' or 'no'.
#'
#' @export
#'
yesno <- function(bool, yes='yes', no='no') {

  # If TRUE then yes.
  if (bool == TRUE) {
    return(yes);
  }

  # Otherwise no.
  else {
    return(no);
  }

}





#'
#' @name named
#' @rdname named
#'
#' @title
#' Utility Value Naming Function
#'
#' @description
#' Utility function to add names to a given value.
#'
#' @details
#' Implemented as:
#'
#' \code{\link[base]{names}(value) <- \link[base]{list}(...)}
#'
#' @param value the value(s) to be named.
#' @param ... the name to(s) give the value(s).
#'
#' @return the named value(s).
#'
#' @seealso \code{\link[base]{unname}}
#'
#' @export
#'
named <- function(value, ...) {
  # Rename the value.
  names(value) <- list(...);

  # Return the renamed value.
  return(value);
}
