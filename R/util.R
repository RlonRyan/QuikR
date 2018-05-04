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

#
# Alias of the view function.
#
view <- View

#'
#' The missing printf function.
#'
#' @param format the format string to use.
#' @param ... the arguments for the given format string.
#'
#' @name printf
#'
#' @export
#' @rdname printf
#'
printf <- function(format, ...) {
  invisible(print(sprintf(format, ...)));
}

#'
#' Returns 'yes' or 'no' based off of the provided boolean input.
#'
#' @param yes the value to return on a true  input.
#' @param no  the value to return on a false input.
#'
#' @name yesno
#' @aliases yn
#'
#' @export
#' @rdname yesno
#'
yn <- yesno <- function(bool, yes='yes', no='no') {

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
#' Utility function to create a named value.
#'
#' @param value the value(s) to be named.
#' @param ... the name to(s) give the value(s).
#'
#' @return the named value(s).
#'
#' @name named
#'
#' @export
#' @rdname named
#'
named <- function(value, ...) {
  # Rename the value.
  names(value) <- list(...);

  # Return the renamed value.
  return(value);
}

#'
#' @export
#' @rdname named
#'
unnamed <- function(value) {
  # Remove the names from the value.
  names(value) <- NULL;

  # Return the unnamed value.
  return(value);
}
