# ==================================================
# functions.R
# --------------------------------------------------
#
# R function utility functions.
#
# --------------------------------------------------
# Author: RlonRyan
# Date  : 5/3/2018
# ==================================================

#'
#' @name fun.res.select
#' @rdname fun.res.select
#'
#' @title
#' Select Function Return Values
#'
#' @description
#' Filters a function to return only values matching the given selection.
#'
#' @details
#' For a given function, the function returns a wrapper that only retains return values specified in the selected list.
#'
#' @param func the function to wrap.
#' @param selection the results to be retained from the function.
#'
#' @return A wrapped function.
#'
#' @export
#'
fun.res.select <- function(func, selection) {
  # Extract the function.
  func <- match.fun(func);

  # Create the wrapped function.
  wrapped <- function(...) {
    # Call the function with the given arguments to get the result.
    result <- do.call(func, list(...));

    # Extract the selected result value.
    extracted <- result[selection];

    # Return the extracted value.
    return(extracted);
  }

  # Return the wrapped function.
  return(wrapped);
}

#'
#' @name fun.res.remap
#' @rdname fun.res.remap
#'
#' @title
#' Remaps a functions return result format using the given mapping.
#'
#' @description
#' Like the select function, but does not drop unmatched results.
#'
#' @details
#' For every pair in the mapping of for \code{(x='y')}, the function take the result of name x and renames it to y.
#'
#' @param func the function to have it's result format remapped.
#' @param mapping the mapping to remap the function with.
#' @param drop if return values that weren't in the mapping should be dropped.
#'
#' @return A remapped function.
#'
#' @export
#'
fun.res.remap <- function(func, mapping, drop=FALSE) {
  # Extract the function.
  func <- match.fun(func);

  # Compute the selection.
  selection<-names(mapping);

  # Create the wrapped function.
  wrapped <- function(...) {
    # Call the function with the given arguments to get the result.
    result <- do.call(func, list(...));

    # Extract the selected values.
    selected <- result[selection];

    # Extract the unselected values.
    unselected <- setdiff(result, selected);

    # Rename the selected values.
    names(selected) <- mapping;

    # Return the result, based on the mode.
    if (drop) {
      return(selected);
    } else {
      return(c(selected, unselected));
    }
  }

  # Return the wrapped function.
  return(wrapped);
}

#'
#' @name fun.arg.default
#' @rdname fun.arg.default
#'
#' @title
#' Change Function Defaults.
#'
#' @description
#' Allows for the changing of function default arguments.
#'
#' @details
#' For every pair of the form \code{name=value} in \code{...}, the function changes the default value of the argument with name \code{name} to the given value \code{value}.
#'
#' @param func the function to have it's defaults changed.
#' @param ... the new defaults mapping.
#'
#' @return A remapped function.
#'
#' @export
#'
fun.arg.default <- function(func, ...) {
    # Extract the function.
    func <- match.fun(func);

    # Get function arguments.
    arguments <- formals(func);

    # Create mapping list.
    defaults <- match.call(expand.dots=FALSE)[["..."]];

    # Iterate.
    for (name in names(defaults)) {
        # Get the actual element.
        newval <- defaults[name];

        # In the case that arg=arg, replace with nothing.
        if (name == newval) {
            newval <- NULL;
        }

        # Look for matching element.
        oldval <- arguments[name];

        # Print
        #printf("Swapping default for %s! From: %s To: %s", name, toString(oldval), toString(newval));

        # Assert that is present.
        assert_that(!is.null(oldval), msg="Cannot set default value for missing parameter!");

        # Update the element.
        arguments[name] <- newval;
    }

    # Now change formals.
    formals(func) <- arguments;

    # Return the re-mapped function.
    return(func);
}
