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
#' Filters a function to return only values matching the given selection.
#'
#' @param func the function to wrap.
#' @param selection the results to be retained from the function.
#'
#' @name select
#'
#' @export
#' @rdname select
#'
select <- function(func, selection) {
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
#' Remaps a functions return result format using the given mapping.
#'
#' Like the select function, but does not drop unmatched results.
#'
#' @param func the function to have it's result format remapped.
#' @param mapping the mapping to remap the function with.
#'
#' @name remap
#'
#' @export
#' @rdname remap
#'
remap <- function(func, mapping, drop=FALSE) {
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
