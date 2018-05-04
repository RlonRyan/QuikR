# ==================================================
# extractor.r
# --------------------------------------------------
#
# List extractor function creator function.
#
# --------------------------------------------------
# Author: RlonRyan
# Date  : 5/3/2018
# ==================================================

#'
#' Create an Extractor Function
#'
#' Creates an extractor function for the given data input.
#'
#' @param x the list to create the extractor for.
#' @param default the default value to return in the case that an applied index turns out to be invalid.
#'
#' @return an extractor function for the list x.
#'
#' @export
#'
extractor <- function(x, default=NA) {
    # Create basic extractor function.
    extractor <- function(...) {
        # Attempt to extract.
        result <- tryCatch(
            x[...],
            error = function(e) { return(NA); }
        );

        # Remove NULL and NAs.
        if(is.null(result) || is.na(result)) {
            result <- default;
        }

        # Return the result.
        return(result);
    }

    # Return the wrapped extractor.
    return(extractor);
}
