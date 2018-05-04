# ==================================================
# qdiff.r
# --------------------------------------------------
#
# Quick difference computation functions.
#
# --------------------------------------------------
# Author: RlonRyan
# Date  : 5/3/2018
# ==================================================

#'
#' @title Compute a Difference
#'
#' @description
#' Computes the difference between two values.
#'
#' @param actual the actual value.
#' @param expected the expected value.
#' @param tolerance the error tolerance.
#'
#' @return a dataframe consisting of all relevant diff data.
#'
#' @name qdiff
#' @export
#'
qdiff.value <- function(actual, expected, tolerance) {
    # Compute delta value.
    delta <- qdist(actual, expected);

    # Create the result object.
    result <- data.frame(
        actual=actual,
        expected=expected,
        delta=delta,
        tolerance=tolerance,
        differ=isTRUE(abs(delta) > tolerance),
        stringsAsFactors=FALSE
    );

    # Return the result.
    return(result);
}

#'
#' @param row the row that the cell is a member of.
#' @param col the column that the cell is a member of within the given row.
#'
#' @name qdiff
#' @export
#'
qdiff.cell <- function(row, col, actual, expected, tolerance) {
    # Compute value diff.
    result <- qdiff.value(actual, expected, tolerance);

    # Add row & column data.
    result <- cbind(row, col, result);

    # Return the result.
    return(result);
}

#'
#' @param tolerances a list of tolerance values, on a per-column basis.
#' @param drop whether or not to keep information on cells that are determined to be the same.
#'
#' @name qdiff
#' @export
#'
qdiff <- function(actual, expected, tolerances=NULL, drop=TRUE) {
    # Compute Output Dimensions.
    rn <- union(rownames(expected), rownames(actual));
    cn <- union(colnames(expected), colnames(actual));

    # The result dataframe.
    result <- data.frame(stringsAsFactors=FALSE);

    # Wrap getters.
    sexp <- extractor(expected);
    sact <- extractor(actual);
    stol <- extractor(tolerances, 0);

    # Iterate over rows.
    for (row in rn) {
        # Iterate over columns.
        for (col in cn) {
            # Fetch in both.
            exp <- sexp(row,col);
            act <- sact(row,col);
            tol <- stol(col);
            # Diff.
            dif <- qdiff.cell(row, col, exp, act, tol);
            # Add if not dropping.
            if (!(isTRUE(drop) && !isTRUE(dif$differ))) {
                result <- rbind(result, dif);
            }
        }
    }

    # Return the result.
    return(result);
}

#'
#' @name qdiff
#' @export
#'
vqdiff <- function(actual, expected, tolerances=NULL, drop=TRUE) {
    # Compute names.
    nam.a <- deparse(substitute(actual));
    nam.e <- deparse(substitute(expected));

    # Combine names.
    nam <- sprintf("%s vs. %s", nam.a, nam.e);

    # Compute difference.
    diff <- qdiff(actual, expected, tolerances, drop);

    # Open Viewer
    invisible(View(diff, nam));
}
