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
#' @title Compute a Value Difference
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
#' @title Compute a Cell Difference
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
#' @title Compute a Table Difference
#'
#' @param tolerances a list of tolerance values, on a per-column basis.
#' @param drop whether or not to keep information on cells that are determined to be the same.
#'
#' @name qdiff
#' @export
#'
qdiff <- function(actual, expected, tolerances=NULL, drop=TRUE) {
    # Set row and column names.
    colnames(actual) <- colnames(actual, do.NULL=FALSE);
    colnames(expected) <- colnames(expected, do.NULL=FALSE);

    rownames(actual) <- rownames(actual, do.NULL=FALSE);
    rownames(expected) <- rownames(expected, do.NULL=FALSE);

    # Compute Output Dimensions.
    cn <- union(colnames(expected), colnames(actual));
    rn <- union(rownames(expected), rownames(actual));
    #printf("Col Names: %s: %d %d %d", toString(cn), length(cn), ncol(actual), ncol(expected));
    #printf("Row Names: %s: %d %d %d", toString(rn), length(rn), nrow(actual), nrow(expected));

    # Test values.
    assert_that(length(cn) >= ncol(actual), msg="Number of Union columns not >= number of Actual columns.");
    assert_that(length(cn) >= ncol(expected), msg="Number of Union Columns not >= number of Expected columns.");
    assert_that(length(rn) >= nrow(actual), msg="Number of Union Rows not >= number of Actual rows.");
    assert_that(length(rn) >= nrow(expected), msg="Number of Union Rows not >= number of Expected rows.");

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
            dif <- qdiff.cell(row=row, col=col, actual=act, expected=exp, tolerance=tol);
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
#' @title Compute and View a Table Difference
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
    diff <- qdiff(actual=actual, expected=expected, tolerances=tolerances, drop=drop);

    # Open Viewer
    invisible(View(diff, nam));
}
