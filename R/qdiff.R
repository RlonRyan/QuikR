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
#' @name qdiff
#' @rdname qdiff
#'
#' @aliases qdiff.value
#' @aliases qdiff.cell
#' @aliases qdiff.table
#'
#' @title Diffs Two Values
#'
#' @description
#' Computes the difference between two values, \code{actual} and \code{expected}.
#'
#' @details
#' Determines the difference using \code{\link[quikr]{qdelta}}, and compares against the given tolerance level.
#'
#' @param actual the actual value.
#' @param expected the expected value.
#' @param tolerance the error tolerance.
#'
#' @return A dataframe consisting of the diff data.
#'
#' @seealso \code{\link[quikr]{vqdiff}}
#'
NULL


#' @name qdiff
#' @rdname qdiff
#' @export
qdiff.value <- function(actual, expected, tolerance) {
    # Compute delta value.
    delta <- qdelta(actual, expected);

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

#' @name qdiff
#' @rdname qdiff
#' @param row the row that the cell is a member of.
#' @param col the column that the cell is a member of within the given row.
#' @export
qdiff.cell <- function(row, col, actual, expected, tolerance) {
    # Compute value diff.
    result <- qdiff.value(actual[row, col], expected[row, col], tolerance);

    # Add row & column data.
    result <- cbind(row, col, result);

    # Return the result.
    return(result);
}

#' @name qdiff
#' @rdname qdiff
#' @param tolerances a list of tolerance values, on a per-column basis.
#' @param drop whether or not to keep information on cells that are determined to be the same.
#' @export
qdiff.table <- function(actual, expected, tolerances=NULL, drop=TRUE) {
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
    #sexp <- extractor(expected);
    #sact <- extractor(actual);
    stol <- extractor(tolerances, 0);

    # Iterate over rows.
    for (row in rn) {
        # Iterate over columns.
        for (col in cn) {
            # Fetch in both.
            #exp <- sexp(row,col);
            #act <- sact(row,col);
            tol <- stol(col);
            # Diff.
            dif <- qdiff.cell(row=row, col=col, actual=actual, expected=expected, tolerance=tol);
            # Add if not dropping.
            if (!(isTRUE(drop) && !isTRUE(dif$differ))) {
                result <- rbind(result, dif);
            }
        }
    }

    # Return the result.
    return(result);
}

#' @name vqdiff
#' @rdname vqdiff
#'
#' @inherit qdiff
#'
#' @aliases vqdiff.value
#' @aliases vqdiff.cell
#' @aliases vqdiff.table
#'
#' @title Views the Diff of Two Values
#'
#' @description
#' Computes the difference between two values, \code{actual} and \code{expected}, and then views it.
#'
#' @seealso \code{\link[quikr]{qdiff}}
NULL

#' @name vqdiff
#' @rdname vqdiff
#' @export
vqdiff.value <- function(actual, expected, tolerance) {
    # Compute names.
    nam.a <- deparse(substitute(actual));
    nam.e <- deparse(substitute(expected));

    # Combine names.
    nam <- sprintf("%s vs. %s", nam.a, nam.e);

    # Compute difference.
    diff <- qdiff.value(actual=actual, expected=expected, tolerance=tolerance);

    # Open Viewer
    invisible(View(diff, nam));
}

#' @name vqdiff
#' @rdname vqdiff
#' @export
vqdiff.cell <- function(row, col, actual, expected, tolerance) {
    # Compute names.
    nam.a <- deparse(substitute(actual));
    nam.e <- deparse(substitute(expected));

    # Combine names.
    nam <- sprintf("%1$s(%3$d, %4$d) vs. %2$s(%4$d, %4$d)", nam.a, nam.e, row, col);

    # Compute difference.
    diff <- qdiff.cell(row=row, col=col, actual=actual, expected=expected, tolerance=tolerance);

    # Open Viewer
    invisible(View(diff, nam));
}

#' @name vqdiff
#' @rdname vqdiff
#' @export
vqdiff.table <- function(actual, expected, tolerances=NULL, drop=TRUE) {
    # Compute names.
    nam.a <- deparse(substitute(actual));
    nam.e <- deparse(substitute(expected));

    # Combine names.
    nam <- sprintf("%s vs. %s", nam.a, nam.e);

    # Compute difference.
    diff <- qdiff.table(actual=actual, expected=expected, tolerances=tolerances, drop=drop);

    # Open Viewer
    invisible(View(diff, nam));
}
