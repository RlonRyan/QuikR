# ==================================================
# qdelta.R
# --------------------------------------------------
#
# R utility function for computing distance between
# generic objects a and b.
#
# --------------------------------------------------
# Author: RlonRyan
# Date  : 5/3/2018
# ==================================================

#'
#' @name qdelta.number
#' @rdname qdelta.number
#' @family qdelta functions
#'
#' @title
#' Compute the Delta Between Two Numbers
#'
#' @description
#' Computes the delta between two boolean values, \code{x} and \code{y}.
#'
#' @details
#' Returns 0 or 1 depending on if the two boolean values are different. In the case that something went wrong, will return \code{Inf}.
#'
#' @param x the first boolean value.
#' @param y the second boolean value.
#'
#' @return A single numeric delta value.
#'
#' @export
#'
qdelta.boolean <- function(x, y) {
    # Validate inputs
    assert_that(is.boolean(x), msg="x must be a boolean value!");
    assert_that(is.boolean(y), msg="y must be a boolean value!");

    # Default distance.
    dist <- Inf;

    # Compute distance.
    if (x == y) {
        dist <- 0;
    } else {
        dist <- 1;
    }

    # Return the distance.
    return(dist);
}

#'
#' @name qdelta.number
#' @rdname qdelta.number
#' @family qdelta functions
#'
#' @title
#' Compute the Delta Between Two Numbers
#'
#' @description
#' Computes the delta between two numbers, \code{x} and \code{y}.
#'
#' @details
#' Computes the absolute value of the difference between the two numbers.
#'
#' @param x the first number.
#' @param y the second number.
#'
#' @return A single numeric delta value.
#'
#' @export
#'
qdelta.number <- function(x, y) {
    # Validate inputs
    assert_that(is.number.na.null(x), msg="x must be a number!");
    assert_that(is.number.na.null(y), msg="y must be a number!");

    # Compute distance.
    dist <- abs(x - y);

    # Return the distance.
    return(dist);
}

#'
#' @name qdelta.numeric
#' @rdname qdelta.numeric
#' @family qdelta functions
#'
#' @title
#' Compute the Delta Between Two Numeric Values
#'
#' @description
#' Computes the delta between two numeric values, \code{x} and \code{y}.
#'
#' @details
#' Computes the euclidean distance between the numeric values, which are possibly numeric lists.
#'
#' @param x the first numeric value.
#' @param y the second numeric value.
#'
#' @return A single numeric delta value.
#'
#' @export
#'
qdelta.numeric <- function(x, y) {
    # Validate inputs
    assert_that(is.numeric.na.null(x), msg="x must be a numeric!");
    assert_that(is.numeric.na.null(y), msg="y must be a numeric!");

    # Compute distance.
    dist <- (x - y) %>%
        map(~.x^2) %>%
        reduce(sum) %>%
        sqrt(.);

    # Return the distance.
    return(dist);
}

#'
#' @name qdelta.string
#' @rdname qdelta.string
#' @family qdelta functions
#'
#' @title
#' Compute the Delta Between Two Strings
#'
#' @description
#' Computes the delta between two strings, \code{x} and \code{y}.
#'
#' @details
#' Uses \code{\link[utils]{adist}} to compute the string delta value.
#'
#' @param x the first string.
#' @param y the second string.
#'
#' @return A single numeric delta value.
#'
#' @export
#'
qdelta.string <- function(x, y) {
    # Coerce inputs into strings.
    x <- toString(x);
    y <- toString(y);

    # Compute distances.
    dist <- utils::adist(x, y)[1,1];

    # Return distance.
    return(dist);
}

#'
#' @name qdelta.list
#' @rdname qdelta.list
#' @family qdelta functions
#'
#' @title
#' Compute the Delta Between Two Lists.
#'
#' @description
#' Computes the delta between lists, \code{x} and \code{y}.
#'
#' @details
#' Computes the delta between each value pair in the two lists, then sums as to get the final delta value.
#'
#' @param x the first list.
#' @param y the second list.
#'
#' @return A single numeric delta value.
#'
#' @export
#'
qdelta.list <- function(x, y) {
    # Validate inputs
    assert_that(is.string(x), msg="x must be a list!");
    assert_that(is.string(y), msg="y must be a list!");

    # Compute lengths.
    lx <- length(x);
    ly <- length(y);
    ll <- max(x, y);

    # Set lengths.
    length(x) <- ll;
    length(y) <- ll;

    # Compute distances.
    dist <- map2(x, y, ~qdelta(.x, .y)) %>%
        map(~.x^2) %>%
        reduce(sum) %>%
        sqrt(.);

    # Return distance.
    return(dist);
}

#'
#' @name qdelta
#' @rdname qdelta
#' @family qdelta functions
#'
#' @title
#' Compute the Delta Between Two Values
#'
#' @description
#' Computes the delta between two alike-type values, \code{x} and \code{y}.
#'
#' @details
#' First determines the type of the two values, then calls the respective qdelta function to compute the actual delta value.
#'
#' @param x the first value.
#' @param y the second value.
#'
#' @return A single numeric delta value.
#'
#' @export
#'
qdelta <- function(x, y) {
    # Default distance.
    dist <- Inf;

    # Attempt to apply numeric distance.
    if(is.numeric.na.null(x) && is.numeric.na.null(y)) {
        dist <- qdelta.numeric(x, y);
    }
    # Attempt to apply nubmer distance.
    else if (is.number.na.null(x) && is.number.na.null(y)) {
        dist <- qdelta.number(x, y);
    }
    # Attempt to apply boolean distance.
    else if (is.boolean(x) && is.boolean(y)) {
        dist <- qdelta.boolean(x, y);
    }
    # Otherwise default to string distance.
    else {
        dist <- qdelta.string(x, y);
    }

    # Return the computed distance value.
    return(dist);
}
