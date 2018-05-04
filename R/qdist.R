# ==================================================
# qdist.R
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
#' @name qdist
#' @export
#'
qdist.number <- function(x, y) {
    # Validate inputs
    assert_that(is.number.na.null(x), msg="x must be a number!");
    assert_that(is.number.na.null(y), msg="y must be a number!");

    # Compute distance.
    dist <- abs(x - y);

    # Return the distance.
    return(dist);
}

#'
#' @name qdist
#' @export
#'
qdist.numeric <- function(x, y) {
    # Validate inputs
    assert_that(is.numeric.na.null(x), msg="x must be a numeric!");
    assert_that(is.numeric.na.null(y), msg="y must be a numeric!");

    # Compute distance.
    dist <- (x - y) %>%
        map(~.x^2) %>%
        reduce('+') %>%
        sqrt();

    # Return the distance.
    return(dist);
}

#'
#' @name qdist
#' @export
#'
qdist.string <- function(x, y) {
    # Coerce inputs into strings.
    x <- toString(x);
    y <- toString(y);

    # Compute distances.
    dist <- adist(x, y)[1,1];

    # Return distance.
    return(dist);
}

#'
#' @name qdist
#' @export
#'
qdist.list <- function(x, y) {
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
    dist <- map2(x, y, ~qdist(.x, .y)) %>%
        map(~.x^2) %>%
        reduce('+') %>%
        sqrt();

    # Return distance.
    return(dist);
}

#'
#' @name qdist
#' @export
#'
qdist <- function(x, y) {
    # Default distance.
    dist <- Inf;

    # Attempt to apply numeric distance.
    if(is.numeric.na.null(x) && is.numeric.na.null(y)) {
        dist <- qdist.numeric(x, y);
    }
    # Otherwise default to string distance.
    else {
        dist <- qdist.string(x, y);
    }

    # Return the computed distance value.
    return(dist);
}
