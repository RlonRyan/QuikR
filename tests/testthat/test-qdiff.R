# ==================================================
# test-qdiff.R
# --------------------------------------------------
#
# Tests for the qdiff function.
#
# --------------------------------------------------
# Author: RlonRyan
# Date  : 5/3/2018
# ==================================================

#
# The test group context, should be related to filename.
#
context("qdiff");

#
# The acutal tests.
#
test_that("qdiff works with mtcars", {

    # Compute diff, both dropping, and not dropping.
    a.diff <- qdiff(mtcars, mtcars, drop = TRUE);
    b.diff <- qdiff(mtcars, mtcars, drop = FALSE);

    # Compute expected lengths.
    a.e.len <- 0;
    b.e.len <- nrow(mtcars) * ncol(mtcars);

    # Compute actual lengths.
    a.a.len <- nrow(a.diff);
    b.a.len <- nrow(b.diff);

    # Expect that the lengths match expectations.
    expect_equal(a.a.len, a.e.len);
    expect_equal(b.a.len, b.e.len);

    # Create a mutated copy of mtcars.
    mtcars2 <- mtcars;
    mtcars2$mpg <- 2*mtcars2$mpg;

    # Compute the diffs, both dropping and not dropping.
    a.diff <- qdiff(mtcars, mtcars2, drop = TRUE);
    b.diff <- qdiff(mtcars, mtcars2, drop = FALSE);

    # Compute expected lengths.
    a.e.len <- nrow(mtcars);
    b.e.len <- nrow(mtcars) * ncol(mtcars);

    # Compute actual lengths.
    a.a.len <- nrow(a.diff);
    b.a.len <- nrow(b.diff);

    # Expect that the lengths match expectations.
    expect_equal(a.a.len, a.e.len);
    expect_equal(b.a.len, b.e.len);

})
