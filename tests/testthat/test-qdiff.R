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
    a.diff <- qdiff.table(mtcars, mtcars, drop = TRUE);
    b.diff <- qdiff.table(mtcars, mtcars, drop = FALSE);

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
    a.diff <- qdiff.table(mtcars, mtcars2, drop = TRUE);
    b.diff <- qdiff.table(mtcars, mtcars2, drop = FALSE);

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

test_that("qdiff works with matricies", {

    # Create a matrix.
    expected <- qgen.matrix(5, 5);

    # Create acutal.
    actual <- expected;

    # Perform initial diff.
    diff <- qdiff.table(actual, expected);

    # Test that no cells differ to start.
    expect_equal(nrow(diff), 0);

    # Iterate, changing actual each time.
    i <- 0;
    for (r in 1:nrow(expected)) {
        for (c in 1:ncol(expected)) {
            # Increment i.
            i <- i + 1;

            # Change a value.
            actual[r, c] <- actual[r, c] * 2;

            # Compute difference.
            diff <- qdiff.table(actual, expected);

            # Test that number of differing cells is the same.
            expect_equal(nrow(diff), i)
        }
    }

})
