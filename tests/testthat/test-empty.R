# ==================================================
# test-empty.R
# --------------------------------------------------
#
# Empty test file, used as a template, and as to
# check that everything in the toolchain is working.
#
# --------------------------------------------------
# Author: RlonRyan
# Date  : 5/3/2018
# ==================================================

#
# The test group context, should be related to filename.
#
context("Empty");

#
# The acutal tests.
#
test_that("True is True", {
    expect_true(TRUE);
    expect_equal(TRUE, TRUE);
    expect_identical(TRUE, TRUE);
});

test_that("False is False", {
    expect_false(FALSE);
    expect_equal(FALSE, FALSE);
    expect_identical(FALSE, FALSE);
});
