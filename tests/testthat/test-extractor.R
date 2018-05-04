# ==================================================
# test-extractor.R
# --------------------------------------------------
#
# Tests for the extractor function
#
# --------------------------------------------------
# Author: RlonRyan
# Date  : 5/3/2018
# ==================================================

#
# The test group context, should be related to filename.
#
context("extractor");

#
# The acutal tests.
#
test_that("extractor works as expected", {

    # Create an extractor for mtcars.
    ex <- extractor(mtcars);

    # Attempt to extract a valid column.
    expect_equal(ex(,1), mtcars[,1]);

    # Attempt to extract a valid row.
    expect_equal(ex(1,), mtcars[1,]);

    # Attempt to extract an invalid value.
    expect_equal(ex(10000), NA);

})
