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
context("functions");

#
# Test of result select function.
#
test_that("True is True", {
    expect_true(TRUE);
    expect_equal(TRUE, TRUE);
    expect_identical(TRUE, TRUE);
})

#
# Test of result remap function.
#
test_that("False is False", {
    expect_false(FALSE);
    expect_equal(FALSE, FALSE);
    expect_identical(FALSE, FALSE);
})

#
# Create a basic test function.
#
test_fun <- function(a, b, c) {
    return(sprintf("A: %s, B: %s, C: %s", toString(a), toString(b), toString(c)));
}

#
# Test of basic test function.
#
test_that("Test Function is Consistent",{
    # Test that the test function works as expected.
    expect_equal(test_fun(a='a', b='b', c='c'), test_fun(a='a', b='b', c='c'));
})

#
# Test of default function add.
#
test_that("fun.arg.default Add Works (n=1)", {
    # Create the functions.
    test_fun_a <- fun.arg.default(test_fun, a='da');
    test_fun_b <- fun.arg.default(test_fun, b='db');
    test_fun_c <- fun.arg.default(test_fun, c='dc');

    # Test the functions.
    expect_equal(test_fun(a='a', b='b', c='c'), test_fun_a(a='a', b='b', c='c'));
    expect_equal(test_fun(a='da', b='b', c='c'), test_fun_a(b='b', c='c'));

    expect_equal(test_fun(a='a', b='b', c='c'), test_fun_b(a='a', b='b', c='c'));
    expect_equal(test_fun(a='a', b='db', c='c'), test_fun_b(a='a', c='c'));

    expect_equal(test_fun(a='a', b='b', c='c'), test_fun_c(a='a', b='b', c='c'));
    expect_equal(test_fun(a='a', b='b', c='dc'), test_fun_c(a='a', b='b'));
})

test_that("fun.arg.default Add Works (n=2)", {
    # Create the functions.
    test_fun_ab <- fun.arg.default(test_fun, a='da', b='db');
    test_fun_ac <- fun.arg.default(test_fun, a='da', c='dc');
    test_fun_bc <- fun.arg.default(test_fun, b='db', c='dc');

    # Test the functions.
    expect_equal(test_fun(a='a', b='b', c='c'), test_fun_ab(a='a', b='b', c='c'));
    expect_equal(test_fun(a='da', b='db', c='c'), test_fun_ab(c='c'));

    expect_equal(test_fun(a='a', b='b', c='c'), test_fun_ac(a='a', b='b', c='c'));
    expect_equal(test_fun(a='da', b='b', c='dc'), test_fun_ac(b='b'));

    expect_equal(test_fun(a='a', b='b', c='c'), test_fun_bc(a='a', b='b', c='c'));
    expect_equal(test_fun(a='a', b='db', c='dc'), test_fun_bc(a='a'));
})

test_that("fun.arg.default Add Works (n=3)", {
    # Create the functions.
    test_fun_abc <- fun.arg.default(test_fun, a='da', b='db', c='dc');

    # Test the functions.
    expect_equal(test_fun(a='a', b='b', c='c'), test_fun_abc(a='a', b='b', c='c'));
    expect_equal(test_fun(a='da', b='db', c='dc'), test_fun_abc());
})

#
# Test of default function remove.
#
test_that("fun.arg.default Remove Works (n=1)", {
    # Create the base function.
    test_fun_abc <- fun.arg.default(test_fun, a='da', b='db', c='dc');

    # Create the functions.
    test_fun_rem_a <- fun.arg.default(test_fun_abc, a=);
    test_fun_rem_b <- fun.arg.default(test_fun_abc, b=);
    test_fun_rem_c <- fun.arg.default(test_fun_abc, c=);

    # Test the functions.
    expect_error(test_fun_rem_a(), "argument \"a\" is missing, with no default");

    expect_error(test_fun_rem_b(), "argument \"b\" is missing, with no default");

    expect_error(test_fun_rem_c(), "argument \"c\" is missing, with no default");
})

test_that("fun.arg.default Remove Works (n=1)", {
    # Create the base function.
    test_fun_abc <- fun.arg.default(test_fun, a='da', b='db', c='dc');

    # Create the functions.
    test_fun_rem_ab <- fun.arg.default(test_fun_abc, a=, b=);
    test_fun_rem_ac <- fun.arg.default(test_fun_abc, a=, c=);
    test_fun_rem_bc <- fun.arg.default(test_fun_abc, b=, c=);

    # Test the functions.
    expect_error(test_fun_rem_ab(), "argument \"a\" is missing, with no default");
    expect_error(test_fun_rem_ab(b=1), "argument \"a\" is missing, with no default");
    expect_error(test_fun_rem_ab(a=1), "argument \"b\" is missing, with no default");

    expect_error(test_fun_rem_ac(), "argument \"a\" is missing, with no default");
    expect_error(test_fun_rem_ac(c=1), "argument \"a\" is missing, with no default");
    expect_error(test_fun_rem_ac(a=1), "argument \"c\" is missing, with no default");

    expect_error(test_fun_rem_bc(), "argument \"b\" is missing, with no default");
    expect_error(test_fun_rem_bc(c=1), "argument \"b\" is missing, with no default");
    expect_error(test_fun_rem_bc(b=1), "argument \"c\" is missing, with no default");
})
