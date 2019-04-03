
# Reference 1
#   Title: Example of unit testing R code with testthat
#   Published on: June 12, 2013
#   Author: John
#   Accessibility: https://www.johndcook.com/blog/2013/06/12/example-of-unit-testing-r-code-with-testthat/

# to run this test: < test_file("C:/Users/User/Desktop/School/2610 - R Prog for Data Science/qsolve.R") >

library(testthat) #loading the library testtaht

test_that("Distinct roots", {
    roots <- qsolve(1, 7, 12)
    expect_that( roots, is_a("numeric") )
    expect_that( length(roots), equals(2) )
    expect_that( roots[1] < roots[2], is_true() )
})

test_that("Repeated root", {
    roots <- qsolve(1, 6000, 9000000)
    expect_that( length(roots), equals(2) )
    expect_that( roots, equals(c(-3000, -3000)) )
    expect_that( roots, equals(c(-3000.01, -3000.01), tolerance  = 0.1) )# Test whether ABSOLUTE error is within 0.1 
    
    expect_equal( roots, c(-3001, -3001), tolerance  = 0.1, scale=-3001) 
})

test_that("A, B, and C should be finite real numbers", {
    expect_that( qsolve("a", 2, 1), equals(NA) )    # Test for finite error 
    expect_that( qsolve(1, "2", 1), equals(NA) )    # Test for finite error  
    expect_that( qsolve(1, NA, 2), equals(NA) )     # Test for finite error 
    expect_that( qsolve(1, 10e308, 1), equals(NA) ) # Test for finite error
    
    ox <- qsolve("fred", 2, 1)
    expect_true(is.na(ox) && length(ox)==1)
})

test_that("B^2 must be smaller than Inf", {
    expect_that( qsolve(1, 1e308, 1), equals(NA) )  # Test for Inf error  
    expect_that( qsolve(5, 1e210, 5), equals(NA) )  # Test for Inf error  
})

test_that("A cannot be zero", {
    expect_that( qsolve(0, 2, 3), equals(NA) )# Test for ANY error  
    expect_that( qsolve(0, 4, 3), equals(NA) )# Test specifically for an error string containing "zero"
    expect_that( qsolve(0, 12, 4), equals(NA) )# Test specifically for an error string containing "zero" or "Zero" using regular expression
})
