# qsolve.R 
# Version: 1.0
# Author: Ty Cobb 2019
# License: GPL 2.0
#
# The function qsolve(a, b, c)
# solves the quadratic equation
#     a x^2 + b x + c = 0
# for real solutions x1 and x2
# input:
#     a, b and c are finite numeric real numbers
#     a != 0
#     there are real solutions (not two complex ones)
# return:
#     c(x1, x2) where x1 and x2 are finite 
#               numeric real numbers
#               x1 <= x2
#     NA       if the input is not as expected or 
#              the return values can not be computed 
#              as expected. 
# Note the next version of qsolve() will
# 1) address several cases that now return NA because
#    of internal overflows and 
# 2) will provide more precision in some cases.
#
# qsolve <- function(a, b, c) {
#   disc <- b^2 - 4*a*c
#   if( disc < 0) {
#     return(NULL)
#   }
#   x1 <- (-b - sqrt(disc))/(2*a)
#   x2 <- (-b + sqrt(disc))/(2*a)
#   return(c(x1, x2))
# }

qsolve <- function(a, b, c) {
	#Test input values:
    if(is.finite(a)!=TRUE || is.finite(b)!=TRUE || is.finite(c)!=TRUE){
#        print("a, b, and c must be finite real numbers.")
        return(NA)
    }
    if(a==0){
#        print("a cannot equal zero")
        return(NA)
    }
    if((b^2) == Inf){
#        print("b^2 must be smaller than Inf")
        return(NA)
    }

    #Perform quadratic function
    disc <- b^2 - 4*a*c
    if( disc < 0) {
        return(NA)
    }
    x1 <- (-b - sqrt(disc))/(2*a)
    x2 <- (-b + sqrt(disc))/(2*a)

    if(x1>x2){# Not sure if this test is necessary...
#    	print("error...x1 should not be greater than x2...")
    	return(NA)
    }
    return(c(x1, x2))# Return x1 and x2 values
}# End qsolve()



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

    # Test whether RELATIVE error is within 0.1
    # To test relative error, set 'scale' equal to expected value.
    # See base R function all.equal for optional argument documentation.
    expect_equal( roots, c(-3001, -3001), tolerance  = 0.1, scale=-3001) 
})

test_that("A, B, and C should be finite real numbers", {
    expect_that( qsolve("a", 2, 1), equals(NA) )    # Test for finite error  
    expect_that( qsolve(1, "2", 1), equals(NA) )    # Test for finite error  
    expect_that( qsolve(1, NA, 2), equals(NA) )     # Test for finite error 
    expect_that( qsolve(1, 10e308, 1), equals(NA) ) # Test for finite error

#     expect_that( qsolve("a", 2, 1), throws_error("finite") )# Test for finite error  
#     expect_that( qsolve(1, "2", 1), throws_error("finite") )# Test for finite error  
#     expect_that( qsolve(1, NA, 2), throws_error("finite") )# Test for finite error 
#     expect_that( qsolve(1, 10e308, 1), throws_error("finite") )# Test for finite error 
})

test_that("B^2 must be smaller than Inf", {
    expect_that( qsolve(1, 1e308, 1), equals(NA) )  # Test for Inf error  
    expect_that( qsolve(5, 1e210, 5), equals(NA) )  # Test for Inf error  

    # expect_that( qsolve(1, 1e308, 1), throws_error("Inf") )# Test for Inf error  
    # expect_that( qsolve(1, 1e300, 1), throws_error("Inf") )# Test for Inf error  
})

test_that("Polynomial must be quadratic", {
    expect_that( qsolve(0, 2, 3), equals(NA) )# Test for ANY error  
    expect_that( qsolve(0, 2, 3), equals(NA) )# Test specifically for an error string containing "zero"
    expect_that( qsolve(0, 2, 3), equals(NA) )# Test specifically for an error string containing "zero" or "Zero" using regular expression
})

# Reference 1
#   Title: Example of unit testing R code with testthat
#   Published on: June 12, 2013
#   Author: John
#   Accessibility: https://www.johndcook.com/blog/2013/06/12/example-of-unit-testing-r-code-with-testthat/
