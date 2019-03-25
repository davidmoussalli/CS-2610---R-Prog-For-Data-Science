#install.packages("testthat")
require(testthat) #loading the library testtaht
source("~/desktop/qsolve.R") #load the .r file to be tested

#test if there is two distinct roots
test_that("Distinct roots",{
  roots <- qsolve(1,7,2)
  
  expect_that( roots, is_a("numeric") )
  expect_that( length(roots), equals(2) )
  expect_that( roots[1] <= roots[2], is_true() )
  
})

test_that("only one root", {
  
  roots <- qsolve(1, 6000, 9000000)
  
  expect_that( length(roots), equals(1) )
  
  expect_that( roots, equals(-3000) )
  
  # Test whether ABSOLUTE error is within 0.1 
  expect_that( roots, equals(-3000.01, tolerance  = 0.1) )
  
  # Test whether RELATIVE error is within 0.1
  # To test relative error, set 'scale' equal to expected value.
  # See base R function all.equal for optional argument documentation.
  expect_equal( roots, -3001, tolerance  = 0.1, scale=-3001) 
})

#test if the disc (b^2-4ac) is less than 0 which will return null from qsolve.R

test_that("disc is less than 0", {
  roots <- qsolve(1,4,1)
  expect_that(roots, is_null())
})


#test
test_that("a is zero",{
  expect_that( qsolve(1, 2, 3), throws_error("zero") )
  
})


test_that("Polynomial must be quadratic", {
  
  # Test for ANY error                     
  expect_that( qsolve(1, 2, 3), throws_error() )
  
  # Test specifically for an error string containing "zero"
  expect_that( qsolve(0, 2, 3), throws_error("zero") )
  
  # Test specifically for an error string containing "zero" or "Zero" using regular expression
  expect_that( qsolve(0, 2, 3), throws_error("[zZ]ero") )
})

