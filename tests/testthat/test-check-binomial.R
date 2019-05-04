#bin_choose
test_that("bin_choose works", {
  expect_true(bin_choose(5,2))
  expect_true(bin_choose(5,1:3))
})

test_that("bin_choose doesn't take out of range inputs", {
  expect_error(bin_choose(5,10)) 
  expect_error(bin_choose(5,-10)) 
  expect_error(bin_choose(-5,10)) 
  
})

test_that("bin_choose doesn't take invalid inputs", {
  expect_error(bin_choose(10,"hello"))
  expect_error(bin_choose("hello",10))
  
})

#bin_probability
test_that("bin_probability works", {
  expect_true(bin_probability(2,5,0.5))
  expect_true(bin_probability(0:2,5,0.5))
})

test_that("bin_probability doesn't take out of range inputs", {
  expect_error(bin_probability(1,-1,0.2)) 
  expect_error(bin_probability(1,10,0))
  expect_error(bin_probability(-1:3,1,0.2)) 
  
})

test_that("bin_probability doesn't take invalid inputs", {
  expect_error(bin_probability(1,"hello",0.2)) 
  expect_error(bin_probability(1,1,"hello")) 
  
})

#bin distribution
test_that("bin_distribution works", {
  expect_true(bin_distribution(5,0.5))
  expect_true(bin_distribution(10,0.3))
})

test_that("bin_distribution doesn't take invalid inputs", {
  expect_error(bin_distribution("hello",0.2)) 
  expect_error(bin_distribution(10,"hello")) 
  
  
})

test_that("bin_distribution doesn't take out of range inputs", {
  expect_error(bin_distribution(10,0))
  expect_error(bin_distribution(10,2))
  expect_error(bin_distribution(-10,0.4))
  
})

#bin_cumulitive 
test_that("bin_cumulitive works", {
  expect_true(bin_cumulitive(5,0.5))
  expect_true(bin_cumulitive(10,0.3))
})

test_that("bin_cumulitive doesn't take invalid inputs", {
  expect_error(bin_cumulitive("hello",0.2)) 
  expect_error(bin_cumulitive(10,"hello")) 
  
})

test_that("bin_cumulitive doesn't take out of range inputs", {
  expect_error(bin_cumulitive(10,0))
  expect_error(bin_cumulitive(10,2))
  expect_error(bin_cumulitive(-10,0.4))
  
})