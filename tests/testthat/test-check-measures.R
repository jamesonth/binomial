#aux_means
test_that("aux_mean works", {
  expect_true(aux_mean(10,0.3))
  expect_true(aux_mean(10,0.5))
})

test_that("aux_mean doesn't take invalid inputs", {
  expect_error(aux_mean("hello",0.1)) 
  expect_error(aux_mean(10,"hello"))
  
})

test_that("aux_mean doesn't take out of range inputs", {
  expect_error(aux_mean(-10,0.1)) 
  expect_error(aux_mean(10,-0.3))
  
})

#aux_variance
test_that("aux_variance works", {
  expect_true(aux_variance(10,0.3))
  expect_true(aux_variance(100,0.15))
})

test_that("aux_variance doesn't take invalid inputs", {
  expect_error(aux_variance("hello",0.1)) 
  expect_error(aux_variance(10,"hello"))
  
})

test_that("aux_variance doesn't take out of range inputs", {
  expect_error(aux_variance(-10,0.1)) 
  expect_error(aux_variance(10,-0.3))
  
})

#aux_mode 
test_that("aux_mode works", {
  expect_true(aux_mode(10,0.3))
  expect_true(aux_mode(100,0.15))
})

test_that("aux_mode doesn't take invalid inputs", {
  expect_error(aux_mode("hello",0.1)) 
  expect_error(aux_mode(10,"hello"))
  
})

test_that("aux_mode doesn't take out of range inputs", {
  expect_error(aux_mode(-10,0.1)) 
  expect_error(aux_mode(10,-0.3))
  
})

#aux_skewness
test_that("aux_skewness works", {
  expect_true(aux_skewness(10,0.3))
  expect_true(aux_skewness(100,0.15))
})

test_that("aux_skewness doesn't take invalid inputs", {
  expect_error(aux_skewness("hello",0.1)) 
  expect_error(aux_skewness(10,"hello"))
  
})

test_that("aux_skewness doesn't take out of range inputs", {
  expect_error(aux_skewness(-10,0.1)) 
  expect_error(aux_skewness(10,0))
  
})

#aux_kurtosis
test_that("aux_kurtosis works", {
  expect_true(aux_kurtosis(10,0.3))
  expect_true(aux_kurtosis(100,0.15))
})

test_that("aux_kurtosis doesn't take invalid inputs", {
  expect_error(aux_kurtosis("hello",0.1)) 
  expect_error(aux_kurtosis(10,"hello"))
  
})

test_that("aux_kurtosis doesn't take out of range inputs", {
  expect_error(aux_kurtosis(-10,0.1)) 
  expect_error(aux_kurtosis(10,-2))
  
})
