#check_prob
test_that("check_prob works", {
  expect_true(check_prob(0.1))
  expect_true(check_prob(0.5))
  expect_true(check_prob(0.9))
})

test_that("check_prob doesn't take invalid inputs", {
  expect_error(check_prob("hello"))

})

test_that("check_prob doesn't take out of range inputs", {
  expect_error(check_prob(0.0))
  expect_error(check_prob(-1.0))
  expect_error(check_prob(2.0))
  expect_error(check_prob(c(2.4,3.3)))
  
})

#check_trials
test_that("check_trials works", {
  expect_true(check_trials(0))
  expect_true(check_trials(10))
  expect_true(check_trials(100))
})

test_that("check_trials doesn't take invalid inputs", {
  expect_error(check_prob("hello"))
  
})

test_that("check_trials doesn't take out of range inputs", {
  expect_error(check_prob(-1))
  expect_error(check_prob(-2))
  expect_error(check_prob(c(-2.4,-3.3)))
  
})

#check_success
test_that("check_success works", {
  expect_true(check_success(c(1,3),10))
  expect_true(check_success(c(1,2,3,2,103,12),150))
})

test_that("check_success doesn't take invalid inputs", {
  expect_error(check_success("hello",2))
  expect_error(check_success(2,"hello"))
  
})

test_that("check_success doesn't take out of range inputs", {
  expect_error(check_success(c(-1,2),2))
  expect_error(check_success(c(1,2,4),1))
  
})


