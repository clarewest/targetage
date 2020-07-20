test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

test_that("returns string", {
  expect_output(say_hello(), "Hello, stranger!")
})
