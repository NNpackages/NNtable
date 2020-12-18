context("Test context")

test_that("check numerical operator", {
  skip_on_devops()

  expect_equal( 2+2, 4)
})
