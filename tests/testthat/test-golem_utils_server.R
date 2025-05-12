test_that("notin works", {
  expect_true(1 %notin% 2:10)
  expect_false(1 %notin% 1:10)
})

test_that("notnull works", {
  expect_true(notnull(1))
  expect_false(notnull(NULL))
})

test_that("notna works", {
  expect_true(notna(1))
  expect_false(notna(NA))
})

test_that("drop_nulls works", {
  expect_equal(
    drop_nulls(
      list(x = NULL, y = 2)
    ),
    list(y = 2)
  )
})

test_that("%||% works", {
  expect_equal(
    NULL %||% 1,
    1
  )
  expect_equal(
    2 %||% 1,
    2
  )
})

test_that("%|NA|% works", {
  expect_equal(
    NA %|NA|% 1,
    1
  )
  expect_equal(
    2 %|NA|% 1,
    2
  )
})

test_that("rv and rvtl work", {
  expect_true(
    inherits(rv, "function")
  )
  expect_true(
    inherits(rvtl, "function")
  )
})
