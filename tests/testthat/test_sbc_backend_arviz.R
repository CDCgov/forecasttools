test_that("sbc_backend_arviz throws an error if 'data' argument is provided", {
  expect_error(sbc_backend_arviz(data = "some_data"))
})

test_that("sbc_backend_arviz throws an error if 'path' argument is not
  provided", {
            expect_error(sbc_backend_arviz())
})

test_that("sbc_backend_arviz returns a list with the provided arguments", {
  result <- sbc_backend_arviz(path = "path/to/arviz_datafile",
                              other_arg = "value")
  expect_s3_class(result, "sbc_backend_arviz")
  expect_equal(result$args$path, "path/to/arviz_datafile")
  expect_equal(result$args$other_arg, "value")
})
