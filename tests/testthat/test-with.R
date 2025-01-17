test_that("with_defaults works as expected with unnamed args", {
  # assignment_linter is in defaults, so output doesn't change
  expect_named(with_defaults(assignment_linter), names(with_defaults()))
})

test_that("with_defaults warns on unused NULLs", {
  expect_warning(with_defaults(not_a_default = NULL), rex::rex("which is not in `default`."))
  expect_warning(with_defaults(not_a_default = NULL, also_not_default = NULL), rex::rex("which are not in `default`."))
})

test_that("all default linters are tagged default", {
  expect_named(with_defaults(), available_linters(tags = "default")$linter)

  # TODO(michaelchirico): use plain expect_equal after waldo#133 makes it into a CRAN release
  # Here, the environment()s are different because factories use them.
  skip_if_not_r_version("4.1.0") # Desired all.equal behaviour only available in >= 4.1
  # covr modifies package functions causing differing deparse() results even for identical anonymous functions.
  # This happens because default_linters is generated at build time and thus not modifiable by covr, whereas
  # linters_with_tags() constructs the linters at runtime.
  skip_if(covr::in_covr())

  expect_true(all.equal(linters_with_tags("default"), with_defaults()))
  expect_length(linters_with_tags("default", exclude_tags = "default"), 0L)

  # Check that above test also trips on default arguments.
  expect_equal(
    all.equal(linters_with_tags("default"), with_defaults(line_length_linter(120))),
    'Component "line_length_linter": Component "length": Mean relative difference: 0.5'
  )
})

test_that("can instantiate all linters without arguments", {
  all_linters <- linters_with_tags(tags = NULL)

  expect_type(all_linters, "list")
  expect_length(all_linters, nrow(available_linters()))

  really_all_linters <- suppressWarnings(linters_with_tags(tags = NULL, exclude_tags = NULL))
  expect_type(really_all_linters, "list")
  expect_length(really_all_linters, nrow(available_linters(exclude_tags = NULL)))
})
