test_that("require_cmdstanr_cmdstan works", {
  with_mocked_bindings(
    {
      expect_false({
        .require_cmdstanr_cmdstan(error = FALSE)
      })

      expect_error({
        .require_cmdstanr_cmdstan(error = TRUE)
      }, "CmdStanR is not installed")
    },
    .require_ns = function(pkg, quietly) FALSE,
    .get_config = function(value) TRUE
  )

  with_mocked_bindings(
    {
      expect_false({
        .require_cmdstanr_cmdstan(error = FALSE)
      })

      expect_error({
        .require_cmdstanr_cmdstan(error = TRUE)
      }, "CmdStan is not installed")
    },
    .require_ns = function(pkg, quietly) TRUE,
    .cmdstan_version = function(error_on_NA) NULL,
    .get_config = function(value) TRUE
  )

  with_mocked_bindings(
    {
      expect_true({
        .require_cmdstanr_cmdstan(error = FALSE)
      })

      expect_no_error({
        .require_cmdstanr_cmdstan(error = TRUE)
      })
    },
    .require_ns = function(pkg, quietly) TRUE,
    .cmdstan_version = function(error_on_NA) "1.0.0",
    .get_config = function(value) TRUE
  )  

  with_mocked_bindings(
    {
      expect_true({
        .require_cmdstanr_cmdstan(error = FALSE)
      })

      expect_no_error({
        .require_cmdstanr_cmdstan(error = TRUE)
      })
    },
    .require_ns = function(pkg, quietly) FALSE,
    .get_config = function(value) FALSE
  )
})
