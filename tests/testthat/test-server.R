test_server <- function(input, output, session) {}

test_that("functions that display modal dialogs execute without error", {
  testServer(test_server, {
    expect_no_error({
      .show_guide()
    })

    expect_no_error({
      .show_waiter()
      Sys.sleep(0.1)
      waiter::waiter_hide()
    })

    expect_no_error({
      .show_notif("This is a test notification message.")
    })

    expect_no_error({
      .show_alert("This is a test alert message.")
    })

    expect_no_error({
      .show_demo_notif()
    })

    expect_no_error({
      .show_backend_alert()
    })

  })
})

test_that("stop_if_no_backend works", {
  testServer(test_server, {
    with_mocked_bindings(
      {
        expect_error({
          .stop_if_no_backend()
        })
      },
      .require_cmdstanr_cmdstan = function(error) FALSE
    )
  })
})

test_that("stop_if_fit_in_demo works", {
  testServer(test_server, {
    with_mocked_bindings(
      {
        expect_error({
          .stop_if_fit_in_demo()
        })
      },
      .get_config = function(value) TRUE
    )
  })
})

test_that("stop_if_bad_mcmc_params works", {
  testServer(test_server, {
    expect_no_error({
      .stop_if_bad_mcmc_params(n_iter = 1000, n_chains = 4, seed = 123)
    })

    expect_error({
      .stop_if_bad_mcmc_params(n_iter = -1000, n_chains = 4, seed = 123)
    })

    expect_error({
      .stop_if_bad_mcmc_params(n_iter = 1000, n_chains = 0, seed = 123)
    })

    expect_error({
      .stop_if_bad_mcmc_params(n_iter = 1000, n_chains = 4, seed = -5)
    })
  })
})

test_that("stop_if_max_models works", {
  testServer(test_server, {
    expect_no_error({
      .stop_if_max_models(n_models = 2)
    })

    expect_error({
      .stop_if_max_models(n_models = 8)
    })
  })
})

test_that("stop_if_no_effects works", {
  testServer(test_server, {
    expect_no_error({
      .stop_if_no_effects(2, 3)
    })

    expect_error({
      .stop_if_no_effects(0, 0)
    })
  })
})

test_that("stop_if_bad_priors works", {
  testServer(test_server, {
    expect_no_error({
      .stop_if_bad_priors(list("NoRmaL(0,1)", "student_T(3,0,1)", "StrucTureD"))
    })

    expect_error({
      .stop_if_bad_priors(list("structure"))
    })

    expect_error({
      .stop_if_bad_priors(list("normal(0, -1)"))
    })
  })
})