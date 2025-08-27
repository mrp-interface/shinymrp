test_that(".waiter_ui works correctly", {
  # Valid types return tagList with 2 element
  for (type in .const()$ui$loading_types) {
    golem::expect_shinytaglist(.waiter_ui(type))
  }

  expect_error(
    .waiter_ui("invalid"),
    "Assertion on 'loading_type' failed"
  )
})

test_that(".create_guide works correctly", {
  for (section in .const()$ui$guide_sections) {
    golem::expect_shinytag(.create_guide(section))
  }

  expect_error(
    .create_guide("invalid"),
    "Assertion on 'open' failed"
  )
})

test_that(".create_model_tab works correctly", {
  golem::expect_shinytag(
    .create_model_tab(
      ns = function(id) id,
      model = example_model(),
      last_tab_id = NULL
    )
  )
})

test_that(".est_map_ui works correctly", {
  # County map with slider
  golem::expect_shinytaglist(
    .est_map_ui(
      ns = function(id) id,
      model = example_model(is_timevar = TRUE),
      geo_scale = "county",
      geo_view = "map"
    )
  )

  # County map without slider
  golem::expect_shinytaglist(
    .est_map_ui(
      ns = function(id) id,
      model = example_model(is_timevar = FALSE),
      geo_scale = "county",
      geo_view = "map"
    )
  )

  # State map with slider
  golem::expect_shinytaglist(
    .est_map_ui(
      ns = function(id) id,
      model = example_model(is_timevar = TRUE),
      geo_scale = "state",
      geo_view = "map"
    )
  )

  # State map without slider
  golem::expect_shinytaglist(
    .est_map_ui(
      ns = function(id) id,
      model = example_model(is_timevar = FALSE),
      geo_scale = "state",
      geo_view = "map"
    )
  )

  # Error for invalid geo_scale
  expect_error(
    .est_map_ui(
      ns = function(id) id,
      model = example_model(is_timevar = FALSE),
      geo_scale = "invalid",
      geo_view = "map"
    ),
    "Assertion on 'geo_scale' failed"
  )

  # Error for invalid geo_view
  expect_error(
    .est_map_ui(
      ns = function(id) id,
      model = example_model(is_timevar = FALSE),
      geo_scale = "state",
      geo_view = "invalid"
    ),
    "Assertion on 'geo_view' failed"
  )
})

test_that(".plot_height works correctly", {
  expect_equal(.plot_height(n = 3, is_timevar = TRUE), 900)
  expect_equal(.plot_height(n = 3, is_timevar = FALSE), 550)
  expect_equal(.plot_height(n = 1, is_timevar = TRUE), 550)
  expect_equal(.plot_height(n = 1, is_timevar = FALSE), 550)
})