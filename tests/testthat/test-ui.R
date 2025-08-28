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


test_that(".vis_cat_select works", {
  # Test general case with linking geography
  expect_setequal(
    .vis_cat_select(
      metadata = list(
        special_case = NULL,
        is_timevar = TRUE,
        family = "binomial"
      ),
      linkdata = list(link_geo = "zip")
    ),
    c("indiv", "geo", "outcome")
  )

  # Test time-varying data w/out linking geography
  expect_setequal(
    .vis_cat_select(
      metadata = list(
        special_case = NULL,
        is_timevar = TRUE,
        family = "binomial"
      ),
      linkdata = list(link_geo = NULL)
    ),
    c("indiv", "outcome")
  )

  # Test cross-sectional data w/ linking geography
  expect_setequal(
    .vis_cat_select(
      metadata = list(
        special_case = NULL,
        is_timevar = FALSE,
        family = "binomial"
      ),
      linkdata = list(link_geo = NULL)
    ),
    c("indiv")
  )

})

test_that(".vis_subcat_select works", {
  ### COVID data
  md_covid <- list(special_case = "covid", is_timevar = TRUE)
  ld_covid <- list(link_geo = "zip")

  # Test individual characteristics
  out <- .vis_subcat_select("indiv", md_covid, ld_covid)
  expect_equal(out$label, "2. Select characteristic")
  expect_setequal(out$choices, c("sex", "race", "age"))  

  # Test geographic characteristics (covariates
  # available for zip-level data)
  out <- .vis_subcat_select("geo", md_covid, ld_covid)
  expect_equal(out$label, "2. Select characteristic")
  expect_setequal(
    out$choices,
    c("sample", "college", "poverty",
      "employment", "income", "urbanicity", "adi")
  )

  # Test outcome
  out <- .vis_subcat_select("outcome", md_covid, ld_covid)
  expect_equal(out$label, "2. Select plot type")
  expect_setequal(out$choices, c("overall", "by_geo"))

  ### Polling data
  md_poll <- list(special_case = "poll", is_timevar = FALSE)
  ld_poll <- list(link_geo = "state")

  # Test individual characteristics
  out <- .vis_subcat_select("indiv", md_poll, ld_poll)
  expect_setequal(out$choices, c("sex", "race", "age", "edu"))  

  # Test geographic characteristics
  out <- .vis_subcat_select("geo", md_poll, ld_poll)
  expect_setequal(out$choices, c("sample"))

  # Test outcome (only by_geo available for cross-sectional data)
  out <- .vis_subcat_select("outcome", md_poll, ld_poll)
  expect_setequal(out$choices, c("by_geo"))

  ### Test without linking geography
  ld_no_geo <- list(link_geo = NULL)

  # Test time-varying data
  md_no_geo <- list(special_case = NULL, is_timevar = TRUE)

  out <- .vis_subcat_select("geo", md_no_geo, ld_no_geo)
  expect_setequal(out$choices, character(0))

  out <- .vis_subcat_select("outcome", md_no_geo, ld_no_geo)
  expect_setequal(out$choices, c("overall"))

  # Test cross-sectional data
  md_no_geo <- list(special_case = NULL, is_timevar = FALSE)

  out <- .vis_subcat_select("outcome", md_no_geo, ld_no_geo)
  expect_setequal(out$choices, character(0))

  # Test invalid category
  out <- .vis_subcat_select("invalid", md_covid, ld_covid)
  expect_equal(out$label, character(0))
  expect_null(out$choices)

})

test_that(".vis_ui works", {
  ns <- function(id) id

  # Individual characteristics
  for (demo in .const()$vars$demo) {
    golem::expect_shinytag(
      .vis_ui(ns, "indiv", demo)
    )
  }

  # Geographic characteristics
  for (covar in c("sample", .const()$vars$covar)) {
    golem::expect_shinytag(
      .vis_ui(ns, "geo", covar)
    )
  }

  # Outcome measure
  golem::expect_shinytag(
    .vis_ui(ns, "outcome", "overall")
  )
  golem::expect_shinytaglist(
    .vis_ui(ns, "outcome", "by_geo")
  )

  # Invalid category
  expect_error(
    .vis_ui(ns, "invalid", "overall"),
    "Assertion on 'category' failed"
  )

  # Invalid subcategory
  expect_error(
    .vis_ui(ns, "indiv", "invalid"),
    "Assertion on 'subcategory' failed"
  ) 
})


