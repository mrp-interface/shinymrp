test_that(".waiter_ui function works correctly", {
  # Valid types return tagList with 2 element
  for (type in .const()$ui$loading_types) {
    expect_s3_class(.waiter_ui(type), "shiny.tag.list")
  }

  expect_error(
    .waiter_ui("invalid"),
    "Assertion on 'loading_type' failed"
  )
})

test_that(".create_guide function works correctly", {
  for (section in .const()$ui$guide_sections) {
    expect_s3_class(.create_guide(section), "shiny.tag")
  }

  expect_error(
    .create_guide("invalid"),
    "Assertion on 'open' failed"
  )
})

test_that(".create_model_tab function works correctly", {
  expect_s3_class(
    .create_model_tab(
      function(id) id,
      example_model(),
      NULL
    ),
    "shiny.tag"
  )
})