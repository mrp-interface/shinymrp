# tests/testthat/test-package.R

test_that(".onLoad sets ggplot2 theme correctly", {
  # Store original theme
  original_theme <- ggplot2::theme_get()
  
  # Call .onLoad
  .onLoad(libname = "test", pkgname = "shinymrp")
  
  # Get current theme
  current_theme <- ggplot2::theme_get()
  
  # Test the settings
  expect_equal(current_theme$text$size, 20)
  expect_equal(current_theme$plot.title$hjust, 0.5)
  expect_equal(current_theme$plot.caption$hjust, 0.5)
  expect_equal(current_theme$plot.margin, ggplot2::margin(1, 1, 1, 1, "cm"))
  
  # Restore original theme
  ggplot2::theme_set(original_theme)
})

test_that(".onLoad creates correct ggplot2 element types", {
  # Store original theme
  original_theme <- ggplot2::theme_get()
  
  # Call .onLoad
  .onLoad("test", "shinymrp")
  
  current_theme <- ggplot2::theme_get()
  
  # Test that elements are proper ggplot2 classes
  expect_s3_class(current_theme$text, "element_text")
  expect_s3_class(current_theme$plot.title, "element_text")
  expect_s3_class(current_theme$plot.caption, "element_text")
  expect_s3_class(current_theme$plot.margin, "margin")
  
  # Test value types
  expect_type(current_theme$text$size, "double")
  expect_type(current_theme$plot.title$hjust, "double")
  expect_type(current_theme$plot.caption$hjust, "double")
  
  # Restore original theme
  ggplot2::theme_set(original_theme)
})

test_that(".onLoad can be called multiple times", {
  # Store original theme
  original_theme <- ggplot2::theme_get()
  
  # Call multiple times - should not error
  expect_no_error(.onLoad("lib1", "pkg1"))
  expect_no_error(.onLoad("lib2", "pkg2"))
  
  # Theme should still be correct
  current_theme <- ggplot2::theme_get()
  expect_equal(current_theme$text$size, 20)
  
  # Restore original theme
  ggplot2::theme_set(original_theme)
})

test_that(".onLoad overrides existing theme", {
  # Store original theme
  original_theme <- ggplot2::theme_get()
  
  # Set a different theme first
  ggplot2::theme_set(ggplot2::theme_dark())
  
  # Verify we start with different settings
  dark_theme <- ggplot2::theme_get()
  expect_false(dark_theme$text$size == 20)
  
  # Call .onLoad - should override
  .onLoad("test", "shinymrp")
  
  # Should now have our custom theme
  current_theme <- ggplot2::theme_get()
  expect_equal(current_theme$text$size, 20)
  expect_equal(current_theme$plot.title$hjust, 0.5)
  
  # Restore original theme
  ggplot2::theme_set(original_theme)
})