test_that(".show_alert runs without error", {
  testServer(
    function(input, output, session) {
      # Mock showModal to do nothing
      with_mocked_bindings(
        code = {
          expect_no_error(.show_alert("Test message"))
        },
        showModal = function(...) invisible()
      )
    }
  )
})