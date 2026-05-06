# Extracted from test-qlm_segment.R:272

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "quallmer", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
skip_if_not_installed("ellmer")
skip_if_not_installed("quanteda")
cb <- qlm_codebook(
    "Test", "Segment.",
    schema = ellmer::type_object(tag = ellmer::type_string("Tag"))
  )
mock_chat <- structure(list(), class = "ellmer_chat")
mock_results <- list(tibble::tibble(text = "A.", tag = "a"))
mockery::stub(qlm_segment, "ellmer::chat", mock_chat)
mockery::stub(qlm_segment, "ellmer::parallel_chat_structured", mock_results)
warnings_seen <- character(0)
withCallingHandlers(
    qlm_segment("Text.", cb, model = "test/model", not_a_real_arg = TRUE),
    warning = function(w) {
      warnings_seen <<- c(warnings_seen, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
expect_true(any(grepl("not_a_real_arg", warnings_seen)))
