# Extracted from test-qlm_code.R:298

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "quallmer", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
skip_if_not_installed("ellmer")
type_obj <- ellmer::type_object(score = ellmer::type_number("Score"))
codebook <- qlm_codebook("Test", "Test prompt", type_obj)
mock_chat <- structure(list(), class = "ellmer_chat")
mock_results <- data.frame(id = 1:2, score = c(0.5, 0.8))
mockery::stub(qlm_code, "ellmer::chat", mock_chat)
mockery::stub(qlm_code, "ellmer::parallel_chat_structured", mock_results)
warnings_list <- list()
withCallingHandlers(
    qlm_code(c("text1", "text2"), codebook, model = "test/model",
             fake_argument = "value"),
    warning = function(w) {
      warnings_list <<- c(warnings_list, list(conditionMessage(w)))
      invokeRestart("muffleWarning")
    }
  )
expect_true(any(grepl("fake_argument", unlist(warnings_list))))
