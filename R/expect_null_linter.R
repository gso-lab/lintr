#' expect_null Linter
#'
#' Require usage of `expect_null(x)` over `expect_equal(x, NULL)` and similar
#' usages.
#'
#' [testthat::expect_null()] exists specifically for testing for `NULL` objects.
#' [testthat::expect_equal()], [testthat::expect_identical()], and
#' [testthat::expect_true()] can also be used for such tests,
#' but it is better to use the tailored function instead.
#'
#' @evalRd rd_tags("expect_null_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
expect_null_linter <- function() {
  Linter(function(source_expression) {
    if (length(source_expression$parsed_content) == 0L) {
      return(list())
    }

    xml <- source_expression$xml_parsed_content

    # two cases two match:
    #  (1) expect_{equal,identical}(x, NULL) (or NULL, x)
    #  (2) expect_true(is.null(x))
    xpath <- "//expr[
      (
        SYMBOL_FUNCTION_CALL[text() = 'expect_equal' or text() = 'expect_identical']
        and following-sibling::expr[position() <= 2 and NULL_CONST]
      ) or (
        SYMBOL_FUNCTION_CALL[text() = 'expect_true']
        and following-sibling::expr[1][expr[SYMBOL_FUNCTION_CALL[text() = 'is.null']]]
      )
    ]"

    bad_expr <- xml2::xml_find_all(xml, xpath)

    lapply(
      bad_expr,
      xml_nodes_to_lint,
      source_expression,
      function(expr) {
        matched_function <- xml2::xml_text(xml2::xml_find_first(expr, "SYMBOL_FUNCTION_CALL"))
        if (matched_function %in% c("expect_equal", "expect_identical")) {
          sprintf("expect_null(x) is better than %s(x, NULL)", matched_function)
        } else {
          "expect_null(x) is better than expect_true(is.null(x))"
        }
      },
      type = "warning"
    )
  })
}
