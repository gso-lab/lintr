#' Missing package linter
#'
#' Check for missing packages in `library()`, `require()`, `loadNamespace()` and `requireNamespace()` calls.
#'
#' @evalRd rd_tags("missing_package_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
missing_package_linter <- function() {
  Linter(function(source_expression) {

    if (is.null(source_expression$full_xml_parsed_content)) return(list())

    xml <- source_expression$full_xml_parsed_content

    call_xpath <- "//expr[
      (
        expr[SYMBOL_FUNCTION_CALL[text() = 'library' or text() = 'require']]
        and (
          expr[2][STR_CONST]
          or (
            expr[2][SYMBOL]
            and not(
              SYMBOL_SUB[text() = 'character.only']
              /following-sibling::expr[1]
              /NUM_CONST[text() = 'TRUE' or text() = 'T']
            )
          )
        )
      ) or (
        expr[SYMBOL_FUNCTION_CALL[text() = 'loadNamespace' or text() = 'requireNamespace']]
        and expr[2][STR_CONST]
      )
    ]"

    pkg_calls <- xml2::xml_find_all(xml, call_xpath)
    pkg_names <- get_r_string(xml2::xml_find_all(
      pkg_calls,
      "OP-LEFT-PAREN[1]/following-sibling::expr[1][SYMBOL | STR_CONST]"
    ))

    installed_packges <- .packages(all.available = TRUE)
    missing_ids <- which(!(pkg_names %in% installed_packges))

    line1 <- as.integer(xml2::xml_attr(pkg_calls, "line1"))
    col1 <- as.integer(xml2::xml_attr(pkg_calls, "col1"))
    col2 <- as.integer(xml2::xml_attr(pkg_calls, "col2"))

    lapply(missing_ids, function(i) {
      Lint(
        filename = source_expression$filename,
        line_number = line1[[i]],
        column_number = col1[[i]],
        type = "warning",
        message = sprintf("Package '%s' is not installed.", pkg_names[[i]]),
        line = source_expression$file_lines[line1[[i]]],
        ranges = list(c(col1[[i]], col2[[i]]))
      )
    })
  })
}
