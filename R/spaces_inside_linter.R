#' Spaces inside linter
#'
#' Check that parentheses and square brackets do not have spaces directly inside them, i.e., directly following an
#' opening delimiter or directly preceding a closing delimiter.
#'
#' @evalRd rd_tags("spaces_inside_linter")
#' @seealso
#'   [linters] for a complete list of linters available in lintr. \cr
#'   <https://style.tidyverse.org/syntax.html#parentheses>
#' @export
spaces_inside_linter <- function() {
  Linter(function(source_expression) {
    parens_tokens <- c(
      "'('",
      "')'")

    # using a regex here as checking each token is horribly slow
    re <- rex(
      list("(" %if_next_is% " ", none_of(end, "#")) %or%
        list(" " %if_prev_isnt% ",", ")")
    )

    all_parens_matches <- re_matches(source_expression$lines, re, global = TRUE, locations = TRUE)
    line_numbers <- as.integer(names(source_expression$lines))

    parens_res <- Map(
      function(line_matches, line_number) {
        apply(
          line_matches,
          1L,
          function(match) {
            start <- match[["start"]]
            if (is.na(start)) {
              return()
            }
            end <- match[["end"]]
            line <- source_expression$lines[[as.character(line_number)]]

            # make sure that the closing is not at the start of a new line (which is valid).
            start_of_line <- re_matches(line, rex(start, spaces, one_of("])")), locations = TRUE)

            if (is.na(start_of_line$start) || start_of_line$end != end) {
              pc <- source_expression$parsed_content
              is_parens_token <-
                any(pc[["line1"]] == line_number &
                      (pc[["col1"]] == end | pc[["col1"]] == start) &
                      pc[["token"]] %in% parens_tokens)

              if (is_parens_token) {
                Lint(
                  filename = source_expression$filename,
                  line_number = line_number,
                  column_number = if (substr(line, start, start) == " ") start else start + 1L,
                  type = "style",
                  message = "Do not place spaces around code in parentheses.",
                  line = line
                )
              }
            }
          }
        )
      },
      all_parens_matches,
      line_numbers
    )

    brackets_tokens <- c(
      "'['",
      "']'")

    # using a regex here as checking each token is horribly slow
    re <- rex(
      list("[", one_or_more(" ") %if_next_isnt% ",", none_of(end %or% "#" %or% " ")) %or%
        list(" " %if_prev_isnt% ",", "]")
    )
    all_brackets_matches <- re_matches(source_expression$lines, re, global = TRUE, locations = TRUE)
    line_numbers <- as.integer(names(source_expression$lines))

    brackets_res <- Map(
      function(line_matches, line_number) {
        apply(
          line_matches,
          1L,
          function(match) {
            start <- match[["start"]]
            if (is.na(start)) {
              return()
            }
            end <- match[["end"]]
            line <- source_expression$lines[[as.character(line_number)]]

            # make sure that the closing is not at the start of a new line (which is valid).
            start_of_line <- re_matches(line, rex(start, spaces, one_of("])")), locations = TRUE)

            if (is.na(start_of_line$start) || start_of_line$end != end) {
              pc <- source_expression$parsed_content

              is_brackets_token <-
                any(pc[["line1"]] == line_number &
                      (pc[["col1"]] == end | pc[["col1"]] == start) &
                      pc[["token"]] %in% brackets_tokens)

              if (is_brackets_token) {
                Lint(
                  filename = source_expression$filename,
                  line_number = line_number,
                  column_number = if (substr(line, start, start) == " ") start else start + 1L,
                  type = "style",
                  message = paste0("Do not place spaces around code in square brackets unless ",
                                   "directly before or after a comma."),
                  line = line
                )
              }
            }
          }
        )
      },
      all_brackets_matches,
      line_numbers
    )

    # using a regex here as checking each token is horribly slow
    re <- rex(
      list(" " %if_next_is% "[", none_of(end %or% "#" %or% " ")) %or%
        list(" " %if_prev_isnt% ",", "]")
    )
    open_brackets_matches <- re_matches(source_expression$lines, re, global = TRUE, locations = TRUE)
    line_numbers <- as.integer(names(source_expression$lines))

    open_brackets_res <- Map(
      function(line_matches, line_number) {
        apply(
          line_matches,
          1L,
          function(match) {
            start <- match[["start"]]
            if (is.na(start)) {
              return()
            }
            end <- match[["end"]]
            line <- source_expression$lines[[as.character(line_number)]]

            # make sure that the closing is not at the start of a new line (which is valid).
            start_of_line <- re_matches(line, rex(start, spaces, one_of("])")), locations = TRUE)

            if (is.na(start_of_line$start) || start_of_line$end != end) {
              pc <- source_expression$parsed_content

              is_brackets_token <-
                any(pc[["line1"]] == line_number &
                      (pc[["col1"]] == end | pc[["col1"]] == start) &
                      pc[["token"]] %in% brackets_tokens)

              if (is_brackets_token) {
                Lint(
                  filename = source_expression$filename,
                  line_number = line_number,
                  column_number = if (substr(line, start, start) == " ") start else start + 1L,
                  type = "style",
                  message = "Do not place spaces before brackets, unless it's a closing bracket directly preceded by a comma.",
                  line = line
                )
              }
            }
          }
        )
      },
      open_brackets_matches,
      line_numbers
    )

    return(c(parens_res, brackets_res, open_brackets_res))
  })
}
