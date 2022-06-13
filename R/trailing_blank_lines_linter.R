#' Trailing blank lines linter
#'
#' Check that there are no trailing blank lines in source code.
#'
#' @evalRd rd_tags("trailing_blank_lines_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
trailing_blank_lines_linter <- function(terminal_newline = TRUE) {
  Linter(function(source_expression) {
    # Only go over complete file
    if (is.null(source_expression$file_lines)) return(list())

    blanks <- re_matches(source_expression$file_lines,
                         rex(start, any_spaces, end))

    lints <- list()

    if (isTRUE(blanks[1])) {
      lints[[length(lints) + 1L]] <- Lint(
        filename = source_expression$filename,
        line_number = 1,
        column_number = 1,
        type = "style",
        message = "Blank lines at beginning of file disallowed.",
        line = source_expression$file_lines[[1]]
      )
    }

    line_number <- length(source_expression$file_lines)
    while (line_number > 0L && (is.na(blanks[[line_number]]) || isTRUE(blanks[[line_number]]))) {
      if (!is.na(blanks[[line_number]])) {
        lints[[length(lints) + 1L]] <- Lint(
          filename = source_expression$filename,
          line_number = line_number,
          column_number = 1,
          type = "style",
          message = "Trailing blank lines are superfluous.",
          line = source_expression$file_lines[[line_number]]
        )
      }
      line_number <- line_number - 1L
    }

    if (terminal_newline) {
      if (identical(source_expression$terminal_newline, FALSE)) { # could use isFALSE, but needs backports
        last_line <- tail(source_expression$file_lines, 1L)

        lints[[length(lints) + 1L]] <- Lint(
          filename = source_expression$filename,
          line_number = length(source_expression$file_lines),
          column_number = nchar(last_line) + 1L,
          type = "style",
          message = "Missing terminal newline.",
          line = last_line
        )
      }
    } else {
      if (identical(source_expression$terminal_newline, TRUE)) { # could use isTRUE, but needs backports
        last_line <- tail(source_expression$file_lines, 1L)

        if (last_line != "}") {
          lints[[length(lints) + 1L]] <- Lint(
            filename = source_expression$filename,
            line_number = length(source_expression$file_lines) + 1,
            column_number = 1,
            type = "style",
            message = "Terminal newline is superfluous.",
            line = last_line
          )
        }
      }
    }

    runs <- rle(blanks)
    true_runs <- runs$length[runs$values]

    if (any(true_runs > 1)) { ## if there are any consecutive TRUEs (i.e. blank lines)
      end <- cumsum(runs$lengths)
      begin <- c(1, lag(end)[-1] + 1)
      end_true <- end[runs$values & runs$lengths > 1]
      begin_true <- begin[runs$values & runs$lengths > 1]

      double_blanks <- unlist(lapply(
        paste0(begin_true, ":", end_true), function(x) eval(parse(text = x))
      ))

      if (length(double_blanks) > 0) {
        for (db in double_blanks) {
          lints[[length(lints) + 1L]] <- Lint(
            filename = source_expression$filename,
            line_number = db,
            column_number = 1,
            type = "style",
            message = "Consecutive blank lines are superfluous.",
            line = source_expression$file_lines[[db]]
          )
        }
      }
    }

    lints
  })
}
