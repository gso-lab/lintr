object_name_xpath <- local({
  xp_assignment_target <- paste(
    "not(preceding-sibling::OP-DOLLAR)",
    "and ancestor::expr[",
    " following-sibling::LEFT_ASSIGN",
    " or preceding-sibling::RIGHT_ASSIGN",
    " or following-sibling::EQ_ASSIGN",
    "]",
    "and not(ancestor::expr[",
    " preceding-sibling::OP-LEFT-BRACKET",
    " or preceding-sibling::LBB",
    "])"
  )

  paste0(
    "//SYMBOL[", xp_assignment_target, "] | ",
    "//STR_CONST[", xp_assignment_target, "] | ",
    "//SYMBOL_FORMALS"
  )
})

function_name_xpath <- local({
  xp_assignment_target <- paste(
    "not(preceding-sibling::OP-DOLLAR)",
    "and ancestor::expr[",
    " following-sibling::LEFT_ASSIGN",
    " or preceding-sibling::RIGHT_ASSIGN",
    " or following-sibling::EQ_ASSIGN",
    "]",
    "and not(ancestor::expr[",
    " preceding-sibling::OP-LEFT-BRACKET",
    " or preceding-sibling::LBB",
    "])"
  )

  paste0(
    "//expr[expr/FUNCTION/text() = 'function']/expr/SYMBOL[", xp_assignment_target, "] | ",
    "//expr_or_assign_or_help[expr/FUNCTION/text() = 'function']/expr/SYMBOL[", xp_assignment_target, "] |",
    "//expr[FUNCTION/text() = 'function']/expr/expr/SYMBOL[", xp_assignment_target, "]"
  )
})

global_name_xpath <- local({
  xp_assignment_target <- paste(
    "ancestor::expr/expr/*[substring-after(name(), 'CONS')]",
    " or ancestor::expr/expr/expr/SYMBOL_FUNCTION_CALL[text() = 'c'][ancestor::expr/expr/expr/*[substring-after(name(), 'CONS')]]"
  )

  paste0(
    "/exprlist/expr/expr/SYMBOL[", xp_assignment_target, "]"
  )
})

variable_name_xpath <- local({
  xp_assignment_target <- paste(
    "not(preceding-sibling::OP-DOLLAR)",
    "and ancestor::expr[",
    " following-sibling::LEFT_ASSIGN",
    " or (preceding-sibling::RIGHT_ASSIGN and not(preceding-sibling::expr/OP-RIGHT-BRACE))",
    " or following-sibling::EQ_ASSIGN",
    "]",
    "and not(ancestor::expr[",
    " preceding-sibling::OP-LEFT-BRACKET",
    " or preceding-sibling::LBB",
    " or (following-sibling::expr/FUNCTION[text() = 'function'] and following-sibling::expr/expr/OP-RIGHT-BRACE)",
    "])",
    "and not(", global_name_xpath, ")"
  )

  paste0(
    "//SYMBOL[", xp_assignment_target, "] | ",
    "//STR_CONST[", xp_assignment_target, "] | ",
    "//SYMBOL_FORMALS"
  )
})

#' Object name linter
#'
#' Check that object names conform to a naming style.
#' The default naming styles are "snake_case" and "symbols".
#'
#' @param styles A subset of
#'   \Sexpr[stage=render, results=rd]{lintr:::regexes_rd}. A name should
#'   match at least one of these styles.
#' @evalRd rd_tags("object_name_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
object_name_linter <- function(styles = c("snake_case", "symbols")) {
  styles <- match.arg(styles, names(style_regexes), several.ok = TRUE)

  lint_message <- paste0(
    "Variable and function name style should be ",
    glue::glue_collapse(styles, sep = ", ", last = " or "), "."
  )

  Linter(function(source_expression) {
    if (is.null(source_expression$full_xml_parsed_content)) return(list())

    xml <- source_expression$full_xml_parsed_content

    assignments <- xml2::xml_find_all(xml, object_name_xpath)

    # Retrieve assigned name
    nms <- strip_names(
      xml2::xml_text(assignments)
    )

    generics <- c(
      declared_s3_generics(xml),
      imported_s3_generics(namespace_imports(find_package(source_expression$filename)))$fun,
      .base_s3_generics
    )
    generics <- unique(generics[nzchar(generics)])

    style_matches <- lapply(styles, function(style) {
      check_style(nms, style, generics)
    })

    matches_a_style <- Reduce(`|`, style_matches)

    lapply(
      assignments[!matches_a_style],
      xml_nodes_to_lint,
      source_expression,
      lint_message = lint_message,
      type = "style",
      global = TRUE
    )
  })
}

#' Function name linter
#'
#' Check that function names conform to a naming style.
#' The default naming styles is "camelCase".
#'
#' @param styles A subset of
#'   \Sexpr[stage=render, results=rd]{lintr:::regexes_rd}. A name should
#'   match at least one of these styles.
#' @evalRd rd_tags("function_name_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
function_name_linter <- function(styles = c("camelCase")) {
  styles <- match.arg(styles, names(style_regexes), several.ok = TRUE)

  lint_message <- paste0(
    "Function name style should be ",
    glue::glue_collapse(styles, sep = ", ", last = " or "), "."
  )

  Linter(function(source_expression) {
    if (is.null(source_expression$full_xml_parsed_content)) return(list())

    xml <- source_expression$full_xml_parsed_content

    arrow_assignments <- xml2::xml_find_all(xml, function_name_xpath)
    assigns <- xml2::xml_find_all(
      xml,
      "//expr[expr[SYMBOL_FUNCTION_CALL[text() = 'assign']]]/expr[2]"
    )
    generic_or_std <- xml2::xml_find_all(
      xml,
      "//expr[expr[SYMBOL_FUNCTION_CALL[text() = 'setGeneric' or text() = 'standardGeneric']]]/expr[2]"
    )
    method_f <- xml2::xml_find_all(
      xml,
      "//expr[expr[SYMBOL_FUNCTION_CALL[text() = 'setMethod']]]/expr[2]"
    )
    method_def <- xml2::xml_find_all(
      xml,
      "//expr[expr[SYMBOL_FUNCTION_CALL[text() = 'setMethod']]]/expr[4]"
    )

    # Retrieve assigned name
    arrow_nms <- strip_names(xml2::xml_text(arrow_assignments))
    assign_nms <- strip_names(xml2::xml_text(assigns))
    generic_nms <- strip_names(xml2::xml_text(generic_or_std))
    method_nms <- stats::setNames(strip_names(xml2::xml_text(method_def)), strip_names(xml2::xml_text(method_f)))

    # remove method dispatch if setGeneric or setMethod exists
    keeps <- !arrow_nms %in% unname(method_nms)
    arrow_assignments <- arrow_assignments[keeps]
    arrow_nms <- arrow_nms[keeps]
    keeps <- !assign_nms %in% unname(method_nms)
    assigns <- assigns[keeps]
    assign_nms <- assign_nms[keeps]
    dispatches <- sapply(
      names(method_nms), function(nm) {
        if (nm %in% generic_nms) return(strsplit(method_nms[nm], "\\.")[[1]][1]) else return(method_nms[nm])
      },
      simplify = TRUE, USE.NAMES = FALSE
    )

    # remove dupes
    ass <- c(arrow_assignments, assigns, generic_or_std, method_def)
    nms <- c(arrow_nms, assign_nms, generic_nms, dispatches)

    generics <- c(
      declared_s3_generics(xml),
      imported_s3_generics(namespace_imports(find_package(source_expression$filename)))$fun,
      .base_s3_generics
    )
    generics <- unique(generics[nzchar(generics)])

    style_matches <- lapply(styles, function(style) {
      check_style(nms, style, generics)
    })

    matches_a_style <- Reduce(`|`, style_matches)

    lapply(
      ass[!matches_a_style],
      xml_nodes_to_lint,
      source_expression,
      lint_message = lint_message,
      type = "style",
      global = TRUE
    )
  })
}

#' Variable name linter
#'
#' Check that non-global and non-function object names conform to a naming style.
#' The default naming styles are either of "snake_case" or "dotted.case".
#'
#' @param styles A subset of
#'   \Sexpr[stage=render, results=rd]{lintr:::regexes_rd}. A name should
#'   match at least one of these styles.
#' @evalRd rd_tags("variable_name_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
variable_name_linter <- function(styles = c("snake_case", "dotted.case")) {
  styles <- match.arg(styles, names(style_regexes), several.ok = TRUE)

  lint_message <- paste0(
    "Variable name style should be ",
    glue::glue_collapse(styles, sep = ", ", last = " or "), "."
  )

  Linter(function(source_expression) {
    if (is.null(source_expression$full_xml_parsed_content)) return(list())

    xml <- source_expression$full_xml_parsed_content

    assignments <- xml2::xml_find_all(xml, variable_name_xpath)

    # Retrieve assigned name
    nms <- strip_names(
      xml2::xml_text(assignments)
    )

    generics <- c(
      declared_s3_generics(xml),
      imported_s3_generics(namespace_imports(find_package(source_expression$filename)))$fun,
      .base_s3_generics
    )
    generics <- unique(generics[nzchar(generics)])

    style_matches <- lapply(styles, function(style) {
      check_style(nms, style, generics)
    })

    matches_a_style <- Reduce(`|`, style_matches)

    lapply(
      assignments[!matches_a_style],
      xml_nodes_to_lint,
      source_expression,
      lint_message = lint_message,
      type = "style",
      global = TRUE
    )
  })
}

#' Global variable name linter
#'
#' Check that global variable/object names conform to a naming style.
#' The default naming styles are either of "SNAKE_CASE" or "DOTTED.CASE".
#'
#' @param styles A subset of
#'   \Sexpr[stage=render, results=rd]{lintr:::regexes_rd}. A name should
#'   match at least one of these styles.
#' @evalRd rd_tags("global_name_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
global_name_linter <- function(styles = c("SNAKE_CASE", "DOTTED.CASE")) {
  styles <- match.arg(styles, names(style_regexes), several.ok = TRUE)

  lint_message <- paste0(
    "Global variable name style should be ",
    glue::glue_collapse(styles, sep = ", ", last = " or "), "."
  )

  Linter(function(source_expression) {
    if (is.null(source_expression$full_xml_parsed_content)) return(list())

    xml <- source_expression$full_xml_parsed_content

    assignments <- xml2::xml_find_all(xml, global_name_xpath)
    # Retrieve assigned name
    nms <- strip_names(
      xml2::xml_text(assignments)
    )

    # remove setMethod function definitions from results
    method_def <- xml2::xml_find_all(
      xml,
      "//expr[expr[SYMBOL_FUNCTION_CALL[text() = 'setMethod']]]/expr[4]"
    )
    method_nms <- strip_names(xml2::xml_text(method_def))
    keeps <- !nms %in% method_nms
    nms <- nms[keeps]
    assignments <- assignments[keeps]

    generics <- c(
      declared_s3_generics(xml),
      imported_s3_generics(namespace_imports(find_package(source_expression$filename)))$fun,
      .base_s3_generics
    )
    generics <- unique(generics[nzchar(generics)])

    style_matches <- lapply(styles, function(style) {
      check_style(nms, style, generics)
    })

    matches_a_style <- Reduce(`|`, style_matches)

    lapply(
      assignments[!matches_a_style],
      xml_nodes_to_lint,
      source_expression,
      lint_message = lint_message,
      type = "style",
      global = TRUE
    )
  })
}

check_style <- function(nms, style, generics = character()) {
  conforming <- re_matches(nms, style_regexes[[style]])

  # mark empty names and NA names as conforming
  conforming[!nzchar(nms) | is.na(conforming)] <- TRUE

  if (!all(conforming)) {
    possible_s3 <- re_matches(
      nms[!conforming],
      rex(start, capture(name = "generic", or(generics)), ".", capture(name = "method", something), end)
    )
    if (!all(is.na(possible_s3))) {
      has_generic <- possible_s3$generic %in% generics

      # If they are not conforming, but are S3 methods then ignore them
      conforming[!conforming][has_generic] <- TRUE
    }
    # exclude namespace hooks like .onLoad, .Last.lib, etc (#500)
    is_special <- is_special_function(nms[!conforming])
    conforming[!conforming][is_special] <- TRUE
  }
  conforming
}

# Remove quotes or other things from names
strip_names <- function(x) {
  x <- re_substitutes(x, rex(start, some_of(".", quote, "`", "%", "$", "@")), "")
  x <- re_substitutes(x, rex(some_of(quote, "`", "<", "-", "%", "$", "@"), end), "")
  x
}

# see ?".onLoad", ?Startup, and ?quit. Remove leading dot to match behavior of strip_names().
#   All of .onLoad, .onAttach, and .onUnload are used in base packages,
#   and should be caught in is_base_function; they're included here for completeness / stability
#   (they don't strictly _have_ to be defined in base, so could in principle be removed).
#   .Last.sys and .First.sys are part of base itself, so aren't included here.
special_funs <- c(
  "onLoad",
  "onAttach",
  "onUnload",
  "onDetach",
  "Last.lib",
  "First",
  "Last"
)

is_special_function <- function(x) {
  x %in% special_funs
}

loweralnum <- rex(one_of(lower, digit))
upperalnum <- rex(one_of(upper, digit))

style_regexes <- list(
  "symbols"     = rex(start, zero_or_more(none_of(alnum)), end),
  "PascalCase"  = rex(start, maybe("."), upper, zero_or_more(alnum), end),
  "camelCase"   = rex(start, maybe("."), lower, zero_or_more(alnum), end),
  "snake_case"  = rex(start, maybe("."), some_of(lower, digit), any_of("_", lower, digit), end),
  "SNAKE_CASE"  = rex(start, maybe("."), some_of(upper, digit), any_of("_", upper, digit), end),
  "dotted.case" = rex(start, maybe("."), one_or_more(loweralnum), zero_or_more(dot, one_or_more(loweralnum)), end),
  "DOTTED.CASE" = rex(start, maybe("."), one_or_more(upperalnum), zero_or_more(dot, one_or_more(upperalnum)), end),
  "lowercase"   = rex(start, maybe("."), one_or_more(loweralnum), end),
  "UPPERCASE"   = rex(start, maybe("."), one_or_more(upperalnum), end)
)

regexes_rd <- toString(paste0("\\sQuote{", names(style_regexes), "}"))

#' Object length linter
#'
#' Check that object names are not too long.
#' The length of an object name is defined as the length in characters, after removing extraneous parts:
#'
#'  * generic prefixes for implementations of S3 generics, e.g. `as.data.frame.my_class` has length 8.
#'  * leading `.`, e.g. `.my_hidden_function` has length 18.
#'  * "%%" for infix operators, e.g. `%my_op%` has length 5.
#'  * trailing `<-` for assignment functions, e.g. `my_attr<-` has length 7.
#'
#' @param length maximum variable name length allowed.
#' @evalRd rd_tags("object_length_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
object_length_linter <- function(length = 30L) {
  lint_message <- paste("Variable and function names should not be longer than", length, "characters.")

  Linter(function(source_expression) {
    if (is.null(source_expression$full_xml_parsed_content)) return(list())

    xml <- source_expression$full_xml_parsed_content

    assignments <- xml2::xml_find_all(xml, object_name_xpath)

    # Retrieve assigned name
    nms <- strip_names(
      xml2::xml_text(assignments)
    )

    ns_imports <- namespace_imports(find_package(source_expression$filename))
    generics <- strip_names(c(
      declared_s3_generics(xml),
      imported_s3_generics(ns_imports)$fun,
      .base_s3_generics
    ))
    generics <- unique(generics[nzchar(generics)])

    # Remove generic function names from generic implementations
    # This only lints S3 implementations if the class names are too long, still lints generics if they are too long.
    nms_stripped <- re_substitutes(nms, rex(start, or(generics), "."), "")

    too_long <- nchar(nms_stripped) > length

    lapply(
      assignments[too_long],
      xml_nodes_to_lint,
      source_expression = source_expression,
      lint_message = lint_message,
      type = "style",
      global = TRUE
    )
  })
}
