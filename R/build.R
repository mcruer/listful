#' Build an Object Iteratively with a Single List
#'
#' This function takes an initial object and iteratively modifies it using a function and a list of arguments.
#'
#' @param obj The initial object to be modified.
#' @param .x A list of elements to be passed as the second argument to `.f`.
#' @param .f The function to apply to `obj` and elements of `.x`.
#' @param ... Additional arguments to pass to `.f`.
#'
#' @return The modified object.
#'
#' @examples
#' \dontrun{
#' build(0, 1:5, `+`)
#' }
#' @export
build <- function(obj, .x, .f, ...) {
  for (item in .x) {
    obj <- .f(obj, item, ...)
  }
  obj
}


#' Build an Object Iteratively with Two Lists
#'
#' This function takes an initial object and iteratively modifies it using a function and two lists of arguments.
#'
#' @param obj The initial object to be modified.
#' @param .x A list of elements to be passed as the second argument to `.f`.
#' @param .y A list of elements to be passed as the third argument to `.f`.
#' @param .f The function to apply to `obj`, elements of `.x`, and elements of `.y`.
#' @param ... Additional arguments to pass to `.f`.
#'
#' @return The modified object.
#'
#' @examples
#' \dontrun{
#' build2(0, 1:3, 4:6, `+`)
#' }
#' @export
build2 <- function(obj, .x, .y, .f, ...) {
  for (i in seq_along(.x)) {
    obj <- .f(obj, .x[[i]], .y[[i]], ...)
  }
  obj
}

#' Build an Object Iteratively with a List of Lists
#'
#' This function takes an initial object and iteratively modifies it using a function and a list of lists of arguments.
#'
#' @param obj The initial object to be modified.
#' @param .l A list of lists, where each inner list contains elements to be passed as arguments to `.f`.
#' @param .f The function to apply to `obj` and elements of `.l`.
#' @param ... Additional arguments to pass to `.f`.
#'
#' @return The modified object.
#'
#' @examples
#' \dontrun{
#' pbuild(0, list(1:3, 4:6), `+`)
#' }
#' @export
pbuild <- function(obj, .l, .f, ...) {
  n <- length(.l[[1]])
  for (i in seq_len(n)) {
    args <- lapply(.l, `[[`, i)
    obj <- do.call(.f, c(list(obj), args, list(...)))
  }
  obj
}



#I had previous versions of this function but they acted weird under
#certain situations. See code (commented out) below.

# build <- function(df, .x, .f, ...) {
#   fun <- rlang::as_function(.f)  # convert formula to function
#   walk(.x, ~ { df <<- fun(df, .x = .x, ...) })
#   df
# }
#
#
# build2 <- function(df, .x, .y, .f, ...) {
#   fun <- rlang::as_function(.f)
#   purrr::walk2(.x, .y, ~ { df <<- fun(df, .x = .x, .y = .y, ...) })
#   df
# }
#
#
# pbuild <- function(df, .l, .f, ...) {
#   fun <- rlang::as_function(.f)
#   purrr::pwalk(.l, ~ { df <<- fun(df, ..., .l = list(...)) })
#   df
# }
#
#





