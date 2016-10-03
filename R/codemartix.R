#' Construct EQ5D code martix
#'
#' @param x object
#' @param y,z additional numeric vectors if \code{x} is numeric of length 5
#' @param const constant values (numeric of length 3)
#'
#' @return Object of class "codematrix", a \code{5 * 3} numerical matrix
#' with rows corresponding to EQ5D-dimensions (+ constant),
#' columns correcponding to response levels and individual values
#' corresponding to translation weights.
#' @export
#' @name codematrix
#'
#' @examples
#'
#' # Swedish code martix based on individual vectors for each response level
#' uk <- codematrix(
#'         rep(0, 6),
#'         c(.069, .104, .036, .123, .071, .081),
#'         c(.314, .214, .094, ..386, .237, .269))
#'
codematrix <- function(x, const, ...) UseMethod("codematrix", x)


#' @export
#' @rdname codematrix
codematrix.matrix <- function(x, const, ...) {

  stopifnot(
    is.numeric(const),
    length(const) == 3,
    nrow(x) == 5,
    ncol(x) == 3,
    is.numeric(x)
  )

  rownames(x) <- c(
    "mobility",
    "self care",
    "usual activity",
    "pain/discomfort",
    "anxiety/depression"
  )

  colnames(x) <- c(
    "no",
    "moderate",
    "severe"
  )

  structure(x, const = const, class = c("codematrix", "matrix"))
}



#' @export
#' @rdname codematrix
codematrix.numeric <- function(x, y = NULL, z = NULL, ...) {
  if (length(x) == 5 && !is.null(y) && !is.null(z))
    x <- c(x, y, z)
  stopifnot(length(x) == 15)
  codematrix(matrix(x, 5, 3), ...)
}

#' @export
#' @rdname codematrix
is.codematrix <- function(x)
  inherits(x, "codematrix")

#' @export
`-.codematrix` <- function(x) {
  x     <- -unclass(x)
  const <- attr(x, "const")
  attr(x, "const") <- c(const[1], -const[2:3])
  structure(x, class = c("codematrix", "matrix"))
}
