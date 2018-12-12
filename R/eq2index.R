#' Calculate EQ5D-index from individual EQ5D-levels
#'
#' @param x EQ5D-data, see examples
#' @param cm codematrix, either as a a \code{\link{codematrix}} object
#' or a character name of such object
#' @param ... arguments pased between methods
#'
#' @return numeric index vector
#' @export
#'
#' @examples
#' # individual numeric vector
#' eq2index(c(1, 2, 3, 2, 1), "se")
#' eq2index(c(1, 2, 3, 2, 1), "uk")
eq2index <- function(x, ...) UseMethod("eq2index", x)

#' @export
eq2index.character <- function(x, ...) {

  if (!all(nchar(x, keepNA = TRUE) == 5))
    stop("All codestrings must be of length 5!")
  if (!all(grepl("^[1-3]{5}$", x)))
    stop("All codes must be 1-5")
  stopifnot(
    all(nchar(x, keepNA = TRUE) == 5),
    all(grepl("^[1-3]{5}$", x))
  )
  eq2index(t(vapply(strsplit(x, ""), as.numeric, numeric(5))), ...)
}

#' @export
#' @rdname eq2index
eq2index.matrix <- function(x, cm, ...) {

  if (is.character(cm))
    cm <- get(cm)
  # cm should always have negative values
  if (all(cm >= 0))
    cm <- -cm

  # Check conditions
  if (!is.numeric(x))
    stop(        "Values must be numeric!")
  if (ncol(x) != 5)
    stop(sprintf("EQ5D have 5 dimensions, not %s!", ncol(x)))
  if (max(x, na.rm = TRUE) > 5)
    stop(sprintf("All values should be <= 5, not %s!", max(x, na.rm = TRUE)))
  if (min(x, na.rm = TRUE) < 1)
    stop(sprintf("All values should be >= 1, not %s!", min(x, na.rm = TRUE)))
  if (!is.codematrix(cm))
    stop(        "cm is not a code matrix!")

  n     <- nrow(x)
  const <- attr(cm, "const")

  # Assume that cm is a numeric vector of length 15
  # We can add multilpes of 3 to x to turn every level response
  # to the index position of corresponding values in cm
  A <- matrix(rep(3 * 0:4, n), n, byrow = TRUE)
  X <- matrix(t(cm)[x + A], n)

  # Find all cases which have at least one 2 or 3
  # N3 <- as.logical(rowSums(x == 3))
  N3 <- matrixStats::rowAnys(x, value = 3)
  N2 <- !N3 & matrixStats::rowAnys(x, value = 2)
  #N  <- cbind(rep(1, n), N2, N3)

  res <- const[1] + const[2] * N2 + const[3] * N3 + rowSums(X)

  #res <- N %*% const + X %*% rep(1, 5)
  c(res)
}


#' @export
eq2index.factor     <- function(x, ...) eq2index(as.character(x, ...))
#' @export
eq2index.numeric    <- function(x, ...) eq2index(t(x), ...)
#' @export
eq2index.data.frame <- function(x, ...) eq2index(as.matrix(x), ...)
