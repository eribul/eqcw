## Summary statistics

#' Improvement Ratio
#'
#' @param pre,post pre- and post-treatment PROM-values
#' @param best best (desired) post-treatment PROM value
#' @param alpha significance level for confidence interval
#'
#' @return object of class "ir"
#' @export
ir <- function(pre, post, best, alpha = 0.05) {
  l     <- length(pre)
  nonNA <- !is.na(pre) & !is.na(post)
  pre   <- pre[nonNA]
  post  <- post[nonNA]
  N     <- sum(nonNA)

  if (l > N) {
    warning(l - N, " missing observations ignored!")
  }
  y  <- post - pre
  x  <- best - pre
  ir <- (mean(post) - mean(pre)) /
        (best - mean(pre))

  sd <-
    sqrt(
      1 / (N * mean(x) ^ 2) *
        (ir ^ 2 * stats::var(x) + stats::var(y) - 2 * ir * stats::cov(x, y))
    )

  ci <- ir + c(-1, 1) * stats::qnorm(1 - alpha / 2) * sd

  structure(
    list(
      x = x, y = y, N = N, ir = ir, sd = sd,
      ci.low = ci[1], ci.high  = ci[2], alpha = alpha
    ),
    class = "ir"
  )
}

#' @export
print.ir <- function(x, ...) {
  ci <- paste(ci.name(x$alpha), "=", ci(x$ci.low, x$ci.high))
  message(
    sprintf(
      "Improvement ratio for %.0f non-missing cases

      IR      = %f (%.0f %%)
      sd      = %f (%.0f %%)
      %s",
      x$N, x$ir, x$ir * 100, x$sd, x$sd * 100, ci
    )
  )
}

#' @export
as.data.frame.ir <- function(x, row.names = NULL, optional = FALSE, ...) {
  x$x <- x$y <- NULL
  x[ci.name(x$alpha)] <- ci(x$ci.low, x$ci.high)
  as.data.frame(unclass(x), ...)
}

ci      <- function(l, h) sprintf("(%.3f, %.3f)", l, h)
ci.name <- function(alpha) sprintf("CI %.0f %%", (1 - alpha ) * 100)

#' Test if Improvemant Ratios from different groups are equal
#'
#' @param x,y output from \code{\link{ir}}
#'
#' @return Numeric vector with estimated Z-value and associated p-value.
#' @export
ir.test <- function(x, y) {
  se <- function(v) v$sd / sqrt(v$N)
  z  <- (x$ir - y$ir) / (se(x) + se(y))
  p  <- 2 * stats::pnorm(-abs(z))
  c(z = z, p = p)
}


#' Trend test of Improvement Ratio
#'
#' @param x output from \code{\link{ir}}
#' @param sds
#' @param score
#'
#' @return Object of class "htest"
#' @export
ir.trendtest <- function(x, sds, score = seq_along(x)) {
  dname  <- paste(deparse(substitute(x)), "with scores:", paste(score, collapse = " "))
  chisq  <- stats::anova(stats::lm(x ~ score, weights = sds / x))["score", "Sum Sq"]
  structure(
    list(
      statistic = stats::setNames(chisq, "X-squared"),
      parameter = c(df = 1),
      p.value   = stats::pchisq(chisq, 1, lower.tail = FALSE),
      method    = "Chi-squared Test for Trends",
      data.name = dname
    ),
    class = "htest"
  )
}
