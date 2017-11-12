Modif.pairs.panels<-function (x, smooth = TRUE, scale = FALSE, density = TRUE, ellipses = TRUE, 
          digits = 2, method = "pearson", pch = 20, lm = FALSE, cor = TRUE, 
          jiggle = FALSE, factor = 2, hist.col = "cyan", show.points = TRUE, 
          rug = TRUE, breaks = "Sturges", cex.cor = 1, wt = NULL, 
          ...) 
{
  "panel.hist.density" <- function(x, ...) {
    usr <- par("usr")
    on.exit(par(usr))
    par(usr = c(usr[1:2], 0, 1.5))
    h <- hist(x, breaks = breaks, plot = FALSE)
    breaks <- h$breaks
    nB <- length(breaks)
    y <- h$counts
    y <- y/max(y)
    rect(breaks[-nB], 0, breaks[-1], y, col = hist.col)
    if (density) {
      tryd <- try(d <- density(x, na.rm = TRUE, bw = "nrd", 
                               adjust = 1.2), silent = TRUE)
      if (class(tryd) != "try-error") {
        d$y <- d$y/max(d$y)
        lines(d)
      }
    }
    if (rug) 
      rug(x)
  }
  "panel.cor" <- function(x, y, digits = 2, prefix = "", ...) {
    usr <- par("usr")
    on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    if (is.null(wt)) {
      r <- mine(x, y, use = "pairwise", method = method)$MIC
    }
    else {
      r <- cor.wt(data.frame(x, y), w = wt[, c(1:2)])$r[1, 
                                                        2]
    }
    txt <- format(c(round(r, digits), 0.123456789), digits = digits)[1]
    txt <- paste(prefix, txt, sep = "")
    cex <- cex.cor * 0.8/strwidth(txt)
    if (scale) {
      cex1 <- cex * abs(r)
      if (cex1 < 0.25) 
        cex1 <- 0.25
      text(0.5, 0.5, txt, cex = cex1)
    }
    else {
      text(0.5, 0.5, txt, cex = cex)
    }
  }
  "panel.smoother" <- function(x, y, pch = par("pch"), col.smooth = "red", 
                               span = 2/3, iter = 3, ...) {
    xm <- mean(x, na.rm = TRUE)
    ym <- mean(y, na.rm = TRUE)
    xs <- sd(x, na.rm = TRUE)
    ys <- sd(y, na.rm = TRUE)
    r = cor(x, y, use = "pairwise", method = method)
    if (jiggle) {
      x <- jitter(x, factor = factor)
      y <- jitter(y, factor = factor)
    }
    if (show.points) 
      points(x, y, pch = pch, ...)
    ok <- is.finite(x) & is.finite(y)
    if (any(ok)) 
      lines(stats::lowess(x[ok], y[ok], f = span, iter = iter), 
            col = col.smooth, ...)
    panel.ellipse1(xm, ym, xs, ys, r, col.smooth = col.smooth, 
                   ...)
  }
  "panel.smoother.no.noellipse" <- function(x, y, pch = par("pch"), 
                                            col.smooth = "red", span = 2/3, iter = 3, ...) {
    xm <- mean(x, na.rm = TRUE)
    ym <- mean(y, na.rm = TRUE)
    xs <- sd(x, na.rm = TRUE)
    ys <- sd(y, na.rm = TRUE)
    r = cor(x, y, use = "pairwise", method = method)
    if (jiggle) {
      x <- jitter(x, factor = factor)
      y <- jitter(y, factor = factor)
    }
    if (show.points) 
      points(x, y, pch = pch, ...)
    ok <- is.finite(x) & is.finite(y)
    if (any(ok)) 
      lines(stats::lowess(x[ok], y[ok], f = span, iter = iter), 
            col = col.smooth, ...)
  }
  "panel.lm" <- function(x, y, pch = par("pch"), col.lm = "red", 
                         ...) {
    ymin <- min(y)
    ymax <- max(y)
    xmin <- min(x)
    xmax <- max(x)
    ylim <- c(min(ymin, xmin), max(ymax, xmax))
    xlim <- ylim
    if (jiggle) {
      x <- jitter(x, factor = factor)
      y <- jitter(y, factor = factor)
    }
    points(x, y, pch = pch, ylim = ylim, xlim = xlim, ...)
    ok <- is.finite(x) & is.finite(y)
    if (any(ok)) 
      #abline(lm(y[ok] ~ x[ok]), col = col.lm, ...)
  }
  "panel.lm.ellipse" <- function(x, y, pch = par("pch"), col.lm = "red", 
                                 ...) {
    ymin <- min(y)
    ymax <- max(y)
    xmin <- min(x)
    xmax <- max(x)
    ylim <- c(min(ymin, xmin), max(ymax, xmax))
    xlim <- ylim
    if (jiggle) {
      x <- jitter(x, factor = factor)
      y <- jitter(y, factor = factor)
    }
    points(x, y, pch = pch, ylim = ylim, xlim = xlim, ...)
    ok <- is.finite(x) & is.finite(y)
    if (any(ok)) 
     # abline(lm(y[ok] ~ x[ok]), col = col.lm, ...)
    xm <- mean(x, na.rm = TRUE)
    ym <- mean(y, na.rm = TRUE)
    xs <- sd(x, na.rm = TRUE)
    ys <- sd(y, na.rm = TRUE)
    r = cor(x, y, use = "pairwise", method = method)
    panel.ellipse1(xm, ym, xs, ys, r, col.smooth = col.lm, 
                   ...)
  }
  "panel.ellipse1" <- function(x = 0, y = 0, xs = 1, ys = 1, 
                               r = 0, col.smooth, add = TRUE, segments = 51, ...) {
    angles <- (0:segments) * 2 * pi/segments
    unit.circle <- cbind(cos(angles), sin(angles))
    if (!is.na(r)) {
      if (abs(r) > 0) 
        theta <- sign(r)/sqrt(2)
      else theta = 1/sqrt(2)
      shape <- diag(c(sqrt(1 + r), sqrt(1 - r))) %*% matrix(c(theta, 
                                                              theta, -theta, theta), ncol = 2, byrow = TRUE)
      ellipse <- unit.circle %*% shape
      ellipse[, 1] <- ellipse[, 1] * xs + x
      ellipse[, 2] <- ellipse[, 2] * ys + y
      points(x, y, pch = 19, col = col.smooth, cex = 1.5)
      lines(ellipse, ...)
    }
  }
  "panel.ellipse" <- function(x, y, pch = par("pch"), col.smooth = "red", 
                              ...) {
    segments = 51
    xm <- mean(x, na.rm = TRUE)
    ym <- mean(y, na.rm = TRUE)
    xs <- sd(x, na.rm = TRUE)
    ys <- sd(y, na.rm = TRUE)
    r = cor(x, y, use = "pairwise", method = method)
    if (jiggle) {
      x <- jitter(x, factor = factor)
      y <- jitter(y, factor = factor)
    }
    if (show.points) 
      points(x, y, pch = pch, ...)
    angles <- (0:segments) * 2 * pi/segments
    unit.circle <- cbind(cos(angles), sin(angles))
    if (!is.na(r)) {
      if (abs(r) > 0) 
        theta <- sign(r)/sqrt(2)
      else theta = 1/sqrt(2)
      shape <- diag(c(sqrt(1 + r), sqrt(1 - r))) %*% matrix(c(theta, 
                                                              theta, -theta, theta), ncol = 2, byrow = TRUE)
      ellipse <- unit.circle %*% shape
      ellipse[, 1] <- ellipse[, 1] * xs + xm
      ellipse[, 2] <- ellipse[, 2] * ys + ym
      points(xm, ym, pch = 19, col = col.smooth, cex = 1.5)
      if (ellipses) 
        lines(ellipse, ...)
    }
  }
  old.par <- par(no.readonly = TRUE)
  on.exit(par(old.par))
  if (missing(cex.cor)) 
    cex.cor <- 1
  for (i in 1:ncol(x)) {
    if (is.character(x[[i]])) {
      x[[i]] <- as.numeric(as.factor(x[[i]]))
      colnames(x)[i] <- paste(colnames(x)[i], "*", sep = "")
    }
  }
  if (!lm) {
    if (smooth) {
      if (ellipses) {
        pairs(x, diag.panel = panel.hist.density, upper.panel = panel.cor, 
              lower.panel = panel.smoother, pch = pch, ...)
      }
      else {
        pairs(x, diag.panel = panel.hist.density, upper.panel = panel.cor, 
              lower.panel = panel.smoother.no.noellipse, 
              pch = pch, ...)
      }
    }
    else {
      pairs(x, diag.panel = panel.hist.density, upper.panel = panel.cor, 
            lower.panel = panel.ellipse, pch = pch, ...)
    }
  }
  else {
    if (!cor) {
      if (ellipses) {
        pairs(x, diag.panel = panel.hist.density, upper.panel = panel.lm.ellipse, 
              lower.panel = panel.lm.ellipse, pch = pch, 
              ...)
      }
      else {
        pairs(x, diag.panel = panel.hist.density, upper.panel = panel.lm, 
              lower.panel = panel.lm, pch = pch, ...)
      }
    }
    else {
      if (ellipses) {
        pairs(x, diag.panel = panel.hist.density, upper.panel = panel.cor, 
              lower.panel = panel.lm.ellipse, pch = pch, 
              ...)
      }
      else {
        pairs(x, diag.panel = panel.hist.density, upper.panel = panel.cor, 
              lower.panel = panel.lm, pch = pch, ...)
      }
    }
  }
}
