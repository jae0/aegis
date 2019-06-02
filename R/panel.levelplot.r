
  panel.levelplot =  function (x, y, z, subscripts, at = pretty(z), shrink, labels = FALSE, 
    label.style = c("mixed", "flat", "align"), contour = FALSE, 
    region = TRUE, col = add.line$col, lty = add.line$lty, lwd = add.line$lwd, 
    ..., col.regions = regions$col, alpha.regions = regions$alpha, rez=NULL ) 
{
  # copy from lattice: panel.levelplot
  # modifying the grid.rect() to be a user specified (fixed) value
    require(grid)

    if (length(subscripts) == 0) 
        return()
    regions <- trellis.par.get("regions")
    label.style <- match.arg(label.style)
    x.is.factor <- is.factor(x)
    y.is.factor <- is.factor(y)
    x <- as.numeric(x)
    y <- as.numeric(y)
    z <- as.numeric(z)
    zcol <- level.colors(z, at, col.regions, colors = TRUE)
    x <- x[subscripts]
    y <- y[subscripts]
    minXwid <- if (length(unique(x)) > 1) 
        min(diff(sort(unique(x))))
    else 1
    minYwid <- if (length(unique(x)) > 1) 
        min(diff(sort(unique(y))))
    else 1
    fullZrange <- range(as.numeric(z), finite = TRUE)
    z <- z[subscripts]
    zcol <- zcol[subscripts]
    shrinkx <- c(1, 1)
    shrinky <- c(1, 1)
    if (!missing(shrink)) {
        if (is.numeric(shrink)) {
            shrinkx <- rep(shrink, length.out = 2)
            shrinky <- rep(shrink, length.out = 2)
        }
        else if (is.list(shrink)) {
            shrinkx <- rep(shrink[[1]], length.out = 2)
            shrinky <- rep(shrink[[1]], length.out = 2)
            if ("x" %in% names(shrink)) 
                shrinkx <- rep(shrink$x, length.out = 2)
            if ("y" %in% names(shrink)) 
                shrinky <- rep(shrink$y, length.out = 2)
        }
        else warning("Invalid 'shrink' parameter ignored")
    }
    scaleWidth <- function(z, min = 0.8, max = 0.8, zl = range(z, 
        finite = TRUE)) {
        if (diff(zl) == 0) 
            rep(0.5 * (min + max), length(z))
        else min + (max - min) * (z - zl[1])/diff(zl)
    }
    if (x.is.factor) {
        ux <- sort(unique(x[!is.na(x)]))
        lx <- rep(1, length(ux))
        cx <- ux
    }
    else {
        ux <- sort(unique(x[!is.na(x)]))
        bx <- if (length(ux) > 1) 
            c(3 * ux[1] - ux[2], ux[-length(ux)] + ux[-1], 3 * 
                ux[length(ux)] - ux[length(ux) - 1])/2
        else ux + c(-0.5, 0.5) * minXwid
        lx <- diff(bx)
        cx <- (bx[-1] + bx[-length(bx)])/2
    }
    if (y.is.factor) {
        uy <- sort(unique(y[!is.na(y)]))
        ly <- rep(1, length(uy))
        cy <- uy
    }
    else {
        uy <- sort(unique(y[!is.na(y)]))
        by <- if (length(uy) > 1) 
            c(3 * uy[1] - uy[2], uy[-length(uy)] + uy[-1], 3 * 
                uy[length(uy)] - uy[length(uy) - 1])/2
        else uy + c(-0.5, 0.5) * minYwid
        ly <- diff(by)
        cy <- (by[-1] + by[-length(by)])/2
    }
    idx <- match(x, ux)
    idy <- match(y, uy)
    if (region) {
        grid.rect(x = cx[idx], y = cy[idy], width = rez[1], 
            height = rez[2], default.units = "native", gp = gpar(fill = zcol, 
                lwd = 0.00001, col = "transparent", alpha = alpha.regions))
    }
    if (contour) {
        cpl <- current.panel.limits(unit = "cm")
        asp <- diff(cpl$ylim)/diff(cpl$xlim)
        if (is.logical(labels) && !labels) 
            labels <- NULL
        else {
            if (is.characterOrExpression(labels)) 
                labels <- list(labels = labels)
            text <- trellis.par.get("add.text")
            tmp <- list(col = text$col, alpha = text$alpha, cex = text$cex, 
                fontfamily = text$fontfamily, fontface = text$fontface, 
                font = text$font)
            labels <- if (is.list(labels)) 
                updateList(tmp, labels)
            else tmp
            if (!is.characterOrExpression(labels$labels)) 
                labels$labels <- format(at, trim = TRUE)
        }
        add.line <- trellis.par.get("add.line")
        m <- matrix(NA_real_, nrow = length(ux), ncol = length(uy))
        m[(idy - 1) * length(ux) + idx] <- z
        clines <- contourLines(x = ux, y = uy, z = m, nlevels = length(at), 
            levels = at)
        for (val in clines) {
            llines(val, col = col, lty = lty, lwd = lwd)
            if (length(val$x) > 5) {
                if (!is.null(labels)) {
                  slopes <- diff(val$y)/diff(val$x)
                  if (label.style == "flat") {
                    textloc <- which.min(abs(slopes))
                    rotangle <- 0
                  }
                  else if (label.style == "align") {
                    rx <- range(ux)
                    ry <- range(uy)
                    depth <- pmin(pmin(val$x - rx[1], rx[2] - 
                      val$x)/diff(rx), pmin(val$y - ry[1], ry[2] - 
                      val$y)/diff(ry))
                    textloc <- min(which.max(depth), length(slopes))
                    rotangle <- atan(asp * slopes[textloc] * 
                      diff(rx)/diff(ry)) * 180/base::pi
                  }
                  else if (label.style == "mixed") {
                    rx <- range(ux)
                    ry <- range(uy)
                    depth <- pmin(pmin(val$x - rx[1], rx[2] - 
                      val$x)/diff(rx), pmin(val$y - ry[1], ry[2] - 
                      val$y)/diff(ry))
                    textloc <- which.min(abs(slopes))
                    rotangle <- 0
                    if (depth[textloc] < 0.05) {
                      textloc <- min(which.max(depth), length(slopes))
                      rotangle <- atan(asp * slopes[textloc] * 
                        diff(rx)/diff(ry)) * 180/base::pi
                    }
                  }
                  else stop("Invalid label.style")
                  i <- match(val$level, at)
                  ltext(labels$labels[i], adj = c(0.5, 0), srt = rotangle, 
                    col = labels$col, alpha = labels$alpha, cex = labels$cex, 
                    font = labels$font, fontfamily = labels$fontfamily, 
                    fontface = labels$fontface, x = 0.5 * (val$x[textloc] + 
                      val$x[textloc + 1]), y = 0.5 * (val$y[textloc] + 
                      val$y[textloc + 1]))
                }
            }
        }
    }
}



