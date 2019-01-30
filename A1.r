# 
# A1.data <- function() {
# 
#   file <- "A1.csv"
#   df <- read.csv2(file, stringsAsFactors = FALSE)
#   for(j in 3:ncol(df)) df[[j]] <- as.numeric(df[[j]] )
#   df <- df[seq(1, nrow(df), by=5), ] # sampling at 5 minute
# 
#   # prep on columns
#   df.act <- df[, -(3:7)]
#   for(j in 3:7) df.act[,j] <- rowSums(df[,c(j,j+5)])
#   names(df.act) <- sub("Ausserhaus_","",names(df.act))
# 
#   df.out <- df[, 1:12]
# 
#   fun <- function(d) {
#     # d <- df.act
#     d <- split(d[,-1], d$sg)
#     # cut time for nice plot (60/5 = 12 rows are one hour)
#     d <- lapply(d, function(x) x[(12+1):(nrow(x)-11),]) # plus 1 row at the end
#     row.nm <- d[[1]][,1]
#     col.nm <- names(d[[1]])[-1]
#     mx <- lapply(d, function(x) as.matrix(x[,-1]))
#     mxs <- lapply(mx, apply, 2,  function(x) smooth.spline(x, df = 20)[["y"]])
#     # spline can produce negative values
#     mxs <- lapply(mxs, function(x) {x[x < 0] <- 0; x})
#     mx <- lapply(mx, `rownames<-`, row.nm)
#     mxs <- lapply(mxs, `rownames<-`, row.nm)
#     mxs
#   }
# 
#   A1.act.data <- fun(df.act)
#   A1.out.data <- fun(df.out)
# 
#   # grp <- rep(c("indoor","outdoor","ontheway","media"), c(5,5,5,4))
#   single.hue <- list(
#     red    = rev(c("#fee5d9","#fcae91","#fb6a4a","#de2d26","#a50f15")),
#     violet = rev(c("#f2f0f7","#cbc9e2","#9e9ac8","#756bb1","#54278f")),
#     orange = rev(c("#feedde","#fdbe85","#fd8d3c","#e6550d","#a63603")),
#     grey   = rev(c("#f7f7f7","#cccccc","#969696","#636363","#252525")),
#     green  = rev(c("#edf8e9","#bae4b3","#74c476","#31a354","#006d2c")),
#     blue   = rev(c("#eff3ff","#bdd7e7","#6baed6","#3182bd","#08519c"))
#   )
# 
#   A1.act.color <- unlist(single.hue[c(1,2,5)])[1:14]
#   A1.out.color <- rep(sapply(single.hue[-4], `[`, 1), 2)
# 
#   s <- 35
#   colr <- rainbow(s)
#   # plot(seq(s), seq(s), pch = 21, cex = 2.5, col = colr, bg = colr)
#   # abline(v=seq(0,s,5))
#   colr <- colr[c(1:5, 12:8, 22:19)]
# 
#   A1.act.color <- colr
#   names(A1.act.color) <- colnames(A1.act.data[[1]])
#   names(A1.out.color) <- colnames(A1.out.data[[1]])
# 
#   # out
#   out <- c("A1.act.data", "A1.out.data", "A1.act.color", "A1.out.color")
#   for(i in out) assign(i, get(i), envir = .GlobalEnv)
# 
#   save(A1.act.data, A1.out.data, A1.act.color, A1.out.color, file = "A1.rdata")
# }
# 
# A1.data()

A1.act.plot <- function(data, color, stacked) {
  
  # data <- A1.act.data$SR; color <- A1.act.color
  # data <- A1.act.data$SR[, NULL]; color <- A1.act.color[, NULL]
  
  plot.first <- function() {
    par(fg = gr1, col.axis = gr1, las = 1, mar = c(5, 4, 4, 8) + 0.1)
    plot.default(NA, xlim = xlim, ylim = ylim, ann = FALSE, 
                 axes = FALSE, frame.plot = FALSE)
    xtick <- seq(xlim[1], xlim[2], by = 24)
    axis(1, xtick, rownames(data)[xtick])
    axis(2, axTicks(2), axTicks(2))
    segments(xtick, 0, xtick, ylim[2], lty = 1, col = gr2, lwd = .5)
    mtext("Zeit", side = 1, line = 3)
    mtext("%", side = 2, line = 3)
  }
  
  plot.normal <- function() {
    for(j in seq(coln))
      polygon(x = c(seq(rown), rev(seq(rown))), 
              y = c(rep(0,rown), rev(data[,j])), 
              col = sub("FF$","20", color[j]),
              border = sub("FF$","90", color[j]), 
              lwd = 1.5)
    legend(xlim[2], ylim[2], legend = colnames(data), cex = 1,
           bty = "n", xpd = TRUE, inset = c(-0.2,0), text.col = color)
  }
  
  stacked.polys <- function(d) {
    rownames(d) <- NULL
    x <- seq(nrow(d))
    top.old <- x*0
    polys <- setNames(vector(mode="list", ncol(d)), colnames(d))
    for(j in seq(polys)){
      top.new <- top.old + d[,j]
      polys[[j]] <- list(x = c(x, rev(x)), y = c(top.old, rev(top.new)))
      top.old <- top.new
    }
    polys
    }
  
  plot.stacked <- function() {
    colr <- color[names(polys)]
    for(j in seq(polys))
      polygon(polys[[j]], 
              border = sub("FF$","90", colr[j]), 
              col = sub("FF$","20", colr[j]), 
              lwd = 1.5
              )
    col.media <- intersect(colnames(data), media)
    if(length(col.media)){
      for(j in col.media) {
        polygon(x = c(seq(rown), rev(seq(rown))),
                y = c(rep(0,rown), data[, j]),
                col = sub("FF$","20", color[j]),
                border = sub("FF$","90", color[j]),
                lwd = 1.5)
      }
    }
    legend(xlim[2], ylim[2], legend = colnames(data), cex = 1,
           bty = "n", xpd = TRUE, inset = c(-0.2,0), text.col = color)
  }

  gr1 <- gray(0.2)
  gr2 <- gray(0.95)
  rown <- nrow(data)
  coln <- ncol(data)
  xlim <- c(1, rown)
  ylim <- c(0, if(coln) max(data) else 100)
  plot.first()
  if(ncol(data)==0) return()
  
  if(is.null(stacked)) {
    plot.normal() 
  } else {
    media <- c("TV","Radio","Internet","Lesen")
    cols <- setdiff(colnames(data), media)
    
    if(length(cols) < 1) return()
    polys <- stacked.polys(data[, cols, drop = FALSE])
    ylim <- c(0, max(max(polys[[length(polys)]]$y), max(ylim)))
    plot.first()
    plot.stacked()
  }

}


A1.out.plot <- function(data, color) {

  # data <- A1.out.data$SR; color <- A1.out.color
  # data <- A1.out.data$SR[, NULL]; color <- A1.out.color[, NULL]

  gr1 <- gray(0.2)
  gr2 <- gray(0.9)
  par(fg = gr1, col.axis = gr1, las = 1)
  rown <- nrow(data)
  coln <- ncol(data)
  xlim = c(1, rown)
  ylim = c(0, if(coln) max(data) else 100)
  plot.default(NA, xlim = xlim, ylim = ylim[2]*c(-1,1), ann = FALSE, 
               axes = FALSE, frame.plot = FALSE)
  xtick <- seq(xlim[1], xlim[2], by = 24)
  axis(1, xtick, rownames(data)[xtick])
  axis(2, axTicks(2), abs(axTicks(2)))
  segments(xtick, -ylim[2], xtick, ylim[2], col=gr2, lty=2)
  segments(xlim[1], 0, xlim[2], 0, col = gr2, lty = 1)
  mtext("Zeit", side = 1, line = 3)
  mtext("%", side = 2, line = 3)
  
  if(coln==0) return()

  pole <- ifelse(grepl("Ausserhaus",names(color)), 1, -1)
  for(j in seq(coln))
    polygon(x = c(xlim[1], seq(rown), xlim[2]),
            y = c(ylim[1], data[,j] * pole[j], ylim[1]),
            col = adjustcolor(color[j], alpha.f = .5), 
            border = adjustcolor(color[j], alpha.f = .7)
            # col = sub("FF$","30", color[j]),
            # border = sub("FF$","50", color[j])
    )
}
