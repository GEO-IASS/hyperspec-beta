#####
##### Code to calculate diversity metrics based on HSI data
#####

source("R/fit_neural_net.R")

str(pred_mat)
pred_mat


D.iter.q <- function(data, level, q){
  Spoon <- matrix(data = NA, ncol = 2, nrow = 6)
  for (i in 1:6){
    temp <- d(data, lev = level, q = i - 1, boot = TRUE, boot.arg = list(num.iter = 1e3))
    Spoon[i, 1] <- temp[[1]]
    Spoon[i, 2] <- temp[[2]]
  }
  return(Spoon)
}

# calculate the a, b, and g metrics for SJER
SJER.HSI.a <- D.iter.q(data = pred_mat, level = "alpha", q = 5)
SJER.HSI.b <- D.iter.q(data = pred_mat, level = "beta", q = 5)
SJER.HSI.g <- D.iter.q(data = pred_mat, level = "gamma", q = 5)

# plot the results
plotDeMoney <- function(x0, y0, x1, y1, mean, dot){
  points(y = mean, x = dot, pch = 15, col = 'grey', cex = 1.5)
  segments(x0, y0, x1, y1, lwd = 3.5)
}

# now create the plot for alpha at SJER
plot(x = seq(from = 0, to = 5, length.out = 4), y = seq(from = 0, to = 4, length.out = 4), xaxt = "n", type = "n", ylab = expression(paste(italic(alpha))), las = 1, xlab = expression(paste(italic("q"))), main = "SJER HSI")
axis(1, at = c(0, 1, 2, 3, 4, 5))
# populate the plot
for(i in 1:6){
  plotDeMoney(x0 = i - 1, y0 = SJER.HSI.a[i , 1] - SJER.HSI.a[i, 2], x1 = i - 1, y1 = SJER.HSI.a[i , 1] + SJER.HSI.a[i, 2], mean = SJER.HSI.a[i, 1], dot = i - 1)
}

# create plot for beta diversity
plot(x = seq(from = 0, to = 5, length.out = 4), y = seq(from = 0, to = 4.2, length.out = 4), xaxt = "n", type = "n", ylab = expression(paste(italic(beta))), las = 1, xlab = expression(paste(italic("q"))), main = "SJER HSI")
axis(1, at = c(0, 1, 2, 3, 4, 5))
# populate the plot
for(i in 1:6){
  plotDeMoney(x0 = i - 1, y0 = SJER.HSI.b[i , 1] - SJER.HSI.b[i, 2], x1 = i - 1, y1 = SJER.HSI.b[i , 1] + SJER.HSI.b[i, 2], mean = SJER.HSI.b[i, 1], dot = i - 1)
}

# plot for gamma diversity
plot(x = seq(from = 0, to = 5, length.out = 4), y = seq(from = 0, to = 10, length.out = 4), xaxt = "n", type = "n", ylab = expression(paste(italic(gamma))), las = 1, xlab = expression(paste(italic("q"))), main = "SJER HSI")
axis(1, at = c(0, 1, 2, 3, 4, 5))
# populate the plot
for(i in 1:6){
  plotDeMoney(x0 = i - 1, y0 = SJER.HSI.g[i , 1] - SJER.HSI.g[i, 2], x1 = i - 1, y1 = SJER.HSI.g[i , 1] + SJER.HSI.g[i, 2], mean = SJER.HSI.g[i, 1], dot = i - 1)
}
