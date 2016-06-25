## Analysis of vegetation diversity for the NEON D17 region
### Ultimately want to compare diversity estimates (alpha, beta, and gamma) between ground and remote data


set.seed(786121246)

library("raster")
library("rgdal")
library("vegan")
library("vegetarian")
library("dplyr")
options(stringAsFactors = FALSE)

sessID <- sessionInfo()

# save and load data for future ease
# save(file = "SJER_SOAP_veg.RData", list = ls())
# load(file = "SJER_SOAP_veg.RData")

### Now to import the ground truthed data from D17


D17_veg <- read.csv("data/D17_2013_vegStr.csv", header = TRUE)
dim(D17_veg)
str(D17_veg)
head(D17_veg)

### Now to seperate data by site and plot ID, then generate an ecological count matrix


# test run with only SJER data
CA_data <- D17_veg %>% group_by(plotid) %>% select(siteid, plotid, taxonid) 
head(CA_data)

# This will sum the number of each species idnetified in each plot 
CA_sites <- table(CA_data$plotid, CA_data$taxonid)
dim(CA_sites)
dim(CA_sites[, colSums(CA_sites^2) == 0]) # No columns are completely empty


# Parse into SJER site
SJER_site <- CA_sites[1:18, ]
dim(SJER_site)
head(SJER_site)
tail(SJER_site)

dim(SJER_site[, colSums(SJER_site^2) == 0]) # 14 species not found at any SJER plot



# Parse into SOAP site
SOAP_site <- CA_sites[19:36, ]
dim(SOAP_site)
head(SOAP_site)
tail(SOAP_site)

dim(SOAP_site[, colSums(SOAP_site^2) == 0]) # 8 species not found at any SOAP plot

### With our data parsed we can estimate D for the SJER and SOAP sites
# this is a one off to run the function automatically for and only use three arguements

D.one <- function(data, level, q){
		iter_q <- d(data, lev = level, q = q, boot = TRUE, boot.arg = list(num.iter = 1e3))
	return(iter_q)
}
D.one(data = SJER_site, level = "alpha", q = 0)


# Now a more general function to calculate for a range of values
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
SJER.out.a <- D.iter.q(data = SJER_site, level = "alpha", q = 5)
SJER.out.b <- D.iter.q(data = SJER_site, level = "beta", q = 5)
SJER.out.g <- D.iter.q(data = SJER_site, level = "gamma", q = 5)

# calculate the a, b, and g metrics for SOAP
SOAP.out.a <- D.iter.q(data = SOAP_site, level = "alpha", q = 5)
SOAP.out.b <- D.iter.q(data = SOAP_site, level = "beta", q = 5)
SOAP.out.g <- D.iter.q(data = SOAP_site, level = "gamma", q = 5)


### Now plot the data and standard errors


# a little function to plot
plotDeMoney <- function(x0, y0, x1, y1, mean, dot){
	points(y = mean, x = dot, pch = 15, col = 'grey', cex = 1.5)
  segments(x0, y0, x1, y1, lwd = 3.5)
}

# now create the plot for alpha at SJER
plot(x = seq(from = 0, to = 5, length.out = 4), y = seq(from = 0, to = 3, length.out = 4), xaxt = "n", type = "n", ylab = expression(paste(italic(alpha))), las = 1, xlab = expression(paste(italic("q"))), main = expression(paste("SJER ", italic(" in situ"))))
axis(1, at = c(0, 1, 2, 3, 4, 5))
# populate the plot
for(i in 1:6){
  plotDeMoney(x0 = i - 1, y0 = SJER.out.a[i , 1] - SJER.out.a[i, 2], x1 = i - 1, y1 = SJER.out.a[i , 1] + SJER.out.a[i, 2], mean = SJER.out.a[i, 1], dot = i - 1)
}
    


# create plot for alpha diversity at SOAP
plot(x = seq(from = 0, to = 5, length.out = 4), y = seq(from = 0, to = 6, length.out = 4), xaxt = "n", type = "n", ylab = expression(paste(italic(alpha))), las = 1, xlab = expression(paste(italic("q"))), main = expression(paste("SOAP ", italic(" in situ"))))
axis(1, at = c(0, 1, 2, 3, 4, 5))
# populate the plot
for(i in 1:6){
  plotDeMoney(x0 = i - 1, y0 = SOAP.out.a[i , 1] - SOAP.out.a[i, 2], x1 = i - 1, y1 = SOAP.out.a[i , 1] + SOAP.out.a[i, 2], mean = SOAP.out.a[i, 1], dot = i - 1)
}


### Create plots for beta diversity

# now create the plot for beta at SJER
plot(x = seq(from = 0, to = 5, length.out = 4), y = seq(from = 0, to = 4.2, length.out = 4), xaxt = "n", type = "n", ylab = expression(paste(italic(beta))), las = 1, xlab = expression(paste(italic("q"))), main = expression(paste("SJER ", italic(" in situ"))))
axis(1, at = c(0, 1, 2, 3, 4, 5))
# populate the plot
for(i in 1:6){
  plotDeMoney(x0 = i - 1, y0 = SJER.out.b[i , 1] - SJER.out.b[i, 2], x1 = i - 1, y1 = SJER.out.b[i , 1] + SJER.out.b[i, 2], mean = SJER.out.b[i, 1], dot = i - 1)
}

# create plot for beta diversity at SOAP
plot(x = seq(from = 0, to = 5, length.out = 4), y = seq(from = 0, to = 6, length.out = 4), xaxt = "n", type = "n", ylab = expression(paste(italic(beta))), las = 1, xlab = expression(paste(italic("q"))), main = expression(paste("SOAP ", italic(" in situ"))))
axis(1, at = c(0, 1, 2, 3, 4, 5))
# populate the plot
for(i in 1:6){
  plotDeMoney(x0 = i - 1, y0 = SOAP.out.b[i , 1] - SOAP.out.b[i, 2], x1 = i - 1, y1 = SOAP.out.b[i , 1] + SOAP.out.b[i, 2], mean = SOAP.out.b[i, 1], dot = i - 1)
}


### lastly the plots for gamma diversity
# now create the plot for gamma at SJER
plot(x = seq(from = 0, to = 5, length.out = 4), y = seq(from = 0, to = 12, length.out = 4), xaxt = "n", type = "n", ylab = expression(paste(italic(gamma))), las = 1, xlab = expression(paste(italic("q"))),main = expression(paste("SJER ", italic(" in situ"))))
axis(1, at = c(0, 1, 2, 3, 4, 5))
# populate the plot
for(i in 1:6){
  plotDeMoney(x0 = i - 1, y0 = SJER.out.g[i , 1] - SJER.out.g[i, 2], x1 = i - 1, y1 = SJER.out.g[i , 1] + SJER.out.g[i, 2], mean = SJER.out.g[i, 1], dot = i - 1)
}

# create the plot for gamma diversity at SOAP
plot(x = seq(from = 0, to = 5, length.out = 4), y = seq(from = 0, to = 18, length.out = 4), xaxt = "n", type = "n", ylab = expression(paste(italic(gamma))), las = 1, xlab = expression(paste(italic("q"))), main = expression(paste("SOAP ", italic(" in situ"))))
axis(1, at = c(0, 1, 2, 3, 4, 5))
# populate the plot
for(i in 1:6){
  plotDeMoney(x0 = i - 1, y0 = SOAP.out.g[i , 1] - SOAP.out.g[i, 2], x1 = i - 1, y1 = SOAP.out.g[i , 1] + SOAP.out.g[i, 2], mean = SOAP.out.g[i, 1], dot = i - 1)
}


### ordinate the first two PCs using quantitative data (could also use incidence)

```{r ordinate}
# ordination with PCoA

CA.bray <- vegdist(CA_sites, method = "bray")
CA.pcoa <- cmdscale(CA.bray, k = (nrow(CA_sites) - 1), eig = TRUE, add = TRUE) # only 24 dims with information if you don't use the Callaiz correction
# CA.pcoa

fig1 <- ordiplot(scores(CA.pcoa)[, c(1,2)], type = "none", main = "", las = 1, xlab = "PCoA 1", ylab = "PCoA 2", display = "sites", ylim = c(-0.7, 0.7))
points(fig1, "sites", pch = 19, col = c(rep("red", 18), rep("black", 18)), cex = 2)
legend("topleft", legend = c("SOAP", "SJER"), pch = 19, pt.cex = 2, col = c("red", "black"), bty = "n", cex = 1.5)

# what if we jsut use incidence, not quantified occurance? 

CA_01 <- CA_sites
CA_01[CA_01 > 0] <- 1

CA.01.bray <- vegdist(CA_01, method = "bray")
CA.01.pcoa <- cmdscale(CA.01.bray, k = (nrow(CA_01) - 1), eig = TRUE, add = TRUE) 
# CA.01.pcoa

fig2 <- ordiplot(scores(CA.01.pcoa)[, c(1,2)], type = "none", main = "", las = 1, xlab = "PCoA 1", ylab = "PCoA 2", display = "sites", ylim = c(-0.8, 0.8))
points(fig2, "sites", pch = 19, col = c(rep("red", 18), rep("black", 18)), cex = 2)
legend("topleft", legend = c("SOAP", "SJER"), pch = 19, pt.cex = 2, col = c("red", "black"), bty = "n", cex = 1.5)
