library(dplyr)
library(sp)
library(raster)
library(nnet)
library(RCurl)


# load neural net plotting functions
root.url<-'https://gist.githubusercontent.com/fawda123'
raw.fun<-paste(
  root.url,
  '5086859/raw/cc1544804d5027d82b70e74b83b3941cd2184354/nnet_plot_fun.r',
  sep='/'
)
script<-getURL(raw.fun, ssl.verifypeer = FALSE)
eval(parse(text = script))
rm('script','raw.fun')


cubes <- readRDS('output/plot_cubes.rds')

# Load in situ plant data
insitu_path <- "data/D17_2013_vegStr.csv"
plants <- read.csv(insitu_path, stringsAsFactors = FALSE) %>%
  filter(siteid == 'SJER')

# check that all plots are in both datasets
all(plants$plotid %in% names(cubes))
length(setdiff(plants$plotid, names(cubes))) == 0

# divide into training and test sets (50% and 50%)
plots <- unique(plants$plotid)
set.seed(786121246)
train <- sample(length(plots), size = round(length(plots) / 2))
train_plots <- plots[train]
test_plots <- setdiff(plots, train_plots)

plants$is_train <- plants$plotid %in% train_plots

# append data frame to contain nanometer values
wavelengths <- names(cubes[[1]])
n_wavelengths <- length(wavelengths)
nm_matrix <- array(dim = c(nrow(plants), n_wavelengths))
colnames(nm_matrix) <- wavelengths
plants <- cbind(plants, nm_matrix)

# make spatialpointsdataframe
plant_coords <- SpatialPoints(cbind(plants$easting, 
                              plants$northing))
crs(plant_coords) <- crs(cubes[[1]])

# for each plot, we want to extract the values of the hyperspectral
# data at the plant locations
is_hs_column <- grepl(pattern = "nm_", x = names(plants))
for (i in seq_along(cubes)) {
  hs_subset <- raster::extract(cubes[[i]], plant_coords)
  relevant_rows <- complete.cases(hs_subset)
  plants[relevant_rows, is_hs_column] <- hs_subset[relevant_rows, ]
}

# remove any trees that do not have hyperspectral data
missing_hyperspectral <- apply(plants[, is_hs_column], 
                               1, 
                               FUN = function(x) any(is.na(x)))
plants <- plants[!missing_hyperspectral, ]

# visualize species frequencies
par(mar = c(5, 16, 2, 1))
barplot(sort(table(plants$scientific)), horiz = TRUE, las = 2,
        xlab = 'Frequency')
par(mar = c(5, 4, 4, 2) + 0.1)


# produce PC axes
hs_mat <- as.matrix(plants[, is_hs_column])
pca <- prcomp(hs_mat)
plot(pca)
biplot(pca)
plot(summary(pca)$importance[3, ], 
     xlab = "PC axis", 
     ylab = "Cumulative proportion of variance explained")

pc_mat <- pca$x[, 1:10]
plants_merged <- cbind(plants, pc_mat)
is_pc <- grepl("PC", x = names(plants_merged))


# get input features
xcolumns <- names(plants_merged)[is_pc]
xformula <- paste(xcolumns, collapse = "+")


# train model
test_df <- subset(plants_merged, !is_train)
ideal <- class.ind(plants_merged$scientificname)

nn <- nnet(x = plants_merged[plants_merged$is_train, xcolumns],
           y = ideal[plants_merged$is_train, ],
           size = 7,
           softmax = TRUE, 
           maxit = 1000)
test_df$pred <- predict(nn, test_df[, xcolumns], type = 'class')
test_df[, c('scientificname', 'pred')]


par(mfrow = c(1, 1))
plot(nn, nid = F)
title('Neural network structure')

# get plot by species predicted abundance matrix 
specnames <- unique(plants_merged$scientificname)
nspecies <- length(specnames)
test_plots
pred_mat <- matrix(nrow = length(test_plots), ncol = nspecies)
for (i in 1:nrow(pred_mat)) {
  for (j in 1:ncol(pred_mat)) {
    indices <- which(test_df$plotid == test_plots[i] & 
                       test_df$pred == specnames[j])
    predicted_number <- length(indices)
    pred_mat[i, j] <- predicted_number
  }
}
rownames(pred_mat) <- test_plots
colnames(pred_mat) <- specnames
pred_mat
