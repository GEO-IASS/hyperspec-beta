# checking neural network predictions
library(caret)
library(ggrepel)
library(tidyr)
library(corrplot)
library(rasterVis)
source('R/fit_neural_net.R')

# visualize the spectral profiles for each of the training cases ---------------
wavelength_d <- plants %>%
  filter(is_train) %>% 
  dplyr::select(scientificname, starts_with('nm_'), indvidualid) %>%
  gather(wavelength, reflectance, -scientificname, -indvidualid) %>%
  mutate(wavelength = as.numeric(gsub('nm_', '', wavelength)))

# show the mean lines for each species
wavelength_d %>%
  group_by(scientificname, wavelength) %>%
  summarize(mean_reflectance = mean(reflectance)) %>%
  ggplot(aes(x = wavelength, y = mean_reflectance, color = scientificname)) + 
  geom_line() + 
  xlab('Wavelength (nm)') + 
  ylab('Reflectance') + 
  ggtitle("Species-level mean hyperspectral reflectance profiles") + 
  theme_minimal() + 
  scale_color_discrete(guide = guide_legend(title = "Species"))


# show means with among-individual variation
wavelength_d %>%
  group_by(scientificname, wavelength) %>%
  summarize(reflectance = mean(reflectance)) %>%
  ggplot(aes(x = wavelength, y = reflectance)) + 
  geom_line(color = 'red', size = 1.5) + 
  geom_line(aes(group = indvidualid), 
            alpha = .2, data = wavelength_d) + 
  xlab('Wavelength (nm)') + 
  ylab('Reflectance') + 
  ggtitle("Species-level hyperspectral reflectance profiles") + 
  facet_wrap(~scientificname, nrow = 2) + 
  theme_minimal()

# evaluate which regions in the electromagnetic spectrum contain the information
# i.e., which regions could potentially differentiate species? 
#   - a prerequisite for this is that there must be variation!!
#   - as a proxy, let's use standard deviation
wavelength_d %>%
  group_by(wavelength) %>%
  summarize(var = var(reflectance)) %>% 
  ggplot(aes(x = wavelength, y = var)) + 
  geom_line() + 
  ggtitle('Within-band spectral variance across all individuals and species') + 
  xlab('Wavelength (nm)') + 
  ylab(expression('Sample variance in reflectance')) + 
  theme_minimal()

# what about among-individual, within-species variation? 
wavelength_d %>%
  group_by(wavelength, scientificname) %>%
  summarize(var = var(reflectance)) %>% 
  ggplot(aes(x = wavelength, y = var)) + 
  geom_line() + 
  facet_wrap(~scientificname, nrow = 2) + 
  ggtitle('Within-band spectral variance by species') + 
  xlab('Wavelength (nm)') + 
  ylab(expression('Sample variance in reflectance')) + 
  theme_minimal()
# peaks are regions w/ high intRAspecific variation (individual to individual)



# visualize spectral redundance (that adjacent wavelengths are correlated) ----
cormat <- cor(hs_mat)
# set diagonal and upper triangular elements to NA
for (i in 1:nrow(cormat)) {
  for (j in i:ncol(cormat)) {
    cormat[i, j] <- NA
  }
}
correlation_raster <- raster(cormat)
wavelength_range <- range(wavelength_d$wavelength)
extent(correlation_raster) <- extent(rep(wavelength_range, 2))
p <- levelplot(correlation_raster, 
          xlab = 'Wavelength (nm)', ylab = 'Wavelength (nm)', 
          main = "Correlation among wavelengths", margin = FALSE)
devtools::source_gist('306e4b7e69c87b1826db') # gets a plotting function
diverge0(p, ramp = 'RdBu')
# blue along diagonal shows high correlation among adjacent wavelengths




# Generate confusion matrices -------------------------------------------------
test_df$scientificname <- as.factor(test_df$scientificname)
test_df$pred <- as.factor(test_df$pred)
levels(test_df$pred) <- levels(test_df$scientificname)
cm <- confusionMatrix(test_df$pred, test_df$scientificname)

# plot commonness vs. sensitivity
commonness <- c(table(test_df$scientificname))
sensitivity <- cm$byClass[, "Sensitivity"]
specificity <- cm$byClass[, "Specificity"]

check_df <- data.frame(Species = names(commonness), 
                       commonness = commonness, 
                       sensitivity = sensitivity, 
                       specificity = specificity)

ggplot(check_df, aes(x = commonness, y = sensitivity)) + 
  geom_point(size = 2) + 
  theme_minimal() + 
  xlab('Commonness') + 
  ylab('Sensitivity')

ggplot(check_df, aes(x = commonness, y = specificity)) + 
  geom_point(size = 2)  + 
  theme_minimal() + 
  xlab('Commonness') + 
  ylab('Specificity')

ggplot(check_df, aes(x = sensitivity, y = specificity)) + 
  geom_point(size = 3, alpha = .3) + 
  coord_equal() + 
  ylim(0, 1) +
  theme_minimal() + 
  xlab('Sensitivity') + 
  ylab('Specificity') + 
  geom_text_repel(aes(label = Species)) +
  ggtitle("Neural network classification results")
