library(tidyverse)
library(ggthemes)

d <- read_csv('data/neon_plants.csv') %>%
  select(-X1, -starts_with('site'), -plotid, 
         -easting, -northing, -taxonid, -pointid, -individualdistance, -individualazimuth, 
         -starts_with('dbh'), - starts_with('basal'), -starts_with('canopy'), 
         -starts_with('stem'), -livingcanopy, -inplotcanopy, -materialsampleid, 
         -chm_height, -maxcanopydiam) %>%
  gather(wavelength, reflectance, -scientificname, -indvidualid) %>%
  mutate(wavelength = parse_number(wavelength)) %>%
  filter(reflectance > 0, reflectance < 1) %>%
  group_by(scientificname) %>%
  mutate(n_examples = n() / length(unique(wavelength))) %>%
  filter(n_examples > 30, wavelength > 100) %>%
  mutate(reflectance = ifelse((wavelength > 1340 & wavelength < 1430) | 
                                (wavelength > 1800 & wavelength < 1950), 
                              NA, reflectance))

summary_d <- d %>%
  group_by(scientificname, wavelength) %>%
  summarize(median = median(reflectance), 
            q25 = quantile(reflectance, .25, na.rm = TRUE), 
            q75 = quantile(reflectance, .75, na.rm = TRUE))

d <- d %>%
  separate(scientificname, c('genus', 'species'), remove = FALSE) %>%
  ungroup %>%
  mutate(common_name = case_when(
    .$scientificname == 'Arctostaphylos viscida' ~ 'Sticky whiteleaf manzanita ', 
    .$scientificname == 'Calocedrus decurrens' ~ 'Incense cedar', 
    .$scientificname == 'Ceanothus cuneatus' ~ 'Buck brush', 
    .$scientificname == 'Ceanothus integerrimus' ~ 'Deer brush', 
    .$scientificname == 'Lupinus albifrons' ~ 'Silver lupine', 
    .$scientificname == 'Pinus ponderosa' ~ 'Ponderosa pine', 
    .$scientificname == 'Quercus chrysolepis' ~ 'Canyon live oak', 
    .$scientificname == 'Quercus kelloggii' ~ 'California black oak', 
    .$scientificname == 'Quercus wislizeni' ~ 'Interior live oak')) %>%
  mutate(facet_label = paste0(scientificname, ' (', common_name, ')')) %>%
  distinct(individualid, .keep_all = TRUE)

# unlabeled 
d %>%
  ggplot(aes(x = wavelength, reflectance, 
             group = indvidualid)) + 
  geom_point(alpha = .1, size = .1, pch = 1) + 
  theme_minimal() + 
  xlab('Wavelength (nm)') + 
  ylab('Reflectance') + 
  theme(legend.position = 'none')
ggsave(filename = 'unlabeled-spectra.png', width = 9, height = 5)

d %>%
  ggplot(aes(x = wavelength, reflectance, group = indvidualid, color = genus)) + 
  geom_point(alpha = .1, size = .1, pch = 1) + 
  theme_minimal() + 
  scale_color_gdocs() +
  xlab('Wavelength (nm)') + 
  ylab('Reflectance') + 
  facet_wrap(~ facet_label, strip.position = 'bottom') + 
  theme(legend.position = 'none')
ggsave(filename = 'species-dict.png', width = 9, height = 5)

summary_d %>%
  ggplot(aes(wavelength, median, color = scientificname)) + 
  theme_minimal() + 
  geom_ribbon(aes(ymin = q25, ymax = q75, fill = scientificname), color = NA, 
              alpha = .5) + 
  geom_line() + 
  scale_color_gdocs() + 
  scale_fill_gdocs()
