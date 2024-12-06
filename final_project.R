library(tidyverse)
library(dplyr)
library(ggplot2)
library(readr)
library(readxl)
library(patchwork)
library(modelr)
library(splines)
library(AICcmodavg)
library(MASS)
library(embed)
library(ranger)
library(kernlab)
library(lubridate)
library(rpart.plot)
library(modeldata)
library(tidymodels)
tidymodels_prefer(quiet = TRUE)

spp_counts <- read_excel("DSB_DATA.xlsx", sheet = "Spp_Counts")
carp_buff <- read_excel("DSB_DATA.xlsx", sheet = "Carp&BmBuff")

# Perform Z-test for two proportions for each species
results <- spp_counts %>%
  group_by(Species) %>%
  summarize(
    z_test = list(
      {
        # proportions for IDEM and BF for each species
        prop_idem <- Proportion[Source == "IDEM"]
        prop_bf <- Proportion[Source == "BF"]
        
        # counts for IDEM and BF for each species
        count_idem <- Count[Source == "IDEM"]
        count_bf <- Count[Source == "BF"]
          
          # Calculate the pooled proportion
          total_count <- sum(count_idem) + sum(count_bf)
          pooled_p <- (sum(count_idem) * prop_idem + sum(count_bf) * prop_bf) / total_count
          
          # Calculate the Z-statistic
          z_statistic <- (prop_idem - prop_bf) / sqrt(pooled_p * (1 - pooled_p) * 
                                                        (1 / sum(count_idem) + 1 / sum(count_bf)))
          
          # Calculate the p-value from the Z-statistic (two-tailed)
          p_value <- 2 * (1 - pnorm(abs(z_statistic)))
          
          # Return results as a list
          list(z_statistic = z_statistic, p_value = p_value)
        } 
    ),
    .groups = "drop"
  ) %>%
  mutate(
    z_statistic = sapply(z_test, function(x) if (!is.null(x)) x$z_statistic),
    p_value = sapply(z_test, function(x) if (!is.null(x)) x$p_value)
  ) %>%
  select(Species, z_statistic, p_value)

# Print results
print(results)

cbPalette <- c("#009E73", "#CC79A7")

ggplot(spp_counts, aes(x = Species, y = Proportion, fill = Source)) +
  geom_bar(stat = "identity", position = "dodge") +  # Create the bar graph
  labs(x = "Species", y = "Proportion", title = "Proportions of Species from Different Sources") +
  scale_fill_manual(values = cbPalette) +  # Customize colors for sources
  theme_bw()

ggplot(carp_buff, aes(x = Harvest_Status, y = Length, fill = Harvest_Status)) +
  geom_boxplot() +
  facet_wrap(~Species) +
  labs(title = "Length Distribution by Harvest Status for Bigmouth Buffalo and Common Carp",
       x = "Harvest Status",
       y = "Length (cm)") +
  scale_fill_manual(values=cbPalette) +
  theme_bw()

# Alternatively, create histograms to visualize the distribution of lengths
ggplot(carp_buff, aes(x = Length, fill = Harvest_Status)) +
  geom_histogram(position = "dodge", bins = 30, alpha = 0.7) +
  facet_wrap(~Species) +
  labs(title = "Length Distribution by Harvest Status for Bigmouth Buffalo and Common Carp",
       x = "Length (cm)",
       y = "Count") +
  scale_fill_manual(values=cbPalette) +
  theme_bw()

# Statistical tests to compare length distributions between harvest statuses
# Perform t-tests for both species
bigmouth_buffalo_data <- carp_buff %>% filter(Species == "BigmouthBuffalo")
common_carp_data <- carp_buff %>% filter(Species == "CommonCarp")

# t-test for BigmouthBuffalo
t_test_bmb <- t.test(Length ~ Harvest_Status, data = bigmouth_buffalo_data)
print(t_test_bmb)

# t-test for CommonCarp
t_test_cc <- t.test(Length ~ Harvest_Status, data = common_carp_data)
print(t_test_cc)
