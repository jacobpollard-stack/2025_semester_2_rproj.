# Load necessary libraries.
library(tidyverse)
library(ggplot2)
library(readxl)
library(dlpyr)

# Read in the data sheetwise and omit missing data.
## Define the file path.
path <- 'data/data_tidy_sheeted.xlsx'
## Load the sheet names into a variable.
sheets <- excel_sheets(path)
## Read the sheets and compile them into one dataframe.
data_tidy_list <- lapply(sheets, function(sheet) {
  data <- read_excel(path, sheet = sheet)
  data$group <- sheet
  return(data)
})
## Combine all the sheets into one dataframe.
data_tidy <- do.call(rbind, data_tidy_list) |> 
  na.omit()

# Summarise data.
data_summary <- data_tidy |>
  group_by(plate) |> 
  summarise(mean = mean(cfu_count_undiluted),
            sd = sd(cfu_count_undiluted),
            n = length(plate),
            se = sd/sqrt(n)
            )

# Statistical tests
## Check for normality
mod <- lm(cfu_count_undiluted ~ group, data = data_tidy)
### Freedman-Diaconis rule for bin width.
ggplot(mapping = aes(x = mod$residuals)) +
  geom_histogram(bins = 15)
### Data appears to have a positive skew.
shapiro.test(data_tidy$cfu_count_undiluted)
### p = 3.579e-05 < 0.05, there is evidence to suggest that the data is not normally distributed.
## Therefore we will perform a non-parametric test, eg. the Kruskal-Wallace test.
kruskal.test(cfu_count_undiluted ~ plate, data = data_tidy)
### p = 0.7832 > there is no evidence to suggest that the data is significantly different between plates.

# However, this is using the data of the mean of each CFU count between groups; we can instead analyse each group individually.
