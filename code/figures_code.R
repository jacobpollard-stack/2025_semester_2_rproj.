# Load necessary libraries.
library(tidyverse)
library(ggplot2)
library(readxl)
library(FSA)
library(ggstatsplot)

# Read in the data sheetwise and omit missing data.
library(readxl)

# Read in the data sheetwise and omit missing data.
## Define a variable as the path
path <- 'data/data_tidy_sheeted.xlsx'
# Load the sheet names into a variable.
data_allgroups <- excel_sheets(path)
# Read the sheets and compile them into one dataframe.
data_tidy_list <- lapply(data_allgroups, function(sheet) {
  data <- read_excel(path, sheet = sheet)  # Fix: use 'path' instead of 'data_allgroups'
  data$group <- sheet
  return(data)
})
# Combine into one dataframe
data_tidy <- do.call(rbind, data_tidy_list)

# Summarise data.
data_summary <- data_tidy |>
  group_by(plate) |> 
  summarise(mean = mean(cfu_count_undiluted),
            sd = sd(cfu_count_undiluted),
            n = length(plate),
            se = sd / sqrt(n)
            )

# Statistical tests.
## Check for normality in each sheet.
mod <- lm(cfu_count_undiluted ~ group, data = data_tidy)
### Freedman-Diaconis rule for bin width.
iqr <- IQR(data_tidy$cfu_count_undiluted)
n <- nrow(data_tidy)
bin_width <- 2 * iqr / (n^(1/3))
### Plot residuals to visually check for normality.
histo <- ggplot(mapping = aes(x = mod$residuals),
       data = data_tidy
       ) +
  geom_histogram(binwidth = bin_width)
histo
### Data appears to have a positive skew.
shap <- shapiro.test(data_tidy$cfu_count_undiluted)
shap
### p = 1.743e-8 < 0.05, there is evidence to suggest that the data is not normally distributed.
## Therefore we will perform a non-parametric test, eg. the Kruskal-Wallace test.
krus <- kruskal.test(cfu_count_undiluted ~ plate, data = data_tidy)
print(krus)
### p = 0.7391 > 0.05 therefore there is no evidence to suggest that moisture level nor presence of hexadecane impacts growth of P. putida.

# Visualise data.
mean_CFU_count <- ggplot() +
  geom_point(data = data_tidy,
             mapping = aes(x = plate, 
                           y = cfu_count_undiluted, 
                           colour = group)
             ) +
  geom_point(data = data_summary, 
             mapping = aes(x = plate, 
                           y = mean), 
             colour = 'black', 
             size = 1
             ) +
  geom_errorbar(data = data_summary, 
                mapping = aes(x = plate, 
                              ymin = mean - se, 
                              ymax = mean + se), 
                width = 0.2, 
                colour = 'black'
                ) +
  labs(title = 'CFU count per plate by group and mean with error bars',
       x = 'Plate',
       y = 'CFU Count /µL',
       colour = 'Group'
       ) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal()
mean_CFU_count
### The standard deviations are very high for each group, suggesting that the insignificance is due largely to variation between groups.

# However, as the standard deviations are very high between groups, we can perform tests on each group individually.
## Data for 4 x 5microL spots was only collected for group 1.

# Import group 1 data.
data_group1 <- read_excel('data/group_1_data.xlsx') |> 
  na.omit()

# Summarise data.
group1_summary <- data_group1 |> 
  group_by(Plate) |> 
  summarise(mean = mean(cfu_count_undiluted),
            sd = sd(cfu_count_undiluted),
            n = length(cfu_count_undiluted),
            se = sd / sqrt(n)
            )
group1_summary

# Check for normality in group 1's data.
# Statistical tests.
## Check for normality in each sheet.
mod_group1 <- lm(cfu_count_undiluted ~ spot, data = data_group1)
### Freedman-Diaconis rule for bin width.
iqr_group1 <- IQR(data_group1$cfu_count_undiluted)
n_group1 <- nrow(data_group1)
bin_width_group1 <- 2 * iqr_group1 / (n_group1^(1/3))
### Plot residuals to visually check for normality.
histo_group1 <- ggplot(mapping = aes(x = mod_group1$residuals), data = data_group1) +
  geom_histogram(binwidth = bin_width_group1)
histo_group1
### Data appears to have a positive skew.
shap_group1 <- shapiro.test(data_group1$cfu_count_undiluted)
shap_group1
### p = 1.465e-8 < 0.05, there is evidence to suggest that the data is not normally distributed.
## Therefore we will perform a non-parametric test, eg. the Kruskal-Wallace test.
krus_group1 <- kruskal.test(cfu_count_undiluted ~ Plate, data = data_group1)
krus_group1
### p = 9.638e-5 < 0.05 therefore there is evidence to suggest that, in group 1's data, moisture level and presence of hexadecane impacts growth of P. putida.
## We can perform a post-hoc Dunn test to determine which plates are significantly different
dunn_group1 <- dunnTest(data = data_group1, cfu_count_undiluted ~ Plate, method = 'bonferroni')
dunn_group1

# Plotting the results of the Dunn's test.
## Assigning a colour for each 'contamination' value
Contamination <- ifelse(data_group1$Contamination == 'With hexadecane', 'blue', 'red')
## Make the ggstats plot.
ggstatsplot <- ggbetweenstats(
  data = data_group1,
  x = 'Plate', 
  y = 'cfu_count_undiluted',
  type = 'nonparametric',
  comparison.group = 'all',
  point.args = list(size = 1.5, 
                    alpha = 1, 
                    colour = Contamination),
  centrality.point.args = list(alpha = 0),
  centrality.label.args = list(size = 3, 
                               nudge_x = 0.5, 
                               segment.linetype = 3,
                               min.segment.length = 0),
  ggsignif.args = list(textsize = 3, 
                       tip_length = 0.01,
                       na.rm = TRUE),
)
ggstatsplot

## Designing a custom theme for the plot.
theme_custom <- theme(
  legend.position = 'top',
  legend.title = element_blank(),
    panel.grid.major.y = element_line(colour = "#e3e1e1",
                                      linetype = 1),
    plot.title = element_text(hjust = 0,
                              vjust = -5),
    plot.margin = margin(10, 10, 40, 10),
    plot.subtitle = element_text(vjust = -250,
                                 hjust = 1)
)

## Overlaying correlation onto our ggstatsbetween plot.
combined_plot <- ggstatsplot + 
  geom_smooth(
    data = data_group1,
    formula = 'y ~ x',
    aes(group = Contamination,
        colour = Contamination),
    method = 'lm',
    se = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal() +
  scale_colour_discrete(name = "Contamination") +
  scale_colour_manual(values = c('With hexadecane' = 'blue', 
                                 'Without hexadecane' = 'red')
  ) +
  labs(title = "Group 1 CFU count per µL at varying moisture levels, \n with and without hexadecane contamination", 
       x = 'Soil moisture level /% of field capacity', 
       y = 'Mean CFU count /µL') +
  cowplot::theme_cowplot() + theme_custom
combined_plot
