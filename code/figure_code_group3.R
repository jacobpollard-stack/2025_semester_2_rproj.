# Load necessary libraries.
library(tidyverse)
library(ggplot2)
library(readxl)
library(FSA)
library(ggstatsplot)

# Import group 3 data.
data_group3 <- read_excel('data/group_3_data.xlsx') |> 
  na.omit()

# Summarise data.
group3_summary <- data_group3 |> 
  group_by(Plate) |> 
  summarise(mean = mean(cfu_count_undiluted),
            sd = sd(cfu_count_undiluted),
            n = length(cfu_count_undiluted),
            se = sd / sqrt(n)
  )
group3_summary

# Check for normality in group 3's data.
# Statistical tests.
## Check for normality in each sheet.
mod_group3 <- lm(cfu_count_undiluted ~ spot, data = data_group3)
### Freedman-Diaconis rule for bin width.
iqr_group3 <- IQR(data_group3$cfu_count_undiluted)
n_group3 <- nrow(data_group3)
bin_width_group3 <- 2 * iqr_group3 / (n_group3^(1/3))
### Plot residuals to visually check for normality.
histo_group3 <- ggplot(mapping = aes(x = mod_group3$residuals), data = data_group3) +
  geom_histogram(binwidth = bin_width_group3)
histo_group3
### Data appears to have a positive skew.
shap_group3 <- shapiro.test(data_group3$cfu_count_undiluted)
shap_group3
### p = 2.81e-07, there is evidence to suggest that the data is not normally distributed.
## Therefore we will perform a non-parametric test, eg. the Kruskal-Wallace test.
krus_group3 <- kruskal.test(cfu_count_undiluted ~ Plate, data = data_group3)
krus_group3
### p =  0.0001219 therefore there is evidence to suggest that, in group 3's data, moisture level and presence of hexadecane impacts growth of P. putida.
## We can perform a post-hoc Dunn test to determine which plates are significantly different
dunn_group3 <- dunnTest(data = data_group3, cfu_count_undiluted ~ Plate, method = 'bonferroni')
dunn_group3

# Plotting the results of the Dunn's test.
## Assigning a colour for each 'contamination' value
Contamination <- ifelse(data_group3$Contamination == 'With hexadecane', 'blue', 'red')
## Make the ggstats plot.
ggstatsplot <- ggbetweenstats(
  data = data_group3,
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
    data = data_group3,
    formula = 'y ~ x',
    aes(group = Contamination,
        colour = Contamination),
    method = 'loess',
    se = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal() +
  scale_colour_discrete(name = "Contamination") +
  scale_colour_manual(values = c('With hexadecane' = 'blue', 
                                 'Without hexadecane' = 'red')
  ) +
  labs(title = "Group 3 CFU count per µL at varying moisture levels, \n with and without hexadecane contamination", 
       x = 'Soil moisture level /% of field capacity', 
       y = 'Mean CFU count /µL') +
  cowplot::theme_cowplot() + theme_custom
combined_plot
