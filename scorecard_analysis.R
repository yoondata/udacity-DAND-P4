

## @knitr prelim

# Load packages
suppressWarnings(library(tidyr))
suppressWarnings(library(dplyr))
suppressWarnings(library(ggplot2))

# Set the theme for plotting
theme_set(theme_bw())
base_theme <- theme(plot.margin = unit(c(20, 20, 20, 20), "points"),
                    panel.margin = unit(c(15, 15, 15, 15), "points"),
                    panel.border = element_blank(),
                    strip.background = element_blank(),
                    strip.text = element_text(size = 15),
                    legend.key = element_blank(),
                    axis.ticks = element_blank())

# Load data
scorecard <- read.csv("./scorecard_clean.csv")
scorecard <- tbl_df(scorecard)

# Set levels for categorical variables
scorecard$control <- factor(scorecard$control, levels = c("private_for-profit", "private_nonprofit", "public"))
scorecard$degree <- factor(scorecard$degree, levels = c("certificate", "associate", "bachelor", "graduate"))

## Derive a new variable "region" from "state"
## Classification adapted from Census Bureau divisions
scorecard$region <- c()
lst_region <- list(
  "Northeast" = c("CT", "MA", "ME", "NH", "NJ", "NY", "PA", "RI", "VT"),
  "Midwest" = c("IA", "IL", "IN", "KS", "MI", "MN", "MO", "ND", "NE", "OH", "SD", "WI"),
  "South" = c("AL", "AR", "DC", "DE", "FL", "GA", "KY", "LA", "MD", "MS", "NC", "OK", "SC", "TN", "TX", "VA", "WV"),
  "West" = c("AZ", "CA", "CO", "ID", "MT", "NM", "NV", "OR", "UT", "WA", "WY"),
  "Noncontiguous" = c("AK", "AL", "AS", "FM", "GU", "HI", "MH", "MP", "PR", "PW", "VI")
)
for(key in names(lst_region)) {
  scorecard$region[scorecard$state %in% lst_region[[key]]] <- key
}
scorecard$region <- factor(scorecard$region, levels = names(lst_region))



## @knitr functions

# Function that produces box plots of the median postgraduate earnings for different levels of a category
box_PGE <- function(scorecard_data, var_cat_1, var_cat_2, view_range = NULL) {
  
  if(missing(var_cat_2)) {
    ggplot(scorecard_data %>%
                     filter(!is.na(earn_P10_median) & !is.na(scorecard_data[[var_cat_1]])), 
                   aes(x = eval(parse(text = var_cat_1)), y = earn_P10_median)) +
      geom_boxplot(alpha = 1/2, outlier.colour = "red", outlier.shape = 1) +
      coord_cartesian(ylim = view_range) +
      labs(x = var_cat_1) +
      base_theme +
      theme(panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank())
  } else {
    ggplot(scorecard_data %>%
                     filter(!is.na(earn_P10_median) & !is.na(scorecard_data[[var_cat_1]]) & !is.na(scorecard_data[[var_cat_2]])), 
                   aes(x = eval(parse(text = var_cat_1)), y = earn_P10_median, fill = eval(parse(text = var_cat_2)))) +
      geom_boxplot(alpha = 1/2, outlier.colour = "red", outlier.shape = 1) +
      coord_cartesian(ylim = view_range) +
      labs(x = var_cat_1, fill = var_cat_2) +
      base_theme +
      theme(panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank())
  }
}

# Function that plots histograms of the median postgraduate earnings for different levels of a category
hist_facet_PGE <- function(scorecard_data, cat_var, view_range = NULL, binwidth = 5000) {
  
  group_mdns <- scorecard_data %>%
    filter(!is.na(scorecard_data[[cat_var]])) %>%
    group_by_(cat_var) %>%
    summarise(mdn_PGE = median(earn_P10_median, na.rm = TRUE))
  
  group_mdns_df <- data.frame(cat_var = levels(scorecard_data[[cat_var]]), mdn_PGE = group_mdns[["mdn_PGE"]])
  
  plot <- ggplot(scorecard_data %>%
                   filter(!is.na(earn_P10_median) & !is.na(scorecard_data[[cat_var]])), 
                 aes(x = earn_P10_median)) +
    geom_histogram(binwidth = binwidth, fill = "navajowhite", col = "white") +
    geom_vline(aes(xintercept = mdn_PGE), data = group_mdns, col = "blue", size = 1) +
    coord_cartesian(xlim = view_range) +
    facet_wrap(as.formula(paste("~", cat_var)), ncol = 1, scales = "free_y") +
    base_theme
  
  return(plot)
}

bar_2cat <- function(scorecard_data, x_cat, fill_cat, position_type = "stack") {
  
  plot <- ggplot(scorecard_data %>%
                   filter(!is.na(scorecard_data[[x_cat]]) & !is.na(scorecard_data[[fill_cat]])), 
                 aes(x = eval(parse(text = x_cat)), fill = eval(parse(text = fill_cat)))) +
    geom_bar(position = position_type, alpha = 4/5) +
    labs(x = x_cat, fill = fill_cat) +
    base_theme +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank())
  
  return(plot)
}

# Make sure that the input categorical variable is set as a factor
bar_2cat_mdn_PGE <- function(scorecard_data, x_cat, fill_cat) {
  
  plot <- ggplot(scorecard_data %>%
                   filter(!is.na(earn_P10_median) & 
                            !is.na(scorecard_data[[x_cat]]) & 
                            !is.na(scorecard_data[[fill_cat]])) %>%
                   group_by_(x_cat, fill_cat) %>%
                   summarise(mdn_PGE = median(earn_P10_median, na.rm = TRUE)), 
                 aes(x = eval(parse(text = x_cat)), y = mdn_PGE, 
                     fill = eval(parse(text = fill_cat)))) +
    geom_bar(stat = "identity", position = "dodge", alpha = 1/2) +
    labs(x = x_cat, fill = fill_cat) +
    base_theme +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank())
  
  return(plot)
}

# Function that produces scatter plots and can color-code different levels of a category
scatter_plot <- function(scorecard_data, var_cont_y, var_cont_x, var_cat, view_range = NULL) {
  
  if(missing(var_cat)) {
    plot <- ggplot(scorecard_data %>%
                     filter(!is.na(scorecard_data[[var_cont_y]]) & 
                            !is.na(scorecard_data[[var_cont_x]])), 
                   aes(y = eval(parse(text = var_cont_y)), 
                       x = eval(parse(text = var_cont_x)))) +
      geom_point(alpha = 1/10, position = position_jitter(h = 0)) +
      coord_cartesian(ylim = view_range) +
      base_theme +
      labs(y = var_cont_y, x = var_cont_x)
  } else {
    plot <- ggplot(scorecard_data %>%
                     filter(!is.na(scorecard_data[[var_cont_y]]) & 
                            !is.na(scorecard_data[[var_cont_x]]) & 
                            !is.na(scorecard_data[[var_cat]])), 
                   aes(y = eval(parse(text = var_cont_y)), 
                       x = eval(parse(text = var_cont_x)), 
                       col = eval(parse(text = var_cat)))) +
      geom_point(alpha = 1/10, position = position_jitter(h = 0)) +
      coord_cartesian(ylim = view_range) +
      base_theme +
      labs(y = var_cont_y, x = var_cont_x, col = var_cat)
  }
  
  return(plot)
}

univariate_cont <- function(scorecard_data, var_cont, binwidth = 5000) {
  
  # Plot box plot
  p1 <- ggplot(scorecard_data %>%
                 filter(!is.na(scorecard_data[[var_cont]])), 
               aes(x = 1, y = eval(parse(text = var_cont)))) +
    geom_boxplot(outlier.colour = "red", outlier.shape = 1) +
    labs(y = var_cont) +
    base_theme +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          axis.title.x = element_blank(),
          axis.text.x = element_blank())
  
  # Plot histogram
  p2 <- ggplot(scorecard_data %>%
                 filter(!is.na(scorecard_data[[var_cont]])), 
               aes(x = eval(parse(text = var_cont)))) +
    geom_histogram(binwidth = binwidth, col = "white", fill = "lightsalmon") +
    labs(x = var_cont) +
    base_theme
  
  # Save plots as a list and return the result
  plot_list <- list("box" = p1, "hist" = p2)
  return(plot_list)
}



## @knitr PGE_stat

# Get a brief statistical summary
apply(scorecard[,c("earn_P10_mean", "earn_P10_median")], 2, summary)

# Distribution of Postgraduate Earnings: Mean vs. Median - Box Plot
ggplot(scorecard %>%
         filter(!is.na(earn_P10_median) & !is.na(earn_P10_mean)) %>%
         gather(earn_P10_stat, earn_P10_amount, earn_P10_median:earn_P10_mean), 
       aes(x = earn_P10_stat, y = earn_P10_amount)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 1) +
  base_theme +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

# Distribution of Postgraduate Earnings: Mean vs. Median - Frequency Polygon
ggplot(scorecard %>%
         filter(!is.na(earn_P10_median) & !is.na(earn_P10_mean)) %>%
         gather(earn_P10_stat, earn_P10_amount, earn_P10_median:earn_P10_mean), 
       aes(x = earn_P10_amount, col = earn_P10_stat)) +
  geom_freqpoly(binwidth = 5000) +
  scale_x_continuous(breaks = seq(0, 250000, 50000)) +
  base_theme

# Mean vs. Median Postgraduate Earnings
ggplot(scorecard %>%
         filter(!is.na(earn_P10_median) & !is.na(earn_P10_mean)),
       aes(x = earn_P10_mean, y = earn_P10_median)) +
  geom_point(alpha = 1/10, position = position_jitter(h = 0)) +
  coord_cartesian(xlim = c(0, 250000), ylim = c(0, 250000)) +
  geom_abline(slope = 1, colour = "red") +
  base_theme



## @knitr outliers

# Subset outliers
grad_earn_GT100K <- scorecard %>%
  filter(earn_P10_median > 100000) %>%
  arrange(desc(earn_P10_median))

# Print some of their info
grad_earn_GT100K %>%
  select(inst_name, earn_P10_median, control, degree, region) %>%
  print(n = nrow(grad_earn_GT100K))



## @knitr control

# Distribution of Different Control Types
ggplot(scorecard, aes(x = control)) +
  geom_bar(col = "black", fill = "white") +
  base_theme +
  theme(panel.grid = element_blank())



## @knitr PGE_control

# Distribution of Postgraduate Earnings by Control Type: Box Plot
box_PGE(scorecard, "control")

# Distribution of Postgraduate Earnings by Control Type: Histogram
hist_facet_PGE(scorecard, "control", view_range = c(0, 100000))



## @knitr degree

# Distribution of Primary Degree Types
ggplot(scorecard, aes(x = degree)) +
  geom_bar(col = "black", fill = "white") +
  base_theme +
  theme(panel.grid = element_blank())



## @knitr PGE_degree

# Distribution of Postgraduate Earnings by Degree Type: Box Plot
box_PGE(scorecard, "degree")

# Distribution of Postgraduate Earnings by Degree Type: Histogram
hist_facet_PGE(scorecard, "degree", view_range = c(0, 100000))



## @knitr region

# Regional Distribution
ggplot(scorecard, aes(x = region)) +
  geom_bar(col = "black", fill = "white") +
  base_theme +
  theme(panel.grid = element_blank())



## @knitr PGE_region

# Distribution of Postgraduate Earnings by Region: Box Plot
box_PGE(scorecard, "region")

# Distribution of Postgraduate Earnings by Region: Histogram
hist_facet_PGE(scorecard, "region", view_range = c(0, 100000))



## @knitr distribution_degree

# Distribution of Degree Types by Control Type
bar_2cat(filter(scorecard, !is.na(earn_P10_median)), "control", "degree", "fill") +
  labs(y = "proportion") +
  theme(panel.grid = element_blank())

# Distribution of Degree Types by Region
bar_2cat(filter(scorecard, !is.na(earn_P10_median)), "region", "degree", "fill") +
  labs(y = "proportion") +
  theme(panel.grid = element_blank())



## @knitr PGE_control_adjDegree

# Distribution of Postgraduate Earnings by Control Type, Adjusting for Degree Type
box_PGE(scorecard, "degree", "control")

# Median Postgraduate Earnings by Control Type, Adjusting for Degree Type
bar_2cat_mdn_PGE(scorecard, "degree", "control")



## @knitr PGE_region_adjDegree

# Distribution of Postgraduate Earnings by Region, Adjusting for Degree Type
box_PGE(scorecard, "degree", "region")

# Median Postgraduate Earnings by Region, Adjusting for Degree Type
bar_2cat_mdn_PGE(scorecard, "degree", "region")



## @knitr PGE_degree_adjControl

# Distribution of Postgraduate Earnings by Degree Type, Adjusting for Control Type
box_PGE(scorecard, "control", "degree")

# Median Postgraduate Earnings by Degree Type, Adjusting for Control Type
bar_2cat_mdn_PGE(scorecard, "control", "degree")



## @knitr PGE_degree_adjRegion

# Distribution of Postgraduate Earnings by Degree Type, Adjusting for Region
box_PGE(scorecard, "region", "degree")

# Median Postgraduate Earnings by Degree Type, Adjusting for Region
bar_2cat_mdn_PGE(scorecard, "region", "degree")



## @knitr medical

# Create a new variable for medical specialization based on institution names
scorecard <- scorecard %>%
  mutate(focus_medicine = ifelse(grepl("Medic", inst_name) | grepl("Pharm", inst_name) | grepl("Health", inst_name), "yes", "no"))

# Make the variable a factor
scorecard$focus_medicine <- factor(scorecard$focus_medicine, levels = c("no", "yes"))

# Distribution of medical vs. non-medical institutions
ggplot(scorecard, aes(x = focus_medicine)) +
  geom_bar(col = "black", fill = "white") +
  base_theme +
  theme(panel.grid = element_blank())



## @knitr PGE_medical

# Distribution of Postgraduate Earnings by Specialization in Medicine: Box Plot
box_PGE(scorecard, "focus_medicine")

# Distribution of Postgraduate Earnings by Specialization in Medicine: Histogram
hist_facet_PGE(scorecard, "focus_medicine", view_range = c(0, 100000))



## @knitr PGE_medical_adjDegree

# Distribution of Postgraduate Earnings by Specialization in Medicine, Adjusting for Degree Type
box_PGE(scorecard, "degree", "focus_medicine")



## @knitr PGE_gender

# Distribution of Postgraduate Earnings: Male vs. Female - Box Plot
ggplot(scorecard %>%
         filter(!is.na(earn_P10_mean_male) & !is.na(earn_P10_mean_female)) %>%
         gather(earn_P10_mean_gender, earn_P10_mean_amount, earn_P10_mean_male, earn_P10_mean_female), 
       aes(x = earn_P10_mean_gender, y = earn_P10_mean_amount)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 1) +
  base_theme +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

# Distribution of Postgraduate Earnings: Male vs. Female - Frequency Polygon
ggplot(scorecard %>%
         filter(!is.na(earn_P10_mean_male) & !is.na(earn_P10_mean_female)) %>%
         gather(earn_P10_mean_gender, earn_P10_mean_amount, earn_P10_mean_male, earn_P10_mean_female), 
       aes(x = earn_P10_mean_amount, col = earn_P10_mean_gender)) +
  geom_freqpoly(binwidth = 5000) +
  scale_x_continuous(breaks = seq(0, 250000, 50000)) +
  base_theme

# Male vs. Female Postgraduate Earnings
ggplot(scorecard %>%
         filter(!is.na(earn_P10_mean_male) & !is.na(earn_P10_mean_female)), 
       aes(x = earn_P10_mean_male, y = earn_P10_mean_female)) +
  geom_point(alpha = 1/10, position = position_jitter(h = 0)) +
  geom_abline(slope = 1, colour = "red") +
  base_theme



## @knitr PGE_gender_ttest

scorecard %>%
  filter(!is.na(earn_P10_mean_male) & !is.na(earn_P10_mean_female)) %>%
  mutate(earn_P10_mean_genderDiff = earn_P10_mean_male - earn_P10_mean_female) %>%
  select(earn_P10_mean_male, earn_P10_mean_female, earn_P10_mean_genderDiff) %>%
  summary()

ggplot(scorecard %>%
         filter(!is.na(earn_P10_mean_male) & !is.na(earn_P10_mean_female)) %>%
         mutate(earn_P10_mean_genderDiff = earn_P10_mean_male - earn_P10_mean_female), 
       aes(x = earn_P10_mean_genderDiff)) +
  geom_histogram(binwidth = 1000, col = "white", fill = "lightsalmon") +
  base_theme

t.test(scorecard$earn_P10_mean_male,
       scorecard$earn_P10_mean_female,
       conf.level = 0.95,
       paired = TRUE)



## @knitr PGE_male

# Derive a new variable for male proportion
scorecard <- scorecard %>%
  mutate(male_prop = 1 - female_prop)  # Assume comparatively little presence of LGBTQ students

# Postgraduate Earnings vs. Male Proportion
scatter_plot(scorecard, "earn_P10_median", "male_prop")



## @knitr PGE_male_degree

# Postgraduate Earnings vs. Male Proportion by Degree Type
scatter_plot(scorecard, "earn_P10_median", "male_prop", "degree")



## @knitr PGE_male_degree_zoomed

# Postgraduate Earnings vs. Male Proportion by Degree Type: Zoomed
scatter_plot(scorecard, "earn_P10_median", "male_prop", "degree", view_range = c(0, 75000))



## @knitr male_degree

# Male Proportion by Degree Type
ggplot(scorecard %>%
         filter(!is.na(degree) & !is.na(male_prop)),
       aes(x = male_prop, y = degree)) +
  geom_jitter(alpha = 1/5) +
  base_theme +
  theme(panel.grid = element_blank())



## @knitr PGE_male_adjDegree

# Postgraduate Earnings vs. Male Proportion, Adjusting for Degree Type
scatter_plot(scorecard %>%
               filter(!is.na(degree)), 
             "earn_P10_median", "male_prop") +
  geom_smooth(method = "lm") +
  facet_wrap(~ degree, scales = "free_y")



## @knitr PGE_race

# Postgraduate Earnings vs. Racial Proportion
scatter_plot(scorecard %>%
               filter(degree == "bachelor") %>%
               gather(UGDS_race, UGDS_prop, UGDS_white:UGDS_NA), 
             "earn_P10_median", "UGDS_prop") +
  facet_wrap(~ UGDS_race, scales = "free_x")



## @knitr famIncm

# Distribution of Average Family Income
var_tmp <- univariate_cont(scorecard, "fam_income_median", binwidth = 2500)
var_tmp[["box"]]; var_tmp[["hist"]]



## @knitr PGE_famIncm

# Postgraduate Earnings vs. Family Income
scatter_plot(scorecard, "earn_P10_median", "fam_income_median")



## @knitr PGE_famIncm_adjDegree

# Postgraduate Earnings vs. Family Income by Degree Type
scatter_plot(scorecard, "earn_P10_median", "fam_income_median", "degree")

# Postgraduate Earnings vs. Family Income, Adjusting for Degree Type
scatter_plot(scorecard %>%
               filter(!is.na(degree)), 
             "earn_P10_median", "fam_income_median") +
  facet_wrap(~ degree, scales = "free")



## @knitr cost

# Distribution of Cost of Attendance
var_tmp <- univariate_cont(scorecard, "avg_net_cost", binwidth = 2500)
var_tmp[["box"]]; var_tmp[["hist"]]



## @knitr PGE_cost

# Postgraduate Earnings vs. Cost of Attendance
scatter_plot(scorecard, "earn_P10_median", "avg_net_cost")



## @knitr PGE_cost_degree

# Postgraduate Earnings vs. Cost of Attendance by Degree Type
scatter_plot(scorecard, "earn_P10_median", "avg_net_cost", "degree")



## @knitr PGE_cost_adjDegree

# Postgraduate Earnings vs. Cost of Attendance, Adjusting for Degree Type
scatter_plot(scorecard %>%
               filter(!is.na(degree)), 
             "earn_P10_median", "avg_net_cost") +
  geom_smooth(method = "lm") +
  facet_wrap(~ degree, scales = "free", nrow = 2)



## @knitr PGE_cost_anomaly

# Print some info of unusual data points
scorecard %>%
  filter(avg_net_cost < 0) %>%
  select(inst_name, control, degree, earn_P10_median, avg_net_cost) %>%
  arrange(desc(earn_P10_median))



## @knitr debt

# Distribution of Student Debt
var_tmp <- univariate_cont(scorecard, "grad_debt_median", binwidth = 2500)
var_tmp[["box"]]; var_tmp[["hist"]]



## @knitr PGE_debt

# Postgraduate Earnings vs. Student Debt
scatter_plot(scorecard, "earn_P10_median", "grad_debt_median")



## @knitr PGE_debt_degree

# Postgraduate Earnings vs. Student Debt by Degree Type
scatter_plot(scorecard, "earn_P10_median", "grad_debt_median", "degree")



## @knitr PGE_debt_adjDegree

# Postgraduate Earnings vs. Student Debt, Adjusting for Degree Type
scatter_plot(scorecard %>%
               filter(!is.na(degree)), 
             "earn_P10_median", "grad_debt_median") +
  geom_smooth(method = "lm") +
  facet_wrap(~ degree, scales = "free")



## @knitr finalPlot1

box_PGE(scorecard, "degree", view_range = c(0, 150000)) +
  labs(x = "Primary Degree Type", y = "Median Income, 10 Years After Graduation [USD]",
       title = "Postgraduate Financial Success by Institution's Primary Degree Type")



## @knitr finalPlot2

# hist_density_PGE(scorecard, "control", view_range = c(0, 100000)) +
#   labs(x = "Median Income, 10 Years After Graduation", y = "Proportion",
#        title = "Postgraduate Financial Success by Institution's Control Type") +
#   theme(axis.text.x = element_text(size = 8))
# 
# hist_density_PGE(filter(scorecard, !is.na(degree)), "control", view_range = c(0, 100000)) +
#   facet_wrap(~ degree, ncol = 1) +
#   labs(x = "Median Income, 10 Years After Graduation", y = "Proportion",
#        title = "Adjusting for Primary Degree Type") +
#   theme(axis.text.x = element_text(size = 8),
#         strip.text = element_text(color = "dimgrey"))



## @knitr finalPlot3

# scatter_plot(scorecard, "earn_P10_median", "male_prop") +
#   labs(x = "Male Proportion", y = "Median Income, 10 Years After Graduation",
#        title = "Postgraduate Earnings vs. Male Proportion")
# 
# scatter_plot(scorecard %>%
#                filter(!is.na(degree)), 
#              "earn_P10_median", "male_prop") +
#   facet_wrap(~ degree, scales = "free_y") +
#   labs(x = "Male Proportion", y = "Median Income, 10 Years After Graduation",
#        title = "Adjusting for Primary Degree Type") +
#   theme(axis.text.x = element_text(size = 8),
#         axis.text.y = element_text(size = 8),
#         strip.text = element_text(color = "dimgrey"))


