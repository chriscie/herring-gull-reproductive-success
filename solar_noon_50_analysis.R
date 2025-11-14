#Analysis between covered and uncovered nests using Mann-Whitney U test (Wilcoxon ranked sum) and the normal approximation given large n>100 samples and ties.

#Retrieve csv file of hatch success data
file_path <- "hatch_success_solar_noon.csv"

coverage_data <- read.csv(file_path)

#Check data and obs #'s

str(coverage_data)
length(covered_values)
length(uncovered_values)

#Make values into vectors
covered_values <- coverage_data$hatch_success[coverage_data$group == "covered"]

uncovered_values <- coverage_data$hatch_success[coverage_data$group == "uncovered"]

#Wilcox test (standard)
test <-wilcox.test(covered_values, uncovered_values, correct = FALSE, EXACT = FALSE, alternative = "two.sided" )

#See results
print(test)

#Combine vectors and view ties
all_values <- c(covered_values, uncovered_values)
tie_counts <- table(all_values)

#Tie count print
tie_counts

#Analysis using coin package to retrieve Z for effect size

library(coin)

# Combine data into data frame
data <- data.frame(
  value = c(covered_values, uncovered_values),
  group = factor(rep(c("covered", "uncovered"), 
                     times = c(length(covered_values), length(uncovered_values))))
)

# Wilcoxon rank-sum test (asymptotic normal approximation)
test.z <- wilcox_test(value ~ group, data = data, distribution = "asymptotic")

# Print full test result
test.z

# Extract the standardized Z statistic
z_value <- statistic(test.z, type = "standardized")
z_value

#Calculate r (effect size) by formula 

2.1772/sqrt(length(covered_values)+length(uncovered_values)) 

z_value/sqrt(length(covered_values)+length(uncovered_values))

library(coin)

# new variable name for the data frame to avoid conflicts
perm_data <- data.frame(
  value = c(covered_values, uncovered_values),
  group = factor(rep(c("covered", "uncovered"), 
                     times = c(length(covered_values), length(uncovered_values))))
)

# Wilcoxon rank-sum test using Monte Carlo simulation for P-value ("pure" permutation)
test.perm <- wilcox_test(value ~ group, data = perm_data, distribution = approximate(nresample = 10000)) 

# Use a large number of permutations for stability

# See results
test.perm

# Get Z statistic from coin object
Z_value_perm <- statistic(test.perm, type = "standardized")

# Total sample size
N_total <- nrow(perm_data)

# Effect size r
r_effect <- Z_value_perm / sqrt(N_total)

r_effect

#Graph showing hatch success of covered and uncovered nests
#load graph package
library(ggplot2)

#Function for CI interval
calc_ci <- function(x) {
  n <- length(x)
  mean_x <- mean(x)
  se_x <- sd(x) / sqrt(n)
  t_crit <- qt(0.975, df = n - 1)
  lower <- mean_x - t_crit * se_x
  upper <- mean_x + t_crit * se_x
  list(mean = mean_x, lower = lower, upper = upper)
}

ci_covered <- calc_ci(covered_values)
ci_uncovered <- calc_ci(uncovered_values)

# Print stats for verification
cat("Covered:\n")
cat("Mean:", round(ci_covered$mean, 4), "\n")
cat("95% CI: [", round(ci_covered$lower, 4), ",", round(ci_covered$upper, 4), "]\n\n")

cat("Uncovered:\n")
cat("Mean:", round(ci_uncovered$mean, 4), "\n")
cat("95% CI: [", round(ci_uncovered$lower, 4), ",", round(ci_uncovered$upper, 4), "]\n\n")

overall_mean <- mean(c(covered_values, uncovered_values))

# Point graph with confidence intervals

plot_data <- data.frame(
  group = factor(c("Covered", "Uncovered"), levels = c("Covered", "Uncovered")),
  mean = c(ci_covered$mean, ci_uncovered$mean),
  lower = c(ci_covered$lower, ci_uncovered$lower),
  upper = c(ci_covered$upper, ci_uncovered$upper)
)

ggplot(plot_data, aes(x = group, y = mean)) +
  geom_point(size = 3, color = "black") +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1, color = "black", size = .8) +
  geom_hline(yintercept = overall_mean, linetype = "dashed", color = "black") +
  geom_label(
    aes(x = 2.6, y = 1.05),
    label = paste0("Overall hatch success:\n", round(overall_mean, 3)),
    fill = "white", color = "black", fontface = "bold", size = 4, hjust = 1
  ) +
  ylim(0, 1.1) +
  labs(
    title = NULL,
    x = NULL,
    y = "Hatch Success (proportion)"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
        axis.title.y = element_text(size = 14),   axis.text.x  = element_text(size = 12),  
        axis.text.y  = element_text(size = 14),  panel.grid.major = element_line(size = .8),
        panel.grid.minor = element_line(size = .8))

#Source citation for manuscript
citation()

citation(package = "ggplot2")

citation(package = "coin")


