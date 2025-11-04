file_path <- "Nest Monitoring Data Solar Noon.csv"

coverage_data <- read.csv(file_path)

#Check data and obs #'s

str(coverage_data)

#Make values into vectors
covered_values <- coverage_data$Hatch.success[coverage_data$Group == "covered"]

uncovered_values <- coverage_data$Hatch.success[coverage_data$Group == "uncovered"]

#Wilcox test w continuity correction
test <-wilcox.test(covered_values, uncovered_values, correct = TRUE, EXACT = FALSE, alternative = "two.sided" )

#See results
print(test)

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

#Effect size code work (incomplete)
#"coin" package?

coverage_data$Group <- as.factor(coverage_data$Group)

#Group is a factor so it doesn't work, needs to compare the numerical groups
coin::wilcox_test(Hatch.success ~ Group, data = coverage_data, distribution = "exact")

#Calculate r

2.1772/sqrt(length(covered_values)+length(uncovered_values)) 

citation()

citation(package = "ggplot2")


