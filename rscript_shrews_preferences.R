# =====================================================================
# Title: Consumption of lead-contaminated prey by shrews differing in metabolic rate (Sorex araneus and Crocidura gueldenstaedtii)
# Description: Linear Mixed-Effects Models (LMM), Kaplan-Meier Survival 
#              Analysis, and Custom ggplot2 Visualizations for Shrews
# Author: [Bogna Malinowska]
# =====================================================================

# 1. Load Required Libraries
library(lme4)        # For linear mixed-effects models (LMM)
library(lmerTest)    # For obtaining p-values in LMM
library(emmeans)     # For estimated marginal means and pairwise comparisons
library(ggplot2)     # For advanced data visualization
library(dplyr)       # For data wrangling, filtering, and summarization
library(cowplot)     # For combining multi-panel plots and extracting legends
library(survival)    # For survival analysis structures
library(survminer)   # For Kaplan-Meier survival curves visualization

# =====================================================================
# SECTION 2: Sorex araneus (SA) - Behavioral & Mass Analysis
# =====================================================================

# Filter data for Sorex araneus (SA)
SA <- filter(data, Species == "SA")

# Linear Mixed-Effects Model for Preferences
modelP_SA <- lmer(
  Preferences ~ Type * Infected + Rogers + (1 | Animal),
  data = SA
)
summary(modelP_SA)
emm2_SA <- emmeans(modelP_SA, ~ Type * Infected)
pairs(emm2_SA)

# Linear Mixed-Effects Model for Body Mass (mg)
modelM_SA <- lmer(
  `Mass (mg)` ~ Type * Infected + Rogers + (1 | Animal),
  data = SA
)
summary(modelM_SA)
emm_SA <- emmeans(modelM_SA, ~ Type * Infected)
pairs(emm_SA)

# --- Visualization: Preferences (SA) ---
plot_preferences_SA <- ggplot(as.data.frame(emm2_SA), 
                              aes(x = Infected, y = emmean, colour = Type, group = Type)) +
  geom_point(size = 3) +
  geom_line(linewidth = 1) +
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE), width = 0.1) +
  labs(title = "", x = "", y = expression("Preferences (" * italic(N) * ")"), colour = "Food") +
  scale_colour_manual(labels = c("M" = "Mealworms", "E" = "Earthworms"),
                      values = c("burlywood4", "#FF1493")) +
  scale_x_discrete(labels = c("YES" = "Contaminated", "NO" = "Uncontaminated")) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "italic"),
    axis.title.x = element_text(size = 12, face = "plain"),
    axis.title.y = element_text(size = 12, face = "plain"),
    axis.text = element_text(size = 11, colour = "black"),
    legend.position = "none",
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.8),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks = element_line(colour = "black", linewidth = 0.5),
    axis.ticks.length = unit(-0.15, "cm"),
    axis.line = element_line(colour = "black", linewidth = 0.6)
  ) +
  scale_y_continuous(labels = function(x) sub("\\.0$", "", x))

# --- Visualization: Mass (SA) ---
plot_mass_SA <- ggplot(as.data.frame(emm_SA), 
                       aes(x = Infected, y = emmean, colour = Type, group = Type)) +
  geom_point(size = 3) +
  geom_line(linewidth = 1) +
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE), width = 0.1) +
  labs(title = "", x = "", y = "Mass (mg)", colour = "Food") +
  scale_colour_manual(labels = c("M" = "Mealworms", "E" = "Earthworms"),
                      values = c("burlywood4", "#FF1493")) +
  scale_x_discrete(labels = c("YES" = "Contaminated", "NO" = "Uncontaminated")) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "italic"),
    axis.title.x = element_text(size = 12, face = "plain"),
    axis.title.y = element_text(size = 12, face = "plain"),
    axis.text = element_text(size = 11, colour = "black"),
    legend.position = "none",
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.8),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks = element_line(colour = "black", linewidth = 0.5),
    axis.ticks.length = unit(-0.15, "cm"),
    axis.line = element_line(colour = "black", linewidth = 0.6)
  ) +
  scale_y_continuous(labels = function(x) sub("\\.0$", "", x))

# Combine SA plots into a single multi-panel figure with cowplot
legend_SA <- get_legend(plot_mass_SA)
plots_SA <- plot_grid(plot_mass_SA, plot_preferences_SA, labels = c("A", "B"))
final_SA <- plot_grid(plots_SA, legend_SA, ncol = 1, rel_heights = c(1, 0.1))
title_SA <- ggdraw() + draw_label("S. araneus", fontface = 'italic', x = 0.5, hjust = 0.5)
plot_grid(title_SA, final_SA, ncol = 1, rel_heights = c(0.1, 1))


# =====================================================================
# SECTION 3: Crocidura gueldenstaedtii (CS) - Behavioral & Mass Analysis
# =====================================================================

# Filter data for Crocidura gueldenstaedtii (CS)
CS <- filter(data, Species == "CS")

# Linear Mixed-Effects Model for Preferences
modelP_CS <- lmer(
  Preferences ~ Type * Infected + Rogers + (1 | Animal),
  data = CS
)
summary(modelP_CS)
emm2_CS <- emmeans(modelP_CS, ~ Type * Infected)
pairs(emm2_CS)

# Linear Mixed-Effects Model for Body Mass (mg)
modelM_CS <- lmer(
  `Mass (mg)` ~ Type * Infected + Rogers + (1 | Animal),
  data = CS
)
summary(modelM_CS)
emm_CS <- emmeans(modelM_CS, ~ Type * Infected)
pairs(emm_CS)

# --- Visualization: Preferences (CS) ---
plot_preferences_CS <- ggplot(as.data.frame(emm2_CS), 
                              aes(x = Infected, y = emmean, colour = Type, group = Type)) +
  geom_point(size = 3) +
  geom_line(linewidth = 1) +
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE), width = 0.1) +
  labs(title = "", x = "", y = expression("Preferences (" * italic(N) * ")"), colour = "Type") +
  scale_colour_manual(labels = c("M" = "Mealworms", "E" = "Earthworms"),
                      values = c("burlywood4", "#FF1493")) +
  scale_x_discrete(labels = c("YES" = "Contaminated", "NO" = "Uncontaminated")) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "italic"),
    axis.title.x = element_text(size = 12, face = "plain"),
    axis.title.y = element_text(size = 12, face = "plain"),
    axis.text = element_text(size = 11, colour = "black"),
    legend.position = "none",
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.8),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks = element_line(colour = "black", linewidth = 0.5),
    axis.ticks.length = unit(-0.15, "cm"),
    axis.line = element_line(colour = "black", linewidth = 0.6)
  ) +
  scale_y_continuous(labels = function(x) sub("\\.0$", "", x))


# =====================================================================
# SECTION 4: Comparative Mass Percentage Across Species
# =====================================================================

data_summary <- data %>%
  group_by(Species, Food) %>%
  summarise(TotalMass = sum(`Mass (%)`), .groups = "drop") %>%
  group_by(Species) %>%
  mutate(Percent = (TotalMass / sum(TotalMass)) * 100)

ggplot(data_summary, aes(x = Species, y = Percent, fill = Food)) +
  geom_bar(stat = "identity", position = "stack", color = "black", linewidth = 0.1, width = 0.5) +
  theme_minimal(base_size = 12) +
  labs(title = "", x = "", y = "Food eaten (%)", fill = "Food:") +
  scale_fill_manual(values = c("E0" = "#FFC0CB", "EPb" = "#FF1493", "M0" = "coral3", "MPb" = "burlywood4")) +
  scale_x_discrete(labels = c("CS" = "C. gueldenstaedtii", "SA" = "S. araneus")) +
  guides(fill = guide_legend(title = "Food:")) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.2),
    panel.grid = element_blank(),
    axis.line = element_blank(),
    axis.ticks.length = unit(-0.2, "cm"),
    axis.ticks = element_line(color = "black", linewidth = 0.4),
    axis.text.x = element_text(size = 10, face = "italic"),
    axis.text.y = element_text(size = 10, colour = "black"),
    legend.position = "top",
    legend.justification = "left"
  )


# =====================================================================
# SECTION 5: Kaplan-Meier Survival Analysis
# =====================================================================

surv_object <- Surv(data$Minute, data$Event)
fit <- survfit(surv_object ~ Food, data = data)

# Kaplan-Meier Curve generation for S. araneus with customized theme
p <- ggsurvplot(
  fit, data = data, pval = FALSE, conf.int = TRUE, risk.table = FALSE,
  title = "Order", subtitle = expression(italic("S. araneus")),
  xlab = "Time (min)", legend.title = "Food",
  legend.labs = c("E0", "EPb", "M0", "MPb"),
  palette = c("#FFC0CB", "#FF1493", "coral3", "burlywood4"),
  size = 1.5,
  ggtheme = theme_minimal(base_size = 12) + 
    theme(
      plot.title = element_text(hjust = 0.5, size = 14),
      plot.subtitle = element_text(hjust = 0.5, size = 12, face = "italic"),
      panel.border = element_blank(),
      axis.line = element_blank(),
      panel.grid = element_blank(),
      axis.ticks.length = unit(-0.2, "cm"),
      axis.ticks = element_line(color = "black", linewidth = 0.4),
      axis.text = element_text(size = 10, colour = "black")
    )
)

p$plot <- p$plot +
  coord_cartesian(clip = "off") +
  scale_x_continuous(labels = function(x) gsub("\\.0$", "", as.character(x))) +
  scale_y_continuous(labels = function(y) gsub("\\.0$", "", as.character(y))) +
  theme(
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.6),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks.length = unit(-0.15, "cm")
  ) +
  annotate("text", x = -Inf, y = Inf, label = "(b)", hjust = 1, vjust = -6, size = 5) +
  annotate("text", x = 125, y = 1, label = "italic(P) == 0.87", parse = TRUE, size = 5)

print(p)
