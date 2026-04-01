library(ggplot2)
library(gganimate)
library(dplyr)

# 1. Data Generation (Consistent Logic)
set.seed(123)
tp_names <- c("Baseline", "W2", "W4", "W8", "W12", "W24")
patient_ids <- paste("Patient", 1:10)

df_gif <- expand.grid(Timepoint = tp_names, PatientID = patient_ids) %>%
  group_by(PatientID) %>% 
  mutate(
    BaseVal = runif(1, 65, 95),
    Trend = case_when(
      PatientID == "Patient 1" ~ seq(0, -50, length.out = 6), 
      PatientID == "Patient 4" ~ c(0, -10, -20, 10, 35, 50),  
      TRUE ~ seq(0, -10, length.out = 6)                     
    ),
    Level = round(pmax(0, BaseVal + Trend + rnorm(6, 0, 2)), 1)
  ) %>%
  arrange(Timepoint) %>% 
  mutate(
    Baseline_Level = Level[1],
    Pct_Change = round(((Level - Baseline_Level) / Baseline_Level) * 100, 1),
    Label = ifelse(Timepoint == "Baseline", "START", paste0(Pct_Change, "%")),
    Text_Col = ifelse(Pct_Change <= 0, "#00FFFF", "#FFFF00"),
    # FIX: Create a numeric version of the Patient ID for the X-axis
    PatientNum = as.numeric(factor(PatientID, levels = patient_ids))
  ) %>%
  ungroup() %>%
  mutate(Timepoint = factor(Timepoint, levels = tp_names))

# 2. The Plot (Using PatientNum for the X-axis)
p <- ggplot(df_gif, aes(x = PatientNum, y = Level)) +
  # BACKGROUND: Target Zone
  annotate("rect", xmin = 0.5, xmax = 10.5, ymin = 20, ymax = 50, 
           fill = "#10B981", alpha = 0.15) +
  annotate("text", x = 0.3, y = 35, label = "TARGET", 
           color = "#059669", angle = 90, fontface = "bold", size = 4) +
  
  # BACKGROUND: High Risk Line
  geom_hline(yintercept = 100, color = "#EF4444", linetype = "dashed", size = 1) +
  annotate("text", x = 1.5, y = 105, label = "HIGH RISK THRESHOLD", 
           color = "#EF4444", fontface = "bold", size = 3.5) +

  # The Patient Bubbles
  geom_point(aes(fill = PatientID), shape = 21, size = 22, color = "black", stroke = 1.5, show.legend = FALSE) +
  
  # The % Labels
  geom_text(aes(label = Label, color = Text_Col), size = 5.5, fontface = "bold") +
  
  # Colors & Scales
  scale_fill_manual(values = c("#B2182B", "#2166AC", "#1B7837", "#8C510A", "#4D4D4D", 
                               "#762A83", "#B35806", "#252525", "#01665E", "#542788")) +
  scale_color_identity() +
  
  # FIX: Map the numbers back to the Patient names
  scale_x_continuous(breaks = 1:10, labels = patient_ids) +
  coord_cartesian(ylim = c(0, 160), xlim = c(0.5, 10.5)) +
  
  labs(
    title = 'Biomarker Progression: {closest_state}', 
    subtitle = 'Tracking 10 Patients from Baseline through Week 24',
    caption = 'VISUAL KEY: Cyan = Improvement | Yellow = Increase | Green Zone = Target Range',
    y = 'Concentration (ng/mL)', x = ''
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 22, face = "bold", color = "#1E3A8A"),
    plot.subtitle = element_text(size = 14, color = "#475569", margin = margin(b=20)),
    plot.caption = element_text(size = 11, hjust = 0, color = "#334155", face = "italic"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(face = "bold", size = 11)
  ) +
  
  # Animation
  transition_states(Timepoint, transition_length = 3, state_length = 2) +
  ease_aes('quintic-in-out')

# 3. Save the GIF
anim_save("final_annotated_trajectory.gif", animation = p, 
          width = 900, height = 700, fps = 25, duration = 15)
