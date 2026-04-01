library(ggplot2)
library(gganimate)
library(dplyr)
library(tidyr)

# =========================================================
# 1. Data generation
# =========================================================
set.seed(123)

tp_names <- c("Baseline", "W2", "W4", "W8", "W12", "W24")
patient_ids <- paste("Patient", 1:10)

df_gif <- expand.grid(
  Timepoint = tp_names,
  PatientID = patient_ids,
  KEEP.OUT.ATTRS = FALSE,
  stringsAsFactors = FALSE
) %>%
  mutate(
    Timepoint = factor(Timepoint, levels = tp_names),
    PatientID = factor(PatientID, levels = patient_ids)
  ) %>%
  arrange(PatientID, Timepoint) %>%
  group_by(PatientID) %>%
  mutate(
    BaseVal = runif(1, 65, 95),
    Trend = case_when(
      PatientID == "Patient 1" ~ seq(0, -50, length.out = 6),          # strong responder
      PatientID == "Patient 4" ~ c(0, -10, -20, 10, 35, 50),           # relapser
      TRUE ~ seq(0, -10, length.out = 6)                               # mild improvement
    ),
    Level = round(pmax(0, BaseVal + Trend + rnorm(6, 0, 2)), 1),
    Baseline_Level = first(Level),
    Pct_Change = round(((Level - Baseline_Level) / Baseline_Level) * 100, 1),
    Label = ifelse(Timepoint == "Baseline", "START", paste0(Pct_Change, "%")),
    Text_Col = ifelse(Pct_Change <= 0, "Improvement", "Increase"),
    PatientNum = as.numeric(PatientID)
  ) %>%
  ungroup()

# =========================================================
# 2. Color palettes
# =========================================================
patient_palette <- c(
  "Patient 1"  = "#C0392B",
  "Patient 2"  = "#1F77B4",
  "Patient 3"  = "#2CA02C",
  "Patient 4"  = "#8E44AD",
  "Patient 5"  = "#E67E22",
  "Patient 6"  = "#16A085",
  "Patient 7"  = "#D35400",
  "Patient 8"  = "#34495E",
  "Patient 9"  = "#7F8C8D",
  "Patient 10" = "#2980B9"
)

label_palette <- c(
  "Improvement" = "#00CFE8",  # bright cyan
  "Increase"    = "#FFD166"   # warm yellow
)

# =========================================================
# 3. Plot
# =========================================================
p <- ggplot(df_gif, aes(x = PatientNum, y = Level)) +

  # Target zone
  annotate(
    "rect",
    xmin = 0.5, xmax = 10.5,
    ymin = 20, ymax = 50,
    fill = "#2ECC71", alpha = 0.14
  ) +
  annotate(
    "text",
    x = 0.72, y = 35,
    label = "TARGET\nZONE",
    color = "#1E8449",
    angle = 90,
    fontface = "bold",
    size = 4.2,
    lineheight = 0.9
  ) +

  # High-risk threshold
  geom_hline(
    yintercept = 100,
    color = "#E74C3C",
    linetype = "dashed",
    linewidth = 1.1
  ) +
  annotate(
    "label",
    x = 2.3, y = 108,
    label = "HIGH-RISK THRESHOLD",
    fill = "white",
    color = "#C0392B",
    fontface = "bold",
    label.size = 0.25,
    size = 3.8
  ) +

  # Subtle guide lines
  geom_hline(
    yintercept = c(20, 50),
    color = "#27AE60",
    linetype = "solid",
    linewidth = 0.35,
    alpha = 0.6
  ) +

  # Patient bubbles
  geom_point(
    aes(fill = PatientID),
    shape = 21,
    size = 20,
    color = "#111827",
    stroke = 1.2,
    show.legend = FALSE
  ) +

  # Labels inside bubbles
  geom_text(
    aes(label = Label, color = Text_Col),
    size = 5.2,
    fontface = "bold",
    show.legend = FALSE
  ) +

  # Scales
  scale_fill_manual(values = patient_palette) +
  scale_color_manual(values = label_palette) +
  scale_x_continuous(
    breaks = 1:10,
    labels = patient_ids,
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  scale_y_continuous(
    limits = c(0, 160),
    breaks = seq(0, 160, by = 20),
    expand = expansion(mult = c(0, 0.02))
  ) +

  # Labels
  labs(
    title = "Biomarker Progression — {closest_state}",
    subtitle = "Tracking 10 patients across a 24-week longitudinal study",
    caption = "Cyan = improvement from baseline | Yellow = increase from baseline | Green band = target range",
    x = NULL,
    y = "Concentration (ng/mL)"
  ) +

  # Theme
  theme_minimal(base_size = 14) +
  theme(
    plot.background = element_rect(fill = "#F8FAFC", color = NA),
    panel.background = element_rect(fill = "#F8FAFC", color = NA),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "#DCE3EA", linewidth = 0.45),

    axis.title.y = element_text(size = 13, face = "bold", color = "#1F2937"),
    axis.text.x = element_text(size = 10.5, face = "bold", color = "#334155"),
    axis.text.y = element_text(size = 10.5, color = "#475569"),

    plot.title = element_text(size = 22, face = "bold", color = "#0F172A"),
    plot.subtitle = element_text(size = 13.5, color = "#475569", margin = margin(b = 18)),
    plot.caption = element_text(size = 10.5, color = "#64748B", hjust = 0, margin = margin(t = 14)),

    plot.margin = margin(20, 25, 20, 25)
  ) +

  # Animation
  transition_states(Timepoint, transition_length = 3, state_length = 2) +
  ease_aes("cubic-in-out")

# =========================================================
# 4. Render / save
# =========================================================
anim <- animate(
  p,
  width = 950,
  height = 700,
  fps = 20,
  duration = 15,
  end_pause = 20,
  renderer = gifski_renderer()
)

anim_save("final_annotated_trajectory.gif", animation = anim)
