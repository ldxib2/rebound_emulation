
# Load libraries ----------------------------------------------------------

library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(viridis)
library(vroom)
library(ggpattern)


# Set up config file & paths --------------------------------------------

script_dir <- this.path::this.dir()
wd <- dirname(dirname(script_dir))
# set the working directory as FTT folder
setwd(wd)

# main paths TODO: are any unnecessary
data_path <- "Emulation/data/"
## Output paths
preds_dir <- paste0(data_path, "/predictions/")

# Prediction file names EDIT FOR PLOT DATA
bases_preds <- "bases_preds_lhc.csv"

# Load emulator prediction data
df <- vroom(paste0(preds_dir, bases_preds))


# Policy factors function -------------------------------------------------

# add plotting vars to dataframe
add_policy_factors <- function(df){
  df %>% 
    # Create US policy factor variable
    mutate(
      # reb_level_tri = factor(
      #   case_when(
      #     reb_switch == 0 ~ "no rebound",
      #     reb_switch == 1 & 
      #       heat_rebound == 0 & 
      #       trans_rebound == 0 ~ "low rebound",
      #     reb_switch == 1 & 
      #       heat_rebound == 1 & 
      #       trans_rebound == 1 ~ "high rebound",
      #   ),
      #   levels = c("no rebound", "low rebound", "high rebound")
      # ),
      # reb_active = factor(
      #   case_when(
      #     reb_switch == 0 ~ "no rebound",
      #     reb_switch == 1 ~ "rebound"
      #   ),
      #   levels = c("no rebound", "rebound")
      # ),
      turn_level_bi = factor(
        case_when(
          trans_turnover == 0 &
            heat_turnover == 0 ~ "low turnover",
          trans_turnover == 1 &
            heat_turnover == 1 ~ "high turnover"
        ),
        levels = c("low turnover", "high turnover")
      ),
      Value = Value/1000,
      leads = factor(
        case_when(
          lead_solar < 0.5 & lead_onshore < 0.5 ~ "Short tech lead",
          lead_solar >= 0.5 & lead_onshore >= 0.5 ~ "Long tech lead"
        ),
        levels = c("Short tech lead", "Long tech lead")
      ),
      cr_total = factor(
        case_when(
          cr_wind < 0.5 & cr_solar < 0.5 ~ "Low Cannibalisation",
          cr_solar >= 0.5 & cr_wind >= 0.5 ~ "High Cannibalisation"
        ),
        levels = c("Low Cannibalisation", "High Cannibalisation")
      ),
      turn_level = factor(
        case_when(
          trans_turnover < 0.5 & heat_turnover < 0.5 ~ "low turnover",
          trans_turnover >= 0.5 & heat_turnover >= 0.5 ~ "high turnover"
        ),
        levels = c("low turnover", "high turnover")
      ),
      reb_level = factor(
        case_when(
          trans_rebound < 0.5 & heat_rebound < 0.5 ~ "low rebound",
          trans_rebound >= 0.5 & heat_rebound >= 0.5 ~ "high rebound"
        ),
        levels = c("low rebound", "high rebound")
      ),
      power_transition = factor(
        case_when(
          cr_total == "High Cannibalisation" &
            leads == "Long tech lead" ~ "slow transition",
          cr_total == "Low Cannibalisation" &
            leads == "Short tech lead" ~ "fast transition"
        ),
        levels = c("slow transition", "fast transition")
      ),    
    )
}



# LHC plots ------------------------------------------------------------

# Add in policy factors
df <- add_policy_factors(df)


# Energy demand GLOBAL HI/LOW REBOUND vs NO REBOUND  --------------------------------------------------------
# Create sample for testing- large df
sampled_ids <- df |>
  distinct(sample_id) |>
  slice_sample(prop = 0.1) |>  # 20% of unique IDs
  pull(sample_id)

df_sampled <- df |>
  filter(sample_id %in% sampled_ids)


# Plot
 p <- 
  df_sampled[df_sampled$Year >= 2020,] %>% # cut off historical data
  ggplot(aes(x = Year, y = Value, group = interaction(sample_id, emulator), color = emulator)) +

  geom_line(alpha = 0.05, linewidth = 0.05) + 
  facet_wrap(~turn_level_bi) + 
    
  labs(x = expression("Year"), 
       y = "Energy Demand (TJ") + 
    scale_colour_manual(
      values = c(
        "energy_demand_no" = "#d73027",
        "energy_demand_rb" = "#4575b4"
      ),
      labels = c(
        "energy_demand_no" = "No rebound",
        "energy_demand_rb" = "Rebound"
      ),
      name = "Emulator"    ) +
   guides(
     colour = guide_legend(override.aes = list(alpha = 2, linewidth = 2))  # fix here
   ) +
  theme(
    legend.position = "bottom",
    axis.title.x  = element_text(size = 14, face = "bold"),
    axis.title.y  = element_text(size = 14, face = "bold"),
    axis.text.x   = element_text(size = 12, face = "bold", angle = 45, hjust = 1),
    axis.text.y   = element_text(size = 12),
    strip.text    = element_text(size = 13),
    legend.title  = element_text(size = 14, face = "bold"),
    legend.text   = element_text(size = 12),
    plot.title    = element_text(size = 18, face = "bold", hjust = 0.5),
    panel.spacing.x = unit(0.5, "lines")
  ) 
p
# Save to figures folder - FIG 4
ggsave(paste0(data_path, "figures/emiss_gbl.png"), p, 
       width = 10, height = 8, dpi = 100)


# Emissions under power system scenarios ----------------------------------

# compute statistics for plot
summary_stats <- df_gbl %>%
  group_by(reb_switch, leads, cr_total) %>%
  summarise(median_pred = mean(prediction, na.rm = TRUE)) %>%
  drop_na()

# plot
p <- df_gbl[complete.cases(df_gbl[, c("leads", "cr_total")]),] %>%
  ggplot(aes(x = prediction, fill = reb_active)) +
  geom_density(alpha = 0.5, adjust = 3) + 
  
  geom_vline(data = summary_stats, aes(xintercept = median_pred),
             linetype = "dashed", size = 0.6, show.legend = FALSE) +
  
  labs(x = expression("Emissions Levels (GtCO"[2]*" cumulative)"), 
       y = "Density") + 
  #scale_fill_viridis_d(option = "C") +
  facet_grid(leads ~ cr_total) +
  theme(
    legend.position = "bottom",
    axis.title.x  = element_text(size = 14, face = "bold"),
    axis.title.y  = element_text(size = 14, face = "bold"),
    axis.text.x   = element_text(size = 12, face = "bold", angle = 45, hjust = 1),
    axis.text.y   = element_text(size = 12),
    strip.text    = element_text(size = 13),
    legend.title  = element_text(size = 14, face = "bold"),
    legend.text   = element_text(size = 12),
    plot.title    = element_text(size = 18, face = "bold", hjust = 0.5),
    panel.spacing.x = unit(0.5, "lines")
  )
p

# emissions under set power scens with turnover -----------------------------------------

# compute statistics for plot
summary_stats <- df_gbl %>%
  group_by(reb_level, turn_level, power_transition) %>%
  summarise(median_pred = mean(prediction, na.rm = TRUE)) %>%
  drop_na()

# plot
p <- df_gbl[complete.cases(df_gbl[, c("power_transition", "turn_level", "reb_level")]),] %>%
  ggplot(aes(x = prediction, fill = reb_active)) +
  geom_density(alpha = 0.5, adjust = 2.5) + 
  
  geom_vline(data = summary_stats, aes(xintercept = median_pred),
             linetype = "dashed", size = 0.6, show.legend = FALSE) +
  
  labs(x = expression("Emissions Levels (GtCO"[2]*" cumulative)"), 
       y = "Density") + 
  #scale_fill_viridis_d(option = "C") +
  facet_grid(turn_level + reb_level ~ power_transition, scales = 'free') +
  scale_x_continuous(limits = c(800, 880)) + 
  
  theme(
    legend.position = "bottom",
    axis.title.x  = element_text(size = 14, face = "bold"),
    axis.title.y  = element_text(size = 14, face = "bold"),
    axis.text.x   = element_text(size = 12, face = "bold", angle = 45, hjust = 1),
    axis.text.y   = element_text(size = 12),
    strip.text    = element_text(size = 13),
    legend.title  = element_text(size = 14, face = "bold"),
    legend.text   = element_text(size = 12),
    plot.title    = element_text(size = 18, face = "bold", hjust = 0.5),
    panel.spacing.x = unit(0.5, "lines")
  )
p

