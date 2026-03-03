library(tidyverse)
library(png)
library(grid)
library(baseballr)
library(sabRmetrics)
library(parallelly)



# Sample and plot individual pitches with ellipses
plot_movement = function(pitchername) {
  
  mutated_sc_2025 %>%
    mutate(
      pfx_x = -1 * pfx_x,
      iVB = 12 * pfx_z,
      iHB = 12 * pfx_x
    ) %>%
    filter(pitcher_name == pitchername) %>%
    group_by(pitch_type) %>%
    slice_sample(n = 200) %>%   
    ungroup() %>%
    ggplot(aes(x = iHB, y = iVB, color = pitch_type, fill = pitch_type)) +
    
    # ellipse with confidence interval = 1 SD
    stat_ellipse(geom = "polygon", alpha = 0.20, level = 0.68) +
    
    geom_point(alpha = 0.60, size = 2) +
    
    geom_vline(xintercept = 0, color = 'grey30', linetype = 'solid') +
    geom_hline(yintercept = 0, color = 'grey30', linetype = 'solid') +
    
    scale_y_continuous(
      limits = c(-20, 20),
      breaks = seq(-20, 20, by = 10)
    ) +
    scale_x_continuous(
      limits = c(-20, 20),
      breaks = seq(-20, 20, by = 10)
    ) +
    
    scale_color_manual(values = c(
      "FF" = "#D22D49", 
      "FT" = "#DE6A04", 
      "SI" = "#FE9D00", 
      "FC" = "#933F2C",
      "CH" = "#1DBE3A", 
      "FS" = "#3BACAC", 
      "SC" = "#60DB33", 
      "FO" = "#55CCAB",
      "ST" = "#DDB33A", 
      "SL" = "#EEE716", 
      "SV" = "#93AFD4", 
      "CU" = "#00D1ED",
      "KC" = "#6236CD", 
      "KN" = "#3C44CD", 
      "CS" = "#0068FF"
    )) +
    scale_fill_manual(values = c(
      "FF" = "#D22D49", 
      "FT" = "#DE6A04", 
      "SI" = "#FE9D00", 
      "FC" = "#933F2C",
      "CH" = "#1DBE3A", 
      "FS" = "#3BACAC", 
      "SC" = "#60DB33", 
      "FO" = "#55CCAB",
      "ST" = "#DDB33A", 
      "SL" = "#EEE716", 
      "SV" = "#93AFD4", 
      "CU" = "#00D1ED",
      "KC" = "#6236CD", 
      "KN" = "#3C44CD", 
      "CS" = "#0068FF"
    )) +
    
    labs(
      x = "Horizontal Break (in)",
      y = "Induced Vertical Break (in)",
      title = paste0(pitchername, " — Pitch Movement"),
      color = NULL, 
      fill = NULL
    ) +
    
    theme(
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = 'white'),
      panel.grid.major = element_line(color = 'grey80', linetype = 'dotted'),
      legend.position = "right",
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
    )
}

plot_movement("Woodruff, Brandon")


##### --------- STRIKE ZONE PLOTS BY COUNT/PITCH TYPE -----------###########
plot_heatmap_by_count = function(pitchername, plate_path = "~/Downloads/home_plate.png") {
  
  #Home plate image
  home_plate_img  = png::readPNG(plate_path)
  home_plate_grob = grid::rasterGrob(home_plate_img, interpolate = TRUE)
  
  mutated_sc_2025 %>% 
    filter(pitcher_name == pitchername & !is.na(count)) %>% 
    ggplot(aes(x = plate_x, y = plate_z)) +
    
    #Home plate 
    annotation_custom(
      home_plate_grob,
      #plate width
      xmin = -0.5, xmax = 0.5,    
      #plate location under strikzone
      ymin = 0.10, ymax = 1.00    
    ) +
    
    # --- 2D Filled Density Heatmap ---
    geom_density_2d_filled(
      contour_var = "ndensity",
      bins = 15,
      alpha = 0.80,
      show.legend = FALSE
    ) +
    scale_fill_brewer(
      palette = "RdBu",
      direction = -1
    ) +
    
    facet_grid(~count) +
    
    #Strikezone
    geom_rect(
      aes(
        xmin = -0.83, xmax = 0.83,
        ymin = 1.50, ymax = 3.50
      ),
      color = "black",
      fill = NA,
      linewidth = 1
    ) +
    
    
    coord_fixed(
      ratio = 1.15,
      xlim = c(-1.5, 1.5),
      ylim = c(0.0, 4.5)
    ) +
    
    
    theme_minimal(base_size = 14) +
    theme(
      panel.grid = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      strip.background = element_rect(fill = "white", color = NA),
      strip.text = element_text(face = "bold", size = 12),
      plot.title = element_text(hjust = 0.5, face = "bold", size = 18)
    ) +
    
    labs(
      title = paste0(pitchername, ": Pitch% Heat Map by Count")
    )
}

plot_heatmap_by_count("Armstrong, Shawn")


create_pitchmetric_radar = function(pitcher) {
  
  # Directionality: 1 = higher is better, -1 = lower is better
  # This controls whether we flip the percentile
  direction_lookup = tribble(
    ~pitch_type, ~VAA_dir, ~iVB_dir, ~iHB_dir, ~HAA_dir, ~Spin_dir, ~SpinEff_dir,
    # Fastballs: flat VAA, high rise, high movement, high spin/efficiency
    "FF",  1,  1,  1,  1,  1,  1,
    "SI",  1, -1,  1,  1,  1, -1,
    "FC",  1,  1,  1,  1,  1,  1,
    # Changeups: steep VAA, more drop, more fade, low spin
    "CH", -1, -1,  1, -1, -1, -1,
    "FS", -1, -1,  1, -1, -1, -1,
    "FO", -1, -1,  1, -1, -1, -1,
    # Curveballs: steep VAA, max drop, high spin/efficiency
    "CU", -1, -1,  1, -1,  1,  1,
    "CS", -1, -1,  1, -1,  1,  1,
    "KC", -1, -1,  1, -1,  1,  1,
    # Sliders/Sweepers: steep, more drop, max sweep
    "SL", -1, -1,  1, -1,  1,  1,
    "ST", -1, -1,  1, -1,  1,  1,
    "SV", -1, -1,  1, -1,  1,  1
  )
  
  # Step 1: Compute pitch-level physics & aggregate
  pitcher_avgs = mutated_sc_2025 %>%
    mutate(
      y0 = 50,
      yf = 17/12,
      vy_f = -sqrt(vy0^2 - (2 * ay * (y0 - yf))),
      t = (vy_f - vy0) / ay,
      vz_f = vz0 + (az * t),
      vaa = -atan(vz_f / vy_f) * (180 / pi),
      iVB = 12 * pfx_z,
      iHB = 12 * pfx_x,
      t_plate = (-vy0 - sqrt(vy0^2 - 2 * ay * release_pos_y)) / ay,
      vx_plate = vx0 + ax * t_plate,
      vy_plate = vy0 + ay * t_plate,
      haa = atan(vx_plate / abs(vy_plate)) * 180 / pi,
      movement = sqrt((12 * pfx_x)^2 + (12 * pfx_z)^2),
      transverse_spin = movement / max(movement, na.rm = TRUE) * release_spin_rate,
      spin_efficiency = transverse_spin / release_spin_rate
    ) %>%
    group_by(pitcher_name, pitch_type) %>%
    filter(pitch_type %in% c("FF", "FS", "FC", "SI",
                             "CH", "FO", "CU", "CS",
                             "SL", "ST", "SV", "KC")) %>%
    summarise(
      n_pitches = n(),
      Extension = mean(release_extension, na.rm = TRUE),
      ArmAngle = mean(arm_angle, na.rm = TRUE),
      RelHeight= mean(release_pos_z, na.rm = TRUE),
      VAA  = mean(vaa, na.rm = TRUE),
      HAA  = mean(haa, na.rm = TRUE),
      Velo  = mean(release_speed, na.rm = TRUE),
      iVB = mean(iVB, na.rm = TRUE),
      iHB = mean(iHB, na.rm = TRUE),
      Spin  = mean(release_spin_rate, na.rm = TRUE),
      SpinEfficiency = mean(spin_efficiency, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    filter(n_pitches >= 50)
  
  # Step 2: Target pitcher
  target = pitcher_avgs %>%
    filter(pitcher_name == pitcher) %>%
    select(pitch_type,
           t_ext = Extension, t_aa = ArmAngle,
           t_velo = Velo, t_relh = RelHeight,
           t_VAA = VAA, t_HAA = HAA, t_iVB = iVB, t_iHB = iHB,
           t_Spin = Spin, t_SpinEff = SpinEfficiency)
  
  # Step 3: Join, filter contextually, compute raw percentiles
  radar_data = pitcher_avgs %>%
    inner_join(target, by = "pitch_type") %>%
    filter(
      Extension >= (t_ext - 0.5)  & Extension <= (t_ext + 0.5),
      Velo >= (t_velo - 1.5) & Velo  <= (t_velo + 1.5),
      RelHeight >= (t_relh - 0.5) & RelHeight <= (t_relh + 0.5)
    ) %>%
    group_by(pitch_type) %>%
    summarise(
      VAA_percentile = round(ecdf(VAA)(t_VAA[1]), 3),
      HAA_percentile = round(ecdf(HAA)(t_HAA[1]), 3),
      iVB_percentile = round(ecdf(iVB)(t_iVB[1]), 3),
      iHB_percentile= round(ecdf(iHB)(t_iHB[1]), 3),
      Spin_percentile  = round(ecdf(Spin)(t_Spin[1]), 3),
      SpinEfficiency_percentile = round(ecdf(SpinEfficiency)(t_SpinEff[1]), 3),
      n_comps = n(),
      .groups = 'drop'
    ) %>%
    # Step 4: Join directionality and flip where lower is better
    left_join(direction_lookup, by = "pitch_type") %>%
    mutate(
      VAA_percentile = ifelse(VAA_dir == -1, 1 - VAA_percentile, VAA_percentile),
      HAA_percentile= ifelse(HAA_dir == -1, 1 - HAA_percentile, HAA_percentile),
      iVB_percentile= ifelse(iVB_dir == -1, 1 - iVB_percentile, iVB_percentile),
      iHB_percentile = ifelse(iHB_dir == -1, 1 - iHB_percentile, iHB_percentile),
      Spin_percentile = ifelse(Spin_dir == -1, 1 - Spin_percentile, Spin_percentile),
      SpinEfficiency_percentile = ifelse(SpinEff_dir == -1, 1 - SpinEfficiency_percentile, SpinEfficiency_percentile)
    ) %>%
    select(pitch_type, ends_with("_percentile")) %>%
    pivot_longer(
      cols = ends_with("_percentile"),
      names_to = "metric",
      values_to = "percentile"
    ) %>%
    mutate(metric = gsub("_percentile", "", metric)) %>%
    group_by(pitch_type) %>%
    mutate(
      n_metrics = n(),
      angle   = 2 * pi * (row_number() - 1) / n_metrics,
      x = percentile * cos(angle),
      y = percentile * sin(angle),
      x_label = 1.2 * cos(angle),
      y_label = 1.2 * sin(angle)
    ) %>%
    ungroup()
  
  # Step 5: Plot
  ggplot(radar_data, aes(x = x, y = y, group = pitch_type,
                         color = pitch_type, fill = pitch_type)) +
    geom_polygon(
      data = data.frame(
        percentile = rep(c(0.25, 0.5, 0.75, 1), each = 100),
        angle = rep(seq(0, 2 * pi, length.out = 100), 4)
      ) %>%
        mutate(x = percentile * cos(angle), y = percentile * sin(angle)),
      aes(x = x, y = y, group = percentile),
      fill = NA, color = "grey80", inherit.aes = FALSE
    ) +
    geom_text(
      data = data.frame(
        percentile = c(0.25, 0.5, 0.75, 1),
        label = c("25th", "50th", "75th", "100th")
      ) %>%
        mutate(
          angle_offset = pi / 20,
          x = percentile * cos(angle_offset),
          y = percentile * sin(angle_offset)
        ),
      aes(x = x, y = y, label = label),
      size = 2.5, color = "grey50", inherit.aes = FALSE
    ) +
    geom_segment(
      data = radar_data %>% distinct(pitch_type, metric, .keep_all = TRUE),
      aes(x = 0, y = 0, xend = x_label, yend = y_label),
      color = "grey80", inherit.aes = FALSE
    ) +
    geom_polygon(alpha = 0.3) +
    geom_point(size = 2) +
    geom_text(
      data = radar_data %>% distinct(pitch_type, metric, .keep_all = TRUE),
      aes(x = x_label, y = y_label, label = metric),
      size = 3, inherit.aes = FALSE
    ) +
    facet_wrap(~ pitch_type, nrow = 1) +
    coord_fixed(clip = "off") +
    scale_color_manual(values = c(
      "FF" = "#D22D49", "FT" = "#DE6A04", "SI" = "#FE9D00",
      "FC" = "#933F2C", "CH" = "#1DBE3A", "FS" = "#3BACAC",
      "SC" = "#60DB33", "FO" = "#55CCAB", "ST" = "#DDB33A",
      "SL" = "#EEE716", "SV" = "#93AFD4", "CU" = "#00D1ED",
      "KC" = "#6236CD", "KN" = "#3C44CD", "CS" = "#0068FF"
    )) +
    scale_fill_manual(values = c(
      "FF" = "#D22D49", "FT" = "#DE6A04", "SI" = "#FE9D00",
      "FC" = "#933F2C", "CH" = "#1DBE3A", "FS" = "#3BACAC",
      "SC" = "#60DB33", "FO" = "#55CCAB", "ST" = "#DDB33A",
      "SL" = "#EEE716", "SV" = "#93AFD4", "CU" = "#00D1ED",
      "KC" = "#6236CD", "KN" = "#3C44CD", "CS" = "#0068FF"
    )) +
    labs(
      title = paste("Contextual Pitch Metric Radar -", pitcher),
      subtitle = "Adjusted percentiles vs. similar pitchers (higher = better stuff for that pitch type)"
    ) +
    theme_minimal() +
    theme(
      axis.text = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      legend.position = "none",
      strip.text = element_text(face = "bold", size = 12),
      plot.title = element_text(hjust = 0.5, size = 18, face = 'bold'),
      plot.subtitle = element_text(hjust = 0.5, size = 9),
      plot.margin = margin(10, 30, 10, 30)
    )
}

create_pitchmetric_radar("Gil, Luis")



create_pitcher_radar = function(pitcher) {
  mutated_sc_2025 %>% 
    group_by(pitcher_name, pitch_type) %>% 
    filter(pitch_type %in% c("FF", "FS", "FC", "SI",
                             "CH", "FO", "CU", "CS", 
                             "SL", "ST", "SV",
                             "CS", "KC")) %>% 
    summarise(
      n_pitches = n(),
      `Whiff%` = round(sum(whiff_or_contact == 'whiff', na.rm = T)/
                         sum(swing_or_noswing == 'swing', na.rm = T),3),
      `z-Whiff%` = round(sum(whiff_or_contact == 'whiff' & zone %in% as.character(1:9), na.rm = T)/
                           sum(swing_or_noswing == 'swing' & zone %in% as.character(1:9), na.rm = T),3),
      `PutAway%` = round(sum(events %in% c("strikeout", "strikeout_double_play") & strikes == 2, na.rm = T)/
                           sum(strikes == 2, na.rm = T),3),
      `CS%` = round(sum(description == "called_strike", na.rm = T)/
                      sum(!is.na(description), na.rm = T),3),
      `CSW%` = round(sum(description %in% c("called_strike","swinging_strike", 
                                            "swinging_strike_blocked","foul_tip"), na.rm = T)/
                       sum(!is.na(description), na.rm = T),3),
      `Chase%` = round(sum(swing_or_noswing == 'swing' & zone %in% c("11", "12", "13","14"), na.rm = T)/
                         sum(zone %in% c("11", "12", "13", "14"), na.rm = T),3),
      `Zone%` = round(sum(zone %in% as.character(1:9), na.rm = T)/
                        n(),3),
      xwOBAcon = round(mean(estimated_woba_using_speedangle[type == "X"], na.rm = T),3),
      `Hardhit%` = round(sum(launch_speed >= 95 & type == "X", na.rm = T)/
                           sum(type == "X", na.rm = T),3),
      `GB%` = round(sum(bb_type %in% c("ground_ball") & type == "X", na.rm = T)/
                      sum(type == "X", na.rm = T),3),
      `AirEV` = round(median(launch_speed[bb_type %in% c("fly_ball", "line_drive", "popup")], na.rm = T),3),
      .groups = "drop"
    ) %>% 
    group_by(pitch_type) %>% 
    mutate(
      #ecdf functions based only on pitchers with 100+ pitches minimum
      Whiff_percentile   = round(ecdf(`Whiff%`[n_pitches >= 100])(`Whiff%`),3),
      zWhiff_percentile  = round(ecdf(`z-Whiff%`[n_pitches >= 100])(`z-Whiff%`),3),
      PutAway_percentile = round(ecdf(`PutAway%`[n_pitches >= 100])(`PutAway%`),3),
      CS_percentile      = round(ecdf(`CS%`[n_pitches >= 100])(`CS%`),3),
      CSW_percentile     = round(ecdf(`CSW%`[n_pitches >= 100])(`CSW%`),3),
      Chase_percentile   = round(ecdf(`Chase%`[n_pitches >= 100])(`Chase%`),3),
      Zone_percentile    = round(ecdf(`Zone%`[n_pitches >= 100])(`Zone%`), 3),
      xwOBAcon_percentile = round(1 - ecdf(xwOBAcon[n_pitches >= 100])(xwOBAcon),3),
      HardHit_percentile  = round(1 - ecdf(`Hardhit%`[n_pitches >= 100])(`Hardhit%`),3),
      GB_percentile       = round(ecdf(`GB%`[n_pitches >= 100])(`GB%`),3),
      AirEV_percentile    = round(1 - ecdf(AirEV[n_pitches >= 100])(AirEV),3)
    ) %>% 
    filter(n_pitches >= 50) %>% 
    ungroup() %>% 
    filter(pitcher_name == pitcher) %>% 
    select(pitch_type, ends_with("_percentile")) %>%
    # Pivot to long format
    pivot_longer(
      cols = ends_with("_percentile"),
      names_to = "metric",
      values_to = "percentile"
    ) %>%
    mutate(
      metric = gsub("_percentile", "", metric)
    ) %>%
    # Add coordinates for radar chart
    group_by(pitch_type) %>%
    mutate(
      n_metrics = n(),
      angle = 2 * pi * (row_number() - 1) / n_metrics,
      x = percentile * cos(angle),
      y = percentile * sin(angle),
      x_label = 1.2 * cos(angle),
      y_label = 1.2 * sin(angle)
    ) %>%
    ungroup() %>%
    ggplot(aes(x = x, y = y, group = pitch_type, color = pitch_type, fill = pitch_type)) +
    #Radar background circles
    geom_polygon(
      data = data.frame(
        percentile = rep(c(0.25, 0.5, 0.75, 1), each = 100),
        angle = rep(seq(0, 2*pi, length.out = 100), 4)
      ) %>%
        mutate(x = percentile * cos(angle), y = percentile * sin(angle)),
      aes(x = x, y = y, group = percentile),
      fill = NA, color = "grey80", inherit.aes = FALSE
    ) +
    #labels for the background axis
    geom_text(
      data = data.frame(
        percentile = c(0.25, 0.5, 0.75, 1),
        label = c("25th", "50th", "75th", "100th")
      ) %>%
        mutate(
          angle_offset = pi / 20,
          x = percentile * cos(angle_offset), 
          y = percentile * sin(angle_offset)
        ),
      aes(x = x, y = y, label = label),
      size = 2.5,
      color = "grey50",
      inherit.aes = FALSE
    ) +
    #radial, straight lines
    geom_segment(
      aes(x = 0, y = 0, xend = x_label, yend = y_label),
      color = "grey80",
      inherit.aes = FALSE
    ) +
    #The actual radar plots/points
    geom_polygon(alpha = 0.3) +
    geom_point(size = 2) +
    #Metric labels 
    geom_text(
      aes(x = x_label, y = y_label, label = metric),
      size = 3,
      inherit.aes = FALSE,
      check_overlap = FALSE
    ) +
    facet_wrap(~ pitch_type, nrow = 1) +
    coord_fixed(clip = "off") +
    scale_color_manual(values = c(
      "FF" = "#D22D49",
      "FT" = "#DE6A04",
      "SI" = "#FE9D00",
      "FC" = "#933F2C",
      "CH" = "#1DBE3A",
      "FS" = "#3BACAC",
      "SC" = "#60DB33",
      "FO" = "#55CCAB",
      "ST" = "#DDB33A",
      "SL" = "#EEE716",
      "SV" = "#93AFD4",
      "CU" = "#00D1ED",
      "KC" = "#6236CD",
      "KN" = "#3C44CD",
      "CS" = "#0068FF"
    )) +
    scale_fill_manual(values = c(
      "FF" = "#D22D49",
      "FT" = "#DE6A04",
      "SI" = "#FE9D00",
      "FC" = "#933F2C",
      "CH" = "#1DBE3A",
      "FS" = "#3BACAC",
      "SC" = "#60DB33",
      "FO" = "#55CCAB",
      "ST" = "#DDB33A",
      "SL" = "#EEE716",
      "SV" = "#93AFD4",
      "CU" = "#00D1ED",
      "KC" = "#6236CD",
      "KN" = "#3C44CD",
      "CS" = "#0068FF"
    )) +
    labs(
      title = paste("Pitch Performance Radar Charts -", pitcher),
      subtitle = "Percentile Rankings by Pitch Type (Based on 100+ Pitch Sample)"
    ) +
    theme_minimal() +
    theme(
      axis.text = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      legend.position = "none",
      strip.text = element_text(face = "bold", size = 12),
      plot.title = element_text(hjust = 0.5,
                                size = 18,
                                color = 'black',
                                face = 'bold'),
      plot.subtitle = element_text(hjust = 0.5),
      plot.margin = margin(10, 30, 10, 30)
    )
}

create_pitcher_radar("Palencia, Daniel")









plot_usage_lhb_rhb = function(pitchername) {
  
  plot_data = mutated_sc_2025 %>% 
    filter(pitcher_name == pitchername) %>% 
    group_by(stand, pitch_type) %>% 
    summarise(
      pitches = n(),
      .groups = "drop"
    ) %>% 
    group_by(stand) %>% 
    mutate(
      usage = round(pitches / sum(pitches), 2)
    ) %>% 
    ungroup() %>% 
    select(-pitches) %>% 
    mutate(
      usage = ifelse(stand == "L", -usage, usage)
    ) %>% 
    filter(!is.na(pitch_type)) %>%
    # filter pitches with tiny tiny usage out 
    filter(abs(usage) > 0.001)
  
  n_pitch_types = length(unique(plot_data$pitch_type))
  
  plot_data %>% 
    ggplot(aes(x = pitch_type, y = usage, fill = pitch_type)) +
    geom_col(width = 0.7) +
    # left side labels 
    geom_text(
      data = . %>% filter(usage < 0),
      aes(label = abs(usage)),
      color = "grey60",
      vjust = 0.5,
      hjust = 1.2,  
      size = 3.5
    ) +
    # right side labels 
    geom_text(
      data = . %>% filter(usage > 0),
      aes(label = abs(usage)),
      color = "grey60",
      vjust = 0.5,
      hjust = -0.2, 
      size = 3.5
    ) +
    geom_hline(
      yintercept = 0,
      color = "black",
      linewidth = 0.8,
      linetype = "dotted"
    ) +
    annotate(
      "text",
      x = n_pitch_types + 0.5,  
      y = -0.125,
      label = "vs LHB",
      color = "black",
      fontface = "bold",
      size = 6,
      hjust = 1
    ) +
    annotate(
      "text",
      x = n_pitch_types + 0.5,  
      y = 0.25,
      label = "vs RHB",
      color = "black",
      fontface = "bold",
      size = 6,
      hjust = 1
    ) +
    labs(
      title = paste0(pitchername, ": Pitch Usage vs LHB & RHB"),
      x = NULL,
      y = NULL
    ) +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "white"),
      plot.title = element_text(
        hjust = 0.5,
        size = 18,
        color = "black",
        face = "bold"
      ),
      legend.position = "none"
    ) +
    coord_flip(clip = "off") +
    scale_fill_manual(values = c(
      "FF" = "#D22D49", 
      "FT" = "#DE6A04", 
      "SI" = "#FE9D00", 
      "FC" = "#933F2C",
      "CH" = "#1DBE3A", 
      "FS" = "#3BACAC", 
      "SC" = "#60DB33", 
      "FO" = "#55CCAB",
      "ST" = "#DDB33A", 
      "SL" = "#EEE716", 
      "SV" = "#93AFD4", 
      "CU" = "#00D1ED",
      "KC" = "#6236CD",
      "KN" = "#3C44CD", 
      "CS" = "#0068FF"
    ))
}

plot_usage_lhb_rhb("Woodruff, Brandon")










plot_pitcher_rolling = function(pitchername) {
  
  mutated_sc_2025 %>% 
    filter(pitcher_name == pitchername) %>% 
    group_by(pitcher_name, game_date) %>% 
    summarise(
      ABs = sum(events %in% c("strikeout","single","double","triple","home_run",
                              "force_out","fielders_choice_out","fielders_choice",
                              "triple_play","strikeout_double_play","double_play",
                              "field_out","grounded_into_double_play")),
      swings = sum(swing_or_noswing == "swing", na.rm = TRUE),
      whiffs = sum(description %in% c("foul_tip","swinging_strike","swinging_strike_blocked"), na.rm = TRUE),
      zswings = sum(swing_or_noswing == 'swing' & zone %in% as.character(1:9), na.rm = TRUE),
      oswings = sum(swing_or_noswing == 'swing' & zone %in% c("11","12","13","14"), na.rm = TRUE),
      opitches = sum(zone %in% c("11","12","13","14"), na.rm = TRUE),
      zwhiffs = sum(zone %in% as.character(1:9) & whiff_or_contact == 'whiff', na.rm = TRUE),
      GBs = sum(bb_type %in% c("fly_ball","line_drive","popup"), na.rm = TRUE),
      batted_balls = sum(type == "X", na.rm = TRUE)
    ) %>%
    ungroup() %>%
    arrange(pitcher_name, game_date) %>%
    group_by(pitcher_name) %>%
    mutate(
      #Rolling 7-day whiff%
      whiff_pct_7day = rollsum(whiffs, 7, align = "right", fill = NA, na.rm = TRUE) /
        rollsum(swings, 7, align = "right", fill = NA, na.rm = TRUE),
      #Rolling 7 day z-whiff%
      zwhiff_pct_7day = rollsum(zwhiffs, 7, align = "right", fill = NA, na.rm = TRUE) /
        rollsum(zswings, 7, align = "right", fill = NA, na.rm = TRUE),
      #Rolling 7 day Chase%
      chase_pct_7day = rollsum(oswings, 7, align = "right", fill = NA, na.rm = TRUE) /
        rollsum(opitches, 7, align = "right", fill = NA, na.rm = TRUE)
    ) %>% 
    ungroup() %>% 
    select(game_date, whiff_pct_7day, zwhiff_pct_7day, chase_pct_7day) %>% 
    rename(
      c("Chase%" = "chase_pct_7day",
        "Whiff%" = "whiff_pct_7day",
        "z-Whiff%" = "zwhiff_pct_7day")
    ) %>% 
    pivot_longer(cols = -game_date, names_to = "metric", values_to = "rolling7") %>% 
    arrange(metric, game_date) %>% 
    group_by(metric) %>% 
    mutate(
      date_diff = as.numeric(difftime(game_date, lag(game_date), units = "days")),
      segment = cumsum(ifelse(is.na(date_diff) | date_diff > 14, 1, 0))
    ) %>% 
    ggplot(aes(x = game_date, y = rolling7, color = metric, group = interaction(metric, segment))) +
    geom_line(size = 1.1, alpha = 0.9, na.rm = TRUE) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b") +
    #Start of each month
    geom_vline(
      xintercept = seq(as.Date("2025-03-30"), as.Date("2025-09-28"), by = "month"),
      color = 'grey30', linetype = 'dashed', linewidth = 0.5, alpha = 0.5
    ) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(
      title = "Pitcher: 7-Day Rolling Swing/Miss",
      x = "",
      y = "",
      color = "Metric"
    ) +
    scale_color_manual(
      values = c(
        "Whiff%" = "#003831",
        "z-Whiff%" = "#003087",
        "Chase%" = "#df4601"
      )
    ) +
    theme_minimal(base_size = 14) +
    theme(
      panel.background = element_rect("white"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line.x = element_line(color = 'grey20', linetype = 1),
      plot.title = element_text(hjust = 0.5, size = 18, face = 'bold')
    )
}


plot_pitcher_rolling("Suarez, Robert")



plot_velo_distribution = function(pitchername){
  
  #league avg velo
  lg_avg_velo = mutated_sc_2025 %>% 
    filter(!is.na(pitch_type) & !is.na(release_speed)) %>% 
    group_by(pitch_type) %>% 
    summarise(lg.avg.velo = mean(release_speed, na.rm = T), .groups = "drop")
  
  # Get pitcher data
  pitcher_data = mutated_sc_2025 %>% 
    filter(pitcher_name == pitchername)

  #Make sure league avg velo is unique to each player's pitch type 
  lg_avg_velo_filtered = lg_avg_velo %>% 
    filter(pitch_type %in% unique(pitcher_data$pitch_type))
  
  pitcher_data %>% 
    ggplot(aes(
      x = release_speed, 
      fill = pitch_type, 
      color = pitch_type
    )) +
    geom_vline(
      data = lg_avg_velo_filtered,
      aes(xintercept = lg.avg.velo),
      color = 'black',
      linetype = 'dashed',
      linewidth = 0.8
    ) +
    geom_density(alpha = 0.40, linewidth = 1.0) + 
    facet_grid(~ pitch_type, scales = "free_y") +    
    scale_fill_manual(values = c(
      "FF" = "#D22D49", 
      "FT" = "#DE6A04", 
      "SI" = "#FE9D00", 
      "FC" = "#933F2C",
      "CH" = "#1DBE3A", 
      "FS" = "#3BACAC", 
      "SC" = "#60DB33", 
      "FO" = "#55CCAB",
      "ST" = "#DDB33A", 
      "SL" = "#EEE716", 
      "SV" = "#93AFD4", 
      "CU" = "#00D1ED",
      "KC" = "#6236CD", 
      "KN" = "#3C44CD", 
      "CS" = "#0068FF"
    )) +
    scale_color_manual(values = c(
      "FF" = "#D22D49", 
      "FT" = "#DE6A04", 
      "SI" = "#FE9D00", 
      "FC" = "#933F2C",
      "CH" = "#1DBE3A", 
      "FS" = "#3BACAC", 
      "SC" = "#60DB33", 
      "FO" = "#55CCAB",
      "ST" = "#DDB33A", 
      "SL" = "#EEE716", 
      "SV" = "#93AFD4", 
      "CU" = "#00D1ED",
      "KC" = "#6236CD", 
      "KN" = "#3C44CD", 
      "CS" = "#0068FF"
    )) +
    labs(
      title = paste0(pitchername, " - Release Speed Density by Pitch Type"),
      x = "Release Speed (mph)",
      y = NULL,
      fill = "Pitch Type",
      color = "Pitch Type"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(color = "grey85", linewidth = 0.3),
      panel.spacing = unit(1.5, "lines"),  
      strip.background = element_rect(fill = "white", color = NA),
      strip.text = element_text(size = 12, face = "bold"),
      axis.title = element_text(size = 14, face = "bold"),
      axis.text = element_text(size = 12),
      axis.text.x = element_text(margin = margin(t = 5)), 
      plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
      legend.position = "none",         
      plot.margin = margin(10, 20, 10, 20)
    )
}

  

plot_velo_distribution("Fried, Max")










################################# --------------- FIRST HALF/SECOND HALF PERCENTILE CHART --------------------######################
Pitcher.firstsecondhalf.percentile = function(player_name){
  firsthalf_data = mutated_sc_2025 %>% 
    mutate(
      firstsecondhalf = ifelse(game_date <=  "2025-07-15", "First Half", "Second Half")
    ) %>% 
    group_by(pitcher_name) %>% 
    summarise(
      PAs = sum(events != "truncated_pa" & !is.na(events)),
      firsthalf.pitches_seen = sum(firstsecondhalf == "First Half", na.rm = T),
      #Discipline
      firsthalfchasepct = round(sum(firstsecondhalf == "First Half" & zone %in% c("11", "12", "13", "14") & swing_or_noswing == "swing", na.rm =T)/
                                  sum(firstsecondhalf == "First Half" & zone %in% c("11", "12", "13", "14"), na.rm = T), 3),
      firsthalfz_swingpct = round(sum(firstsecondhalf == "First Half" & zone %in% c("1", "2", "3", "4",
                                                                                    "5", "6", "7", "8", "9") & swing_or_noswing == "swing", na.rm = T)/
                                    sum(firstsecondhalf == "First Half" & zone %in% c("1", "2", "3", "4",
                                                                                      "5", "6", "7", "8", "9"), na.rm = T),3),
      firsthalfheart_swingpct = round(sum(firstsecondhalf == "First Half" & attack_zone_heart == "heart" & swing_or_noswing == "swing", na.rm = T)/
                                        sum(firstsecondhalf == "First Half" & attack_zone_heart == "heart", na.rm = T), 3),
      firsthalfshadow_swingpct = round(sum(firstsecondhalf == "First Half" & attack_zone_shadow == "shadow" & swing_or_noswing == "swing", na.rm = T)/
                                         sum(firstsecondhalf == "First Half" & attack_zone_shadow == "shadow", na.rm = T), 3),
      firsthalfzone_ozonepct = round(firsthalfz_swingpct - firsthalfchasepct, 3),
      firsthalf_Kpct = round(sum(firstsecondhalf == "First Half" & events %in% c("strikeout", "strikeout_double_play"),na.rm = T)/
                               sum(firstsecondhalf == "First Half" & events != "truncated_pa" & !is.na(events), na.rm = T),2),
      firsthalf_BBpct = round(sum(firstsecondhalf == "First Half" & events %in% c("walk"), na.rm = T)/
                                sum(firstsecondhalf == "First Half" & events != "truncated_pa" & !is.na(events), na.rm = T),2),
      #Contact 
      firsthalfwhiffpct = round(sum(firstsecondhalf == "First Half" & whiff_or_contact == "whiff", na.rm = T)/
                                  sum(firstsecondhalf == "First Half" & swing_or_noswing == "swing", na.rm = T),3),
      firsthalfz_whiffpct = round(sum(firstsecondhalf == "First Half" & zone %in% c("1", "2", "3", "4",
                                                                                    "5", "6", "7", "8", "9") & whiff_or_contact == "whiff", na.rm = T)/
                                    sum(firstsecondhalf == "First Half" & zone %in% c("1", "2", "3", "4",
                                                                                      "5", "6", "7", "8", "9") & swing_or_noswing == "swing", na.rm = T),3),
      firsthalfozone_whiffpct = round(sum(firstsecondhalf == "First Half" & !zone %in% c("1", "2", "3", "4",
                                                                                         "5", "6", "7", "8", "9") & whiff_or_contact == "whiff", na.rm = T)/
                                        sum(firstsecondhalf == "First Half" & !zone %in% c("1", "2", "3", "4",
                                                                                           "5", "6", "7", "8", "9") & swing_or_noswing == "swing", na.rm = T),3),
      firsthalfheart_whiffpct = round(sum(firstsecondhalf == "First Half" & attack_zone_heart == "heart" & whiff_or_contact == "whiff", na.rm = T)/
                                        sum(firstsecondhalf == "First Half" & attack_zone_heart == "heart" & swing_or_noswing == "swing", na.rm = T),3),
      firsthalfshadow_whiffpct = round(sum(firstsecondhalf == "First Half" & attack_zone_shadow == "shadow" & whiff_or_contact == "whiff", na.rm = T)/
                                         sum(firstsecondhalf == "First Half" & attack_zone_shadow == "shadow" & swing_or_noswing == "swing", na.rm = T),3),
      #Batted Ball
      firsthalfxwOBAcon = round(mean(estimated_woba_using_speedangle[firstsecondhalf == "First Half" & type == "X"], na.rm = T),3),
      firsthalfsweetspotpct = round(sum(firstsecondhalf == "First Half" & launch_angle >= 8 & launch_angle <= 32 & type == "X", na.rm = T)/sum(firstsecondhalf == "First Half" & type == "X", na.rm = T), 3),
      firsthalfhardhitpct = round(sum(firstsecondhalf == "First Half" & launch_speed >= 95 & type == "X", na.rm = T)/sum(firstsecondhalf == "First Half" & type == "X", na.rm = T), 3),
      firsthalfxwOBA = round(sum(estimated_woba_using_speedangle[firstsecondhalf == "First Half"], na.rm = T)/sum(woba_denom[firstsecondhalf == "First Half"], na.rm = T),3),
      firsthalfGBpct = round(sum(firstsecondhalf == "First Half" & bb_type %in% c("ground_ball"),na.rm =T)/
                               sum(firstsecondhalf == "First Half" & type == "X",na.rm =T),3)
    ) 
  
  qualified_left_data.firsthalf =  firsthalf_data %>% 
    filter(PAs >= 75)
  
  firsthalf_data = firsthalf_data %>% 
    mutate(
      GBpct_percentile = ecdf(qualified_left_data.firsthalf$firsthalfGBpct)(firsthalfGBpct) * 100,
      sweetspotpct_percentile = (1-ecdf(qualified_left_data.firsthalf$firsthalfsweetspotpct)(firsthalfsweetspotpct)) * 100,
      hardhitpct_percentile = (1-ecdf(qualified_left_data.firsthalf$firsthalfhardhitpct)(firsthalfhardhitpct)) * 100,
      xwOBA_percentile = (1-ecdf(qualified_left_data.firsthalf$firsthalfxwOBA)(firsthalfxwOBA)) * 100,
      xwOBAcon_percentile = (1-ecdf(qualified_left_data.firsthalf$firsthalfxwOBAcon)(firsthalfxwOBAcon)) * 100,
      chasepct_percentile = (1-ecdf(qualified_left_data.firsthalf$firsthalfchasepct)(firsthalfchasepct)) * 100,
      z_swingpct_percentile = (1-ecdf(qualified_left_data.firsthalf$firsthalfz_swingpct)(firsthalfz_swingpct)) * 100,
      heart_swingpct_percentile = (1-ecdf(qualified_left_data.firsthalf$firsthalfheart_swingpct)(firsthalfheart_swingpct)) * 100,
      shadow_swingpct_percentile = (1-ecdf(qualified_left_data.firsthalf$firsthalfshadow_swingpct)(firsthalfshadow_swingpct)) * 100,
      BBpct_percentile = (1-ecdf(qualified_left_data.firsthalf$firsthalf_BBpct)(firsthalf_BBpct))*100,
      Kpct_percentile = ecdf(qualified_left_data.firsthalf$firsthalf_Kpct)(firsthalf_Kpct)*100,
      whiffpct_percentile = ecdf(qualified_left_data.firsthalf$firsthalfwhiffpct)(firsthalfwhiffpct) * 100,
      z_whiffpct_percentile = ecdf(qualified_left_data.firsthalf$firsthalfz_whiffpct)(firsthalfz_whiffpct) * 100,
      ozone_whiffpct_percentile = ecdf(qualified_left_data.firsthalf$firsthalfozone_whiffpct)(firsthalfozone_whiffpct) * 100,
      heart_whiffpct_percentile = ecdf(qualified_left_data.firsthalf$firsthalfheart_whiffpct)(firsthalfheart_whiffpct) * 100,
      shadow_whiffpct_percentile = ecdf(qualified_left_data.firsthalf$firsthalfshadow_whiffpct)(firsthalfshadow_whiffpct) * 100
    )
  # Second Half data (right side)
  secondhalf_data = mutated_sc_2025 %>% 
    mutate(
      firstsecondhalf = ifelse(game_date <=  "2025-07-15", "First Half", "Second Half")
    ) %>% 
    group_by(pitcher_name) %>% 
    summarise(
      PAs = sum(events != "truncated_pa" & !is.na(events)),
      secondhalf.pitches_seen = sum(firstsecondhalf == "Second Half", na.rm = T),
      #Discipline
      secondhalfchasepct = round(sum(firstsecondhalf == "Second Half" & zone %in% c("11", "12", "13", "14") & swing_or_noswing == "swing", na.rm =T)/
                                   sum(firstsecondhalf == "Second Half" & zone %in% c("11", "12", "13", "14"), na.rm = T), 3),
      secondhalfz_swingpct = round(sum(firstsecondhalf == "Second Half" & zone %in% c("1", "2", "3", "4",
                                                                                      "5", "6", "7", "8", "9") & swing_or_noswing == "swing", na.rm = T)/
                                     sum(firstsecondhalf == "Second Half" & zone %in% c("1", "2", "3", "4",
                                                                                        "5", "6", "7", "8", "9"), na.rm = T),3),
      secondhalfheart_swingpct = round(sum(firstsecondhalf == "Second Half" & attack_zone_heart == "heart" & swing_or_noswing == "swing", na.rm = T)/
                                         sum(firstsecondhalf == "Second Half" & attack_zone_heart == "heart", na.rm = T), 3),
      secondhalfshadow_swingpct = round(sum(firstsecondhalf == "Second Half" & attack_zone_shadow == "shadow" & swing_or_noswing == "swing", na.rm = T)/
                                          sum(firstsecondhalf == "Second Half" & attack_zone_shadow == "shadow", na.rm = T), 3),
      secondhalfzone_ozonepct = round(secondhalfz_swingpct - secondhalfchasepct, 3),
      secondhalf_Kpct = round(sum(firstsecondhalf == "Second Half" & events %in% c("strikeout", "strikeout_double_play"),na.rm = T)/
                                sum(firstsecondhalf == "Second Half" & events != "truncated_pa" & !is.na(events), na.rm = T),2),
      secondhalf_BBpct = round(sum(firstsecondhalf == "Second Half" & events %in% c("walk"), na.rm = T)/
                                 sum(firstsecondhalf == "Second Half" & events != "truncated_pa" & !is.na(events), na.rm = T),2),
      #Contact 
      secondhalfwhiffpct = round(sum(firstsecondhalf == "Second Half" & whiff_or_contact == "whiff", na.rm = T)/
                                   sum(firstsecondhalf == "Second Half" & swing_or_noswing == "swing", na.rm = T),3),
      secondhalfz_whiffpct = round(sum(firstsecondhalf == "Second Half" & zone %in% c("1", "2", "3", "4",
                                                                                      "5", "6", "7", "8", "9") & whiff_or_contact == "whiff", na.rm = T)/
                                     sum(firstsecondhalf == "Second Half" & zone %in% c("1", "2", "3", "4",
                                                                                        "5", "6", "7", "8", "9") & swing_or_noswing == "swing", na.rm = T),3),
      secondhalfozone_whiffpct = round(sum(firstsecondhalf == "Second Half" & !zone %in% c("1", "2", "3", "4",
                                                                                           "5", "6", "7", "8", "9") & whiff_or_contact == "whiff", na.rm = T)/
                                         sum(firstsecondhalf == "Second Half" & !zone %in% c("1", "2", "3", "4",
                                                                                             "5", "6", "7", "8", "9") & swing_or_noswing == "swing", na.rm = T),3),
      secondhalfheart_whiffpct = round(sum(firstsecondhalf == "Second Half" & attack_zone_heart == "heart" & whiff_or_contact == "whiff", na.rm = T)/
                                         sum(firstsecondhalf == "Second Half" & attack_zone_heart == "heart" & swing_or_noswing == "swing", na.rm = T),3),
      secondhalfshadow_whiffpct = round(sum(firstsecondhalf == "Second Half" & attack_zone_shadow == "shadow" & whiff_or_contact == "whiff", na.rm = T)/
                                          sum(firstsecondhalf == "Second Half" & attack_zone_shadow == "shadow" & swing_or_noswing == "swing", na.rm = T),3),
      #Batted Ball
      secondhalfxwOBAcon = round(mean(estimated_woba_using_speedangle[firstsecondhalf == "Second Half" & type == "X"], na.rm = T),3),
      secondhalfsweetspotpct = round(sum(firstsecondhalf == "Second Half" & launch_angle >= 8 & launch_angle <= 32 & type == "X", na.rm = T)/sum(firstsecondhalf == "Second Half" & type == "X", na.rm = T), 3),
      secondhalfhardhitpct = round(sum(firstsecondhalf == "Second Half" & launch_speed >= 95 & type == "X", na.rm = T)/sum(firstsecondhalf == "Second Half" & type == "X", na.rm = T), 3),
      secondhalfxwOBA = round(sum(estimated_woba_using_speedangle[firstsecondhalf == "Second Half"], na.rm = T)/sum(woba_denom[firstsecondhalf == "Second Half"], na.rm = T),3),
      secondhalfGBpct = round(sum(firstsecondhalf == "Second Half" & bb_type %in% c("ground_ball"),na.rm =T)/
                                sum(firstsecondhalf == "Second Half" & type == "X",na.rm =T),3)
    ) 
  
  qualified_right_data.secondhalf =  secondhalf_data %>% 
    filter(PAs >= 150)
  
  secondhalf_data = secondhalf_data %>% 
    mutate(
      GBpct_percentile = ecdf(qualified_right_data.secondhalf$secondhalfGBpct)(secondhalfGBpct) * 100,
      sweetspotpct_percentile = (1-ecdf(qualified_right_data.secondhalf$secondhalfsweetspotpct)(secondhalfsweetspotpct)) * 100,
      hardhitpct_percentile = (1-ecdf(qualified_right_data.secondhalf$secondhalfhardhitpct)(secondhalfhardhitpct)) * 100,
      xwOBA_percentile = (1-ecdf(qualified_right_data.secondhalf$secondhalfxwOBA)(secondhalfxwOBA)) * 100,
      xwOBAcon_percentile = (1-ecdf(qualified_right_data.secondhalf$secondhalfxwOBAcon)(secondhalfxwOBAcon)) * 100,
      chasepct_percentile = (1-ecdf(qualified_right_data.secondhalf$secondhalfchasepct)(secondhalfchasepct)) * 100,
      z_swingpct_percentile = (1-ecdf(qualified_right_data.secondhalf$secondhalfz_swingpct)(secondhalfz_swingpct)) * 100,
      heart_swingpct_percentile = (1-ecdf(qualified_right_data.secondhalf$secondhalfheart_swingpct)(secondhalfheart_swingpct)) * 100,
      shadow_swingpct_percentile = (1-ecdf(qualified_right_data.secondhalf$secondhalfshadow_swingpct)(secondhalfshadow_swingpct)) * 100,
      BBpct_percentile = (1-ecdf(qualified_right_data.secondhalf$secondhalf_BBpct)(secondhalf_BBpct))*100,
      Kpct_percentile = ecdf(qualified_right_data.secondhalf$secondhalf_Kpct)(secondhalf_Kpct)*100,
      whiffpct_percentile = ecdf(qualified_right_data.secondhalf$secondhalfwhiffpct)(secondhalfwhiffpct) * 100,
      z_whiffpct_percentile = ecdf(qualified_right_data.secondhalf$secondhalfz_whiffpct)(secondhalfz_whiffpct) * 100,
      ozone_whiffpct_percentile = ecdf(qualified_right_data.secondhalf$secondhalfozone_whiffpct)(secondhalfozone_whiffpct) * 100,
      heart_whiffpct_percentile = ecdf(qualified_right_data.secondhalf$secondhalfheart_whiffpct)(secondhalfheart_whiffpct) * 100,
      shadow_whiffpct_percentile = ecdf(qualified_right_data.secondhalf$secondhalfshadow_whiffpct)(secondhalfshadow_whiffpct) * 100
    )
  
  
  # Prepare left side First Half data
  left_player.firsthalf = firsthalf_data %>% 
    filter(pitcher_name == player_name) %>% 
    select(pitcher_name,chasepct_percentile, z_swingpct_percentile,
           heart_swingpct_percentile, shadow_swingpct_percentile,
           BBpct_percentile, Kpct_percentile, whiffpct_percentile, 
           z_whiffpct_percentile, ozone_whiffpct_percentile,
           heart_whiffpct_percentile, shadow_whiffpct_percentile,
           xwOBAcon_percentile, sweetspotpct_percentile,
           hardhitpct_percentile, xwOBA_percentile, GBpct_percentile) %>%
    pivot_longer(
      cols = -pitcher_name,
      names_to = "Metric",
      values_to = "Percentile"
    ) %>% 
    mutate(
      Metric = str_remove(Metric, "_percentile"),
      Side = "First Half"
    )
  
  # Prepare right side Second Half data
  right_player.secondhalf = secondhalf_data %>%
    filter(pitcher_name == player_name) %>%
    select(pitcher_name,chasepct_percentile, z_swingpct_percentile,
           heart_swingpct_percentile, shadow_swingpct_percentile,
           BBpct_percentile, Kpct_percentile, whiffpct_percentile, 
           z_whiffpct_percentile, ozone_whiffpct_percentile,
           heart_whiffpct_percentile, shadow_whiffpct_percentile,
           xwOBAcon_percentile, sweetspotpct_percentile,
           hardhitpct_percentile, xwOBA_percentile, GBpct_percentile) %>%
    pivot_longer(cols = -pitcher_name, names_to = "Metric", values_to = "Percentile") %>%
    mutate(
      Metric = str_remove(Metric, "_percentile"),
      Side = "Second Half"
    )
  
  plot_data.firstsecondhalf = bind_rows(
    left_player.firsthalf %>% mutate(Percentile = -Percentile),
    right_player.secondhalf
  ) %>% 
    mutate(
      player_name_clean = str_c(
        str_extract(pitcher_name, "(?<=, ).*"), " ", 
        str_remove(pitcher_name, ",.*")
      ),
      Metric = factor(Metric, levels = c("chasepct", "z_swingpct", "heart_swingpct",
                                         "shadow_swingpct", "Kpct", "BBpct",
                                         "whiffpct", "z_whiffpct", "ozone_whiffpct",
                                         "heart_whiffpct", "shadow_whiffpct",
                                         "xwOBAcon", "sweetspotpct", "hardhitpct",
                                         "xwOBA", "GBpct")),
      Metric_Label = recode(Metric,
                            "chasepct" = "O-Swing%",
                            "z_swingpct" = "Z-Swing%",
                            "heart_swingpct" = "Heart Swing%",
                            "shadow_swingpct" = "Shadow Swing%",
                            "Kpct" = "K%",
                            "BBpct" = "BB%",
                            "whiffpct" = "whiff%",
                            "z_whiffpct" = "Z-whiff%",
                            "ozone_whiffpct" = "O-whiff%",
                            "heart_whiffpct" = "Heart whiff%",
                            "shadow_whiffpct" = "Shadow whiff%",
                            "xwOBAcon" = "xwOBAcon",
                            "sweetspotpct" = "SwtSpt%",
                            "hardhitpct" = "Hardhit%",
                            "xwOBA" = "xwOBA",
                            "GBpct" = "GB%")
    )
  
  
  
  
  
  ggplot(plot_data.firstsecondhalf, 
         aes(x = Metric_Label, y = Percentile, fill = abs(Percentile))) +
    
    # Background grey rectangles 
    geom_rect(
      data = plot_data.firstsecondhalf %>% distinct(Metric_Label),
      aes(xmin = as.numeric(factor(Metric_Label, levels = rev(levels(factor(plot_data.firstsecondhalf$Metric_Label))))) - 0.40,
          xmax = as.numeric(factor(Metric_Label, levels = rev(levels(factor(plot_data.firstsecondhalf$Metric_Label))))) + 0.40,
          ymin = -100, ymax = 100),
      fill = "#F7F7F7", color = NA, inherit.aes = FALSE
    ) +
    # Actual bars that are blue/grey/red (made wider)
    geom_col(width = 0.65, color = NA) +
    #center black line to divide the two halves 
    geom_hline(yintercept = 0, color = "black", linewidth = 0.8) +
    #percentile bubbles and labels 
    geom_point(aes(y = Percentile), shape = 21, size = 5,
               fill = "white", color = "black", stroke = 1) +
    geom_text(aes(y = Percentile, label = round(abs(Percentile))),
              size = 2.8, fontface = "bold") +
    #dashed light green separators to divide each metric region for both left and right side 
    #yend = 107 for the right, yend = -107 for the left 
    geom_segment(
      data = plot_data.firstsecondhalf %>% distinct(Metric_Label),
      aes(x = as.numeric(factor(Metric_Label, levels = rev(levels(factor(plot_data.firstsecondhalf$Metric_Label))))) - 0.5,
          xend = as.numeric(factor(Metric_Label, levels = rev(levels(factor(plot_data.firstsecondhalf$Metric_Label))))) - 0.5,
          y = -100, yend = -107),
      inherit.aes = FALSE,
      color = "#6BAF92", linetype = "dashed", linewidth = 0.6
    ) +
    geom_segment(
      data = plot_data.firstsecondhalf %>% distinct(Metric_Label),
      aes(x = as.numeric(factor(Metric_Label, levels = rev(levels(factor(plot_data.firstsecondhalf$Metric_Label))))) - 0.5,
          xend = as.numeric(factor(Metric_Label, levels = rev(levels(factor(plot_data.firstsecondhalf$Metric_Label))))) - 0.5,
          y = 100, yend = 107),
      inherit.aes = FALSE,
      color = "#6BAF92", linetype = "dashed", linewidth = 0.6
    ) +
    #Flip the coordinates and make sure our annotations at the top don't get clipped out of the chart
    coord_flip(clip = "off") +
    scale_fill_gradientn(
      colours = c("#0047AB", "#B0B0B0", "#FF4C4C"),
      values = rescale(c(0, 50, 100))
    ) +
    scale_y_continuous(
      breaks = c(-100, -50, 0, 50, 100),
      labels = c("100", "50", "0", "50", "100"),
      limits = c(-120, 120),
      expand = expansion(mult = c(0, 0))
    ) +
    labs(
      title = paste0(unique(plot_data.firstsecondhalf$player_name_clean),
                     " - 2025 Percentile Metrics by Season Half"),
      subtitle = "Percentile Rankings in First Half (Left) and Second Half (Right) of the Season ... 0–100 scale",
      x = NULL,
      y = "Percentile"
    ) +
    
    #Annotations per first half/second half 
    annotate("text", x = 17, y = -60, label = "First Half", 
             fontface = "bold", size = 4.5, color = "#333333") +
    annotate("text", x = 17, y = 60, label = "Second Half", 
             fontface = "bold", size = 4.5, color = "#333333") +
    
    theme_minimal(base_size = 11) +
    theme(
      panel.grid = element_blank(),
      axis.ticks = element_blank(),
      axis.text.x = element_text(face = "bold", size = 9),
      plot.title = element_text(face = "bold", size = 14, hjust = 0.5,
                                margin = margin(b = 5)),
      plot.subtitle = element_text(size = 10, hjust = 0.5,
                                   margin = margin(b = 15)),
      axis.text.y = element_text(face = "bold", color = "black", size = 10),
      legend.position = "none",
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      plot.margin = margin(t = 20, r = 20, b = 10, l = 10)  
    )
  
  
  
  
}
Pitcher.firstsecondhalf.percentile("Palencia, Daniel")
















