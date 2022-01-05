# PURPOSE: TX_MMD & VLS analysis for IAS abstract
# COLLABORATOR: T Essam | L Bailey
# DATE: 2021-09-08
# NOTES: Request from Zambia SCA


# GLOBALS -----------------------------------------------------------------

library(tidyverse)
library(gophr)
library(sf)
library(glitr)
library(glamr)
library(gisr)
library(scales)
library(here)
library(gt)
library(ggdist)
library(gghalves)
library(ggbeeswarm)
library(readxl)
library(fuzzyjoin)
library(ggnewscale)
library(extrafont)
library(glue)
library(patchwork)
library(ggpubr)

datain <- "Data"
dataout <- "Dataout"
graphics <- "Graphics"

#path to Q3 msd output file
merdata <- glamr::si_path("path_msd") 
psnu_msd <- merdata %>% return_latest(pattern = "PSNU_IM_FY19-22")


#caption info for plotting
source <- source_info(psnu_msd)
author <- "Tim Essam | SI Team"

#current FY and quarter
curr_fy <- source_info(psnu_msd, return = "fiscal_year")
curr_qtr <- source_info(psnu_msd, return = "quarter")
curr_pd <- source_info(psnu_msd, return = "period")

ou_list <- c("Uganda", "Cameroon", "Rwanda", "Tanzania", "Lesotho",
             "Nigeria", "Kenya", "Cote d'Ivoire", "Ethiopia", "Zambia", 
             "Mozambique", "Zimbabwe", "Burundi", "Malawi", "South Sudan",
             "Eswatini", "Democratic Republic of the Congo", "Angola")

psnu_df <- merdata %>% 
  return_latest(pattern = "PSNU_IM_FY19-22_20211112_v1_1.zip") %>% 
  read_msd() %>% 
  filter(operatingunit %in% ou_list,
         fiscal_year <= curr_fy)


# MUNGE -------------------------------------------------------------------

  tx_mmd <- 
    psnu_df %>% 
    filter(indicator == "TX_CURR", 
           standardizeddisaggregate %in% c("Total Numerator", "Age/Sex/ARVDispense/HIVStatus")) %>% 
    mutate(otherdisaggregate = case_when(
            is.na(otherdisaggregate) ~ "total",
            TRUE ~ str_remove(otherdisaggregate, "ARV Dispensing Quantity - ")
            )
    ) %>% 
    group_by(operatingunit, snu1, fiscal_year, indicator, otherdisaggregate) %>% 
    summarise(across(starts_with("qtr"), sum, na.rm = T)) %>% 
    reshape_msd() %>% 
    pivot_wider(names_from = otherdisaggregate, values_from = value) %>% 
    ungroup() %>% 
    rename(tx_mmd_3mo =`3 to 5 months`, 
           tx_mmd_6mo =`6 or more months`,
           TX_CURR = total) %>% 
    rowwise() %>% 
    mutate(tx_mmd_3plus = sum(tx_mmd_3mo, tx_mmd_6mo, na.rm = T)) %>% 
    select(-`Less than 3 months`) %>% 
    mutate(tx_mmd_3plus_sh = tx_mmd_3plus / TX_CURR,
           tx_mmd_6mo_sh = tx_mmd_6mo / TX_CURR)
  
    vls_df <- psnu_df %>% 
      filter(indicator %in% c("TX_CURR", "TX_PVLS"),
             standardizeddisaggregate %in% c("Total Numerator", "Total Denominator")) %>%
      mutate(
        indicator = if_else(
          numeratordenom == "D",
          paste0(indicator, "_D"),
          indicator
        )
      ) %>% 
      group_by(operatingunit, snu1, fiscal_year, indicator, otherdisaggregate) %>% 
      summarise(across(starts_with("qtr"), sum, na.rm = T), .groups = "drop") %>%
      reshape_msd() %>% 
      dplyr::select(-period_type) %>% 
      pivot_wider(names_from = indicator, values_from = value) %>% 
      group_by(snu1) %>% 
      mutate(TX_CURR_LAG2 = lag(TX_CURR, 2, order_by = period),
             VLC = TX_PVLS_D / TX_CURR_LAG2,
             VLS = TX_PVLS / TX_PVLS_D) %>% 
      ungroup() %>% 
      relocate(TX_CURR_LAG2, .before = TX_CURR) 
  
  # Size based on SNU?
  # Merge VLS with TX_MMD and define site volumes using annual TX_CURR average
  # To determine site size, we calculate the average TX_CURR volume for a given site in a given year
  df_viz <- 
    tx_mmd %>% 
    left_join(vls_df) %>% 
    mutate(fy = substr(period, 3, 4)) %>% 
    group_by(snu1, fy) %>% 
    mutate(tx_curr_ave = mean(TX_CURR, na.rm = T) %>% round(., -1)) %>%
    ungroup() %>% 
    group_by(fy) %>% 
    mutate(size = ntile(tx_curr_ave, 3),
           size_label = case_when(
             size == 1 ~ "LOW",
             size == 2 ~ "MEDIUM",
             size == 3 ~ 'HIGH',
             TRUE ~ NA_character_),
           size_label = fct_relevel(size_label, c("LOW", "MEDIUM", "HIGH"))
    ) %>% 
    group_by(size, fy) %>% 
    mutate(tot_tx_curr = sum(tx_curr_ave, na.rm = T),
           grp_tx_share = tx_curr_ave / tot_tx_curr) %>% 
    ungroup() %>% 
    filter(VLS <= 1, 
           VLS > 0) %>% 
    group_by(snu1) %>% 
    mutate(mmd_group_3plus = mean(tx_mmd_3plus_sh, na.rm = T),
           mmd_group_6plus = mean(tx_mmd_6mo_sh, na.rm = T)) %>% 
    ungroup() %>% 
    mutate(pd = factor(period) %>% as.numeric())
  

# EDA ---------------------------------------------------------------------
  
  # Global filter
  ou_list %>% paste(., collapse = ", ")
  
  df_viz  %>% 
    ggplot(aes(x = pd, y = VLC, color = ifelse(VLC >= .7, genoa, old_rose))) + 
    geom_point(position = position_jitter(width = 0.2), alpha = 0.5) +
    geom_hline(yintercept = 0.7, size = .5, linetype = "dotted") +
    facet_wrap(~operatingunit) + si_style() +
    scale_y_continuous(limits = c(0, 1.25), oob = scales::squish, labels = percent) +
    theme(legend.position = "none") +
    scale_x_continuous(labels = label_number_si(), breaks = seq(1, 12, 1)) +
    scale_color_identity()
    
    
  
  
  # Couple of options for the graphics. 
  # 1) show VLS and mmd by snu (ignore countries) and period / fit a line
  # 2) Show VLS and mmds metrics by snu size -- ingoring countries
  
  # What is the VLS summary by volume across time?
  df_vls_ou <- 
    df_viz %>% group_by(period, operatingunit) %>% 
    summarise(tx_pvls = sum(TX_PVLS, na.rm = T),
              tx_pvls_d =sum(TX_PVLS_D, na.rm = T),
              VLS = tx_pvls / tx_pvls_d) %>% 
    ungroup() %>% 
    select(period, operatingunit, VLS) 
  
  df_vls_ou %>% spread(period, VLS)
  
  
  # Check SNU1 count by OU
  df_viz %>% group_by(operatingunit, period) %>% 
    count() %>% 
    spread(period, n)
  
  
  # What does the scatter of MMD v VLS look like by period?
  # size  = 4 for single scatterplot
  # Across the SNU1 TX_CURR volume?
  scatter_vls <- function(xvar) {
    df_viz %>% 
    filter(pd %in% c(5:11)) %>% 
    ggplot(aes(y = VLS, x = lag({{xvar}}, n = 2, order_by = pd)) +
    geom_point(alpha = 0.5, color = scooter_med) +
    geom_point(stroke = .5, shape = 1, color = scooter) +
    stat_smooth(aes(weight = TX_CURR*VLC), method = "lm", 
                color = grey60k, fill = grey30k, size = 0.75) + 
    scale_y_continuous(labels = label_percent(1)) +
    scale_x_continuous(labels = label_percent(1)) +
    si_style(facet_space = 0.5) 
  }  
  
  all_snus <-  scatter_vls(tx_mmd_6mo_sh) +
    facet_wrap("All SNUs"~period, nrow = 1) +
    labs(x = NULL, y= NULL) 
  
  snus_volume <- scatter_vls(tx_mmd_6mo_sh) +
    facet_wrap(size_label ~ period, nrow = 3) +
    labs(x = "6 Month MMD share of TX_CURR", y = "Viral Load Suppression",
         caption = glue::glue("Source: {source}
                               Notes: Line fitted using a linear model with TX_CURR X Viral Load Coverage weights"))
  
  cowplot::plot_grid(all_snus, snus_volume, ncol = 1,
                     align = "hv", axis = "bt",
                     rel_heights = c(1.5, 3))
  
  si_save("Graphics/snu1_level_VLS_mmd3plus.svg", scale = 1.25,
          height = 8.5, width = 11)
  
  
  
  # Show TX_MMD across SNUS
  df_viz_lead <- df_viz %>% 
    group_by(snu1, operatingunit) %>% 
    mutate(VLS_lead = lead(VLS, n = 2, order_by = period),
           tx_mmd_3plus_sh = ifelse(tx_mmd_3plus_sh == 0, NA_real_, tx_mmd_3plus_sh)) %>% 
    ungroup() 
  
  df_viz_lead %>% 
    filter(tx_mmd_3plus_sh > 0) %>% 
    ggplot(aes(x = pd, y = tx_mmd_3plus_sh)) + geom_smooth()
  
  
  df_viz_lead %>% 
    ggplot(aes(x = pd)) +
    geom_vline(xintercept = 4.75, size = 2, color = grey20k) +
    geom_half_point(
      aes(y = tx_mmd_3plus_sh, group = period), 
      color = old_rose, 
      alpha = 0.25, 
      size = 1,
      side = "r"
      )+
    geom_half_point(
      aes(y = VLS, group = period),
      side = "l",
      color = scooter_med,
      alpha = 0.25, 
      size = 1
    ) +
    stat_smooth(aes(y = tx_mmd_3plus_sh), method="gam", formula = y ~ s(x, bs = "cs", k=5),
                color = old_rose, size = 1)  +
    stat_smooth(aes(y = VLS), method="gam", formula = y ~ s(x, bs = "cs", k = 8), 
                color = scooter, size = 1) +
    si_style_ygrid() +
    scale_x_continuous(breaks = 1:12,
                       labels = c("Oct-19 (Q1)", "Q2", "Q3", "Q4", "Oct-20 (Q1)", "Q1", "Q2", "Q3", "Oct-21 (Q1)", "Q2" ,"Q3", "Q4")) +
    labs(y = NULL, x = NULL, 
         title = "AS 3 MONTH+ MULTIMONTH DISPENSING SCALED ACROSS SNUS, VIRAL LOAD SUPPRESSION REMAINED STABLE") +
    scale_y_continuous(labels = percent) 
  
  +
    facet_wrap(~size_label)
  
  
  si_save("Graphics/ias_plot2.svg", scale = 1.25)
  
  
  
  
  
  
  
  
  
  
  
  df_viz %>% 
    group_by(operatingunit, snu1) %>% 
    mutate(VLS_lead = lead(VLS, n = 2, order_by = period)) %>% 
    ungroup() %>% 
    ggplot(aes(x = pd)) +
    geom_point(aes(y = tx_mmd_3plus_sh), color = grey10k, position =  position_jitter(width = 0.2)) +
    geom_point(aes(y = VLS_lead), color = scooter_med, position =  position_jitter(width = 0.2), alpha = 0.25) +
    stat_smooth(data = . %>% filter(pd > 4), aes(y = tx_mmd_3plus_sh)) +
    stat_smooth(aes(y = VLS_lead)) +
    si_style()
  
  # Across all OUS
  snus_ous <- scatter_vls(tx_mmd_6mo_sh) +
    facet_wrap(~operatingunit, labeller = label_wrap_gen(multi_line=FALSE), nrow = 2) +
    labs(x = "6 MMD share of TX_CURR", y = "Viral Load Suppression", 
         caption = glue::glue("Source: {source}
                               Notes: Line fitted using a linear model with TX_CURR X Viral Load Coverage weights"))
    # theme(strip.text = element_text(size = 6))

  
  
  df_viz %>% 
    ggplot(aes(y = period, x = VLS, group =size_label)) +
    geom_point() +
    facet_wrap(operatingunit~size_label)

  # What does a scatterplot of VLS v MMD look like?
  df_viz %>% 
    filter(!pd %in% c(1, 2, 3, 4, 12)) %>% 
    ggplot(aes(y = VLS, x = tx_mmd_3plus_sh, color = operatingunit)) +
    geom_point() +
    stat_smooth(se = F, aes(weight = TX_CURR), color = grey90k) +
    facet_wrap(~operatingunit, scales = "free") +
    si_style() +
    scale_x_continuous(labels = label_percent(1)) +
    scale_y_continuous(labels = label_percent(1))
  
  # What is the correlation across time for each metric?
  df_viz %>% 
    filter(pd %in% c(5:11)) %>% 
    group_by(size_label) %>% 
    summarise(corr = cor(VLS, tx_mmd_3plus_sh),
              corr2 = cor(VLS, tx_mmd_6mo_sh, use = "pairwise.complete.obs")) %>% 
    select(-corr2) %>% 
    spread(period, corr)
  
  lm(VLS ~ tx_mmd_3plus_sh + factor(size_label), data = df_viz) %>% summary()
  

# CLOUD AND RAINFALL PLOT -------------------------------------------------

  # What is clear is that there appears to be a convergence of VLS towards 1 with time
  # High volume areas have 
  
  df_viz %>% 
    ggplot(aes(x = period, y = VLS)) +
    geom_boxplot(
      width = 0.15, fill = "white",
      size = 0.5, outlier.shape = NA,
      color = scooter, 
      alpha = 0.85
    ) +
    stat_halfeye(
      adjust = 0.33, 
      width = 0.67, 
      color = NA,
      position = position_nudge(x = 0.15),
      fill = scooter, alpha = 0.85
    ) +
    geom_half_point(
      side = "l",
      range_scale = 0.25,
      size = 1,
      color = old_rose_light,
      alpha = 0.5
    ) +
    #geom_hline(yintercept  = seq(0, 1, 1/3), size = 0.1, color = grey20k, linetype = "dotted") +
    geom_vline(xintercept = 8.5) +
    si_style_xgrid() +
    coord_flip() +
    scale_x_discrete(limits = rev, position = "top") +
    scale_y_continuous(labels = label_percent(1)) +
    labs(title = "SNU1 VIRAL LOAD SUPPRESION ROSE CONSIDERABLY FROM FY19 - FY21",
         subtitle = "The distribution of SNU1 VLS tighted as well",
         x = NULL, y = NULL)
  
  si_save("Graphics/snu1_level_VLS.svg", scale = 1.25,
          height = 11, width = 8.5)
 

# MMD Saturation and VLS --------------------------------------------------

  # Pick 3 or 6 month
  
  df_viz_mmd <- 
    df_viz %>% 
    mutate(mmd_6bin = ntile(mmd_group_6plus, 5),
           mmd_3bin = ntile(mmd_group_3plus, 5),
           pd = factor(period) %>% as.numeric(),
           mmd_bin_lab = case_when(
             mmd_3bin == 1 ~ "Low 3+month MMD",
             mmd_3bin == 2 ~ "Medium low",
             mmd_3bin == 3 ~ "Medium",
             mmd_3bin == 4 ~ 'Medium high',
             mmd_3bin == 5 ~ "High",
             TRUE ~ NA_character_),
           mmd_bin_lab = fct_relevel(mmd_bin_lab, c("Low 3+month MMD", "Medium low",
                                                    "Medium", "Medium high", "High"))
    ) %>% 
    filter(!is.na(mmd_bin_lab)) 

  
  df_viz_mmd_sum <- 
    df_viz_mmd %>% 
    group_by(pd, mmd_bin_lab) %>% 
    summarise(num = sum(TX_PVLS, na.rm = T),
              denom = sum(TX_PVLS_D, na.rm = T),
              vls = num / denom
              ) %>% 
    ungroup()
  
  df_viz_mmd_sum %>% 
    select(-c(num, denom)) %>% 
    spread(pd, vls)
  
  df_viz_mmd %>% 
    ggplot(aes(x = pd, y = VLS, group = mmd_bin_lab)) +
    geom_point(position = position_jitter(width = 0.2), color = grey20k, alpha = 0.5) + 
    geom_point(data = df_viz_mmd_sum, aes(y = vls, color = mmd_bin_lab), size = 3) +
    stat_smooth(data = df_viz_mmd_sum %>% filter(pd >4), aes(y = vls, color = mmd_bin_lab), size = 1, se = F) +
    geom_vline(xintercept = 4.5, size = 0.5, color = grey90k) +
    # facet_wrap(~mmd_6bin, drop = T, nrow = 2) +
    si_style_ygrid(facet_space = 0.25) +
    scale_color_brewer(palette = "Paired") +
    scale_y_continuous(limits = c(0.75, 1), labels = label_percent(1)) +
    #facet_wrap(~site_size_label) +
    scale_x_continuous(breaks = 1:12,
                       labels = c("FY19Q1", "Q2", "Q3", "Q4", "FY20Q1", "Q1", "Q2", "Q3", "FY21Q1", "Q2" ,"Q3", "Q4")) +
    labs(color = "MMD rate", x = NULL, y = "Viral Load Suppression") +
    guides(color = guide_legend(nrow=2)) 
  
  
  df_viz_mmd_sum <- 
    df_viz_mmd %>% 
    group_by(pd, mmd_bin_lab, operatingunit) %>% 
    summarise(num = sum(TX_PVLS, na.rm = T),
              denom = sum(TX_PVLS_D, na.rm = T),
              vls = num/denom) %>% 
    ungroup()
  


# OU level ----------------------------------------------------------------

df_viz_mmd_ou <- 
  df_viz %>% 
  group_by(operatingunit) %>% 
  mutate(mmd_6bin = ntile(mmd_group_6plus, 3),
         mmd_3bin = ntile(mmd_group_3plus, 3),
         pd = factor(period) %>% as.numeric(),
         mmd_bin_lab = case_when(
           mmd_3bin == 1 ~ "Low 3+month MMD",
           mmd_3bin == 2 ~ "Medium",
           mmd_3bin == 3 ~ "High",
           TRUE ~ NA_character_),
         mmd_bin_lab = fct_relevel(mmd_bin_lab, c("Low 3+month MMD", "Medium", "High"))
  ) %>% 
  filter(!is.na(mmd_bin_lab)) %>% 
  ungroup()

df_viz_mmd_sum <- 
  df_viz_mmd %>% 
  group_by(pd, mmd_bin_lab, operatingunit) %>% 
  summarise(num = sum(TX_PVLS, na.rm = T),
            denom = sum(TX_PVLS_D, na.rm = T),
            vls = num / denom
  ) %>% 
  ungroup()


  df_viz_mmd_sum %>% 
    select(-c(num, denom)) %>% 
    spread(pd, vls) %>% 
    arrange(operatingunit)
  
  
  

# Basic regression --------------------------------------------------------

  df_viz_lead %>% 
    filter(pd>4) %>% 
    group_by(period, operatingunit) %>% 
    summarise(corr = cor(VLS_lead, tx_mmd_3plus_sh, use = "pairwise.complete.obs")) %>% 
    spread(period, corr)

  ols_rbst <- estimatr::lm_robust(VLS_lead ~ tx_mmd_3plus_sh + factor(period),
                                  clusters = operatingunit,
                                  fixed_effects = ~ operatingunit,
                                  data = df_viz_lead)  
  
  
  summary(ols_rbst)  
  
  lm(VLS_lead ~ tx_mmd_3plus_sh + factor(period) + factor(operatingunit), data = df_viz_lead) %>% summary()
  