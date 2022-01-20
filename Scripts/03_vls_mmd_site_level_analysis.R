# PURPOSE: TX_MMD & VLS analysis at the site level
# COLLABORATOR: T Essam | 
# DATE: 2022-01-10
# NOTES: Seemed like an intersting idea to look at a bit more

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
  library(broom)

  datain <- "Data"
  dataout <- "Dataout"
  graphics <- "Graphics"
  
  #path to Q3 msd output file
  merdata <- glamr::si_path("path_msd") 
  last_msd <- merdata %>% return_latest(pattern = "Site_IM_FY19-22_2021121")
  
  
  #caption info for plotting
  source <- source_info(last_msd)
  author <- "Tim Essam | SI Team"
  
  #current FY and quarter
  curr_fy <- source_info(last_msd, return = "fiscal_year")
  curr_qtr <- source_info(last_msd, return = "quarter")
  curr_pd <- source_info(last_msd, return = "period")


# MAP and COMBINE ------------------------

  msd_list <- list.files(merdata, "Site_IM_FY19-22_2021121", full.names = T)
  
  df_all <- map_dfr(msd_list, .f = ~load_site_msd(.x))
  
  
  df_mmd_site <- 
    df_all %>% 
    munge_mmd(., sitename, orgunituid, psnu, snu1, operatingunit, fiscal_year, indicator, otherdisaggregate) %>% 
    create_vl(., orgunituid) %>% 
    create_mmd(.) %>% 
    mutate(fy = extract_fy(period)) %>% 
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
    mutate(pd = factor(period) %>% as.numeric()) %>% 
    filter(VLS <= 1, 
           VLS > 0) %>% 
    mutate(pd_flag = ifelse(pd>=5, 1, 0)) %>% 
    group_by(snu1, pd_flag) %>% 
    mutate(mmd_group_3plus = mean(tx_mmd_3plus_sh, na.rm = T),
           mmd_group_6plus = mean(tx_mmd_6mo_sh, na.rm = T)) %>% 
    ungroup() %>% 
    clean_countries(colname = "operatingunit") %>% 
    group_by(orgunituid, operatingunit) %>% 
    mutate(tx_mmd_lag = lag(tx_mmd_3plus_sh, n = 2, order = pd)) %>% 
    ungroup()


# VIZ AND CORRELATE -------------------------------------------------------

  df_mmd_site %>% 
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
    labs(title = "SITE LEVEL VIRAL LOAD SUPPRESION ROSE CONSIDERABLY FROM FY19 - FY21",
         subtitle = "The distribution of SNU1 VLS tighted as well",
         x = NULL, y = NULL) +
    facet_wrap(~size_label)  

  
  return_cor(df_mmd_site, size_label) %>% 
  select(`TX_CURR Group` = size_label, estimate, statistic, `p.value`, obs = parameter) %>% 
    gt::gt()  %>% 
    fmt_number(columns = 2:4,
               decimals = 3)

  
  clean_countries(colname = "operatingunit") %>% 
    group_by(operatingunit) %>% 
    mutate(corr = cor(VLS, tx_mmd_lag, use = "pairwise.complete.obs")) %>% 
    ungroup() %>% 
    mutate(ou_order = fct_reorder(paste(operatingunit, percent(corr, 1)), corr, .desc = T)) %>% 
    filter(pd %in% c(7:12)) %>% 
    ggplot(aes(x = tx_mmd_lag, y = VLS)) +
    geom_point(alpha = 0.5, color = scooter_med) +
    stat_smooth(method = "lm", color = grey50k, fill = grey20k) +
    facet_wrap(~ou_order, scales = "free") +
    scale_x_continuous(labels = label_percent(1))+
    scale_y_continuous(labels = label_percent(1)) +
    si_style(facet_space = 0.5) +
    labs(x = "Two-period lagged 3+ MMD share of TX_CURR",
         y = "Viral load suppression")  
  
  
  ols_rbst2 <- estimatr::lm_robust(VLS ~ tx_mmd_lag + factor(period) + factor(size_label),
                                   clusters = psnu,
                                   fixed_effects = ~ psnu,
                                   data = df_mmd_site)

  broom::tidy(ols_rbst2)
   
  