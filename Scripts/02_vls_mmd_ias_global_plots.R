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
  group_by(fiscal_year, indicator, otherdisaggregate) %>% 
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
  group_by(fiscal_year, indicator, otherdisaggregate) %>% 
  summarise(across(starts_with("qtr"), sum, na.rm = T), .groups = "drop") %>%
  reshape_msd() %>% 
  dplyr::select(-period_type) %>% 
  pivot_wider(names_from = indicator, values_from = value) %>% 
  # group_by(snu1) %>% 
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
  group_by(fy) %>% 
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
  # group_by(snu1) %>% 
  mutate(mmd_group_3plus = mean(tx_mmd_3plus_sh, na.rm = T),
         mmd_group_6plus = mean(tx_mmd_6mo_sh, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(pd = factor(period) %>% as.numeric())

# Global viz
  df_viz %>% 
    mutate(VLS_lead = lead(VLS, n = 2, order_by = period),
           tx_mmd_3plus_sh2 = ifelse(tx_mmd_3plus_sh == 0, NA_real_, tx_mmd_3plus_sh)) %>% 
    ggplot(aes(x = pd)) +
    geom_vline(xintercept = 4.75, size = 2, color = grey20k) +
    geom_line(aes(y = tx_mmd_3plus_sh), color = old_rose, alpha = 0.8) +
    geom_point(aes(y = tx_mmd_3plus_sh), color = old_rose, size = 12) +
    geom_line(aes(y = VLS), color = scooter_med) +
    geom_point(aes(y = VLS), color = scooter_med, size = 12) +
    geom_text(aes(y = VLS, label = percent(VLS, 1)), size = 10/.pt, color = grey90k, family = "Source Sans Pro") +
    geom_text(aes(y = tx_mmd_3plus_sh, label = percent(tx_mmd_3plus_sh, 1)), size = 10/.pt, color = grey90k,
            family = "Source Sans Pro") +
    si_style_xline() +
    scale_x_continuous(breaks = 1:12,
                       labels = c("FY19Q1", "Q2", "Q3", "Q4", "FY20Q1", "Q1", "Q2", "Q3", "FY21Q1", "Q2" ,"Q3", "Q4")) +
    labs(y = NULL, x = NULL, 
         title = "AS 3 MONTH+ MULTIMONTH DISPENSING SCALED, VIRAL LOAD SUPPRESSION REMAINED STABLE") +
    theme(axis.text.y = element_blank()) 

  si_save("Graphics/overall_ias.svg")
  


   