# PURPOSE: TX_MMD & VLS analysis at the site level
# COLLABORATOR: T Essam | 
# DATE: 2022-01-20
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
  library(ggnewscale)
  library(extrafont)
  library(glue)
  library(patchwork)
  library(ggpubr)
  library(broom)


# REVIEW AND LOAD DATA ----------------------------------------------------

  data <- "Data"

  path <- list.files(path = "Data", pattern = "_TE", full.names = T)
  excel_sheets(file.path(data, "Rapport sites USAID-pediatric_TE.xlsx"))
  
  mmd <- excel_sheets(path)[1:3]
  mmd_s <- excel_sheets(path)[5]
  vls <- excel_sheets(path)[4]

  # Setting column types b/c the siteCode is a number, but leading 0s may be stripped
  # so reading it in as a text var
  
  mmd_col_types <- c("text", "text", "numeric", "numeric", "numeric", "numeric")
  mmd1 <- map_dfr(mmd, ~dplyr::mutate(read_excel(path, sheet = .x, 
                                                   col_types = mmd_col_types), 
                                    Year = .x %>% substr(5, 8))) 

  mmd2 <- read_excel(path, sheet = mmd_s, col_types = c(mmd_col_types, "text")
  
    
  vls1 <- read_excel(path, sheet = vls, 
                       col_types = c("text", "text", "numeric", "numeric", "numeric", "text")) 
  

# MUNGE & CLEAN -----------------------------------------------------------

  # It appears that mmd2 daa is duplicated
  mmd_df <- bind_rows(mmd1, mmd2) %>% 
    rename(tx_mmd_3mo = `0-89 jours`,
           tx_mmd_3_5mo = `90-180 jours`,
           tx_mmd_6moplus = `>180 jours`,
           total_mmd_patients = `Patient Unique`) %>% 
    group_by(siteCode, Year) %>% 
    mutate(dup_check = row_number()) %>% 
    # filter(dup_check == 1) %>% 
    group_by(siteCode) %>% 
    mutate(group_id = cur_group_id())
  
  vls <- vls1 %>%  
    group_by(siteCode, Year) %>% 
    mutate(dup_check2 = row_number()) 
    # filter(dup_check == 1)
  
  tmp <- mmd_df %>% 
    left_join(., tmp, by = c("siteCode", "Year"))
    
    