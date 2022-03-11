# AD HOD REQUEST

# For the following partners:
#   • Lesotho – Jhpiego USAID PrEP sites
# • Kenya – (1) LVCT Health USAID PrEP sites in Mombasa County, (2) University of Nairobi USAID PrEP sites in Nairobi County, and (3) Jaramogi Oginga University of Science and Technology (in partnership with PATH) USAID PrEP sites in Kisumu County
# • South Africa – Wits RHI USAID PrEP sites
# • Uganda – all Uganda USAID PrEP sites
# • Zimbabwe – (1) ZHI USAID PrEP sites, and (2) OPHID USAID PrEP sites
# 
# The following data points:
#   • PrEP providing facility names
# • Facility Location (urban, peri-urban, or rural)
# • Name of province/county/city where facility is located
# • Name of implementing partner
# • Established PrEP clients (PrEP Curr)
# • Proportion female (%) of Established PrEP and PrEP New
# • New PrEP clients (PrEP New)
# • PrEP client targets for COP20 and COP21


# PURPOSE: Munge and Analysis of
# AUTHOR: Tim Essam | SI
# LICENSE: MIT
# DATE: 2022-03-08
# NOTES: 

# LOCALS & SETUP ============================================================================

  # Libraries
    library(glitr)
    library(glamr)
    library(gisr)
    library(gophr)
    library(tidyverse)
    library(scales)
    library(sf)
    library(extrafont)
    library(tidytext)
    library(patchwork)
    library(ggtext)
    
  
  # Set paths  
    data   <- "Data"
    dataout <- "Dataout"
    images  <- "Images"
    graphs  <- "Graphics"
   
    merdata <- glamr::si_path("path_msd")
    merdata <- glamr::si_path("path_msd")
    rasdata <- glamr::si_path("path_raster")
    shpdata <- glamr::si_path("path_vector")
    datim   <- glamr::si_path("path_datim")  
    
 
  # Functions & lookup tables

# DATA PATH ============================================================================  

  ous <- list.files(path = merdata, 
                    pattern = "Site_IM_FY20-22.*Lesotho|South%20Africa|Uganda|Kenya|Zimbabwe\\.zip",
                    full.names = TRUE) %>% .[3:7]
  

# FUNCTION -----------------------------------------------------------

  # Had to finesse the lookup table a bit based on the IP names. The names provided were not official DATIM names.
    lookup <-   tibble::tribble(
      ~operatingunit,                                                                               ~primepartner,
      "Lesotho",                                                                                  "JHPIEGO ",
      "Kenya",                                                       "Liverpool Vct Care|Nairobi|JARAMOGI",
      "South Africa",                                                                                      "WITS",
      "Uganda",                                                                                          "",
      "Zimbabwe", "Zimbabwe Health Interventions|ORGANIZATION FOR PUBLIC HEALTH INTERVENTIONS AND DEVELOPMENT"
    )
    
  # STEPS
   #1 - load data frame and only prep indicators
   #2 - get the operatingunit into an object
   #3 - subset the lookup by that ou
   #4 - create a primepartner filter for each OU using lookup table
   #5 - combine all data together
  
  load_site_msd <- function(ou_list, ref_table = lookup) {
    
    df <- read_msd(ou_list) %>% filter(str_detect(indicator, "PrEP"))
    
    ou <- unique(df$operatingunit)
    
    # Grab the column from the lookup table that you need to filter the msd
    # To get the limited partners requested
    fltr <- lookup %>% filter(operatingunit == ou) %>% pull(primepartner)
    
    # Kenya request is wonky b/c they want certain PSNUS, so we allow for that in logic
    if(ou == "Kenya"){
      df <- df %>% 
        filter(str_detect(primepartner, fltr)) %>% 
        mutate(psnu_filter = case_when(
          str_detect(psnu, "Kisumu") & str_detect(primepartner, "JARAMOGI") ~ 1,
          str_detect(psnu, "Nairobi") & str_detect(primepartner, "University") ~ 1,
          str_detect(psnu, "Mombasa") & str_detect(primepartner, "Liverpool") ~ 1, 
          TRUE ~ 0)
        ) %>% 
        filter(psnu_filter == 1) %>% 
        select(-psnu_filter)
      
    } else {
        df <- df %>% filter(str_detect(primepartner, fltr))
    }
    
    return(df)
  }
  

# LOAD DATA AND MUNGE -----------------------------------------------------

  curr_fy <- source_info(ous[1], return = "fiscal_year")
 
  # USe purrr to combine everything via the custom function and list of site level msds
  df_prep <- purrr::map_dfr(ous, ~load_site_msd(ou_list = .x)) 
  
  # Check the Kenya logic to make sure it worked
  df_prep %>% 
    filter(operatingunit == "Kenya") %>% 
    count(indicator, psnu, primepartner, indicator, fiscal_year) %>% 
    arrange(psnu)
  
  df_prep %>% 
    distinct(operatingunit, primepartner, indicator, fiscal_year) %>% 
    arrange(operatingunit, primepartner, indicator) %>% prinf()

# ANSWER QUESTIONS --------------------------------------------------------

  # PrEP providing facility names (using PrEP CURR)
  df_prep %>% 
    filter(indicator == "PrEP_NEW", fiscal_year == curr_fy) %>% 
    select(operatingunit, sitename, facility, psnu, snu1, primepartner, cumulative) 
  
  # NOTES:
  # PrEP_CURR reported above site at most places
  # PrEP_CT first time reported is in FY22Q1
  # PrEP targets are only available at PSNU level (need to roll everything up to this level)

  
  
  
  
  
  
  
  
  
  
   

    
