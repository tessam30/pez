# Helper functions

# Function for correlation testing

  return_cor <- function(df, nest_var) {
    df %>% 
      nest(data = -{{nest_var}}) %>% 
      mutate(test = map(data, ~ cor.test(.x$VLS, .x$tx_mmd_lag)), 
             tidied = map(test, tidy)
             ) %>% 
      unnest(tidied) %>% 
      arrange(desc(estimate))
  }

  
  load_site_msd <- function(df, peds = F) {
    msd <- read_msd(df)

    if(peds == TRUE) {  
      msd <- msd %>% filter(trendscoarse == "<15")
    }
    
    msd <- msd %>% 
         filter(indicator %in% c("TX_CURR", "TX_PVLS"),  
             standardizeddisaggregate %in% c("Total Numerator", "Age/Sex/ARVDispense/HIVStatus",
                                             "Total Denominator")) %>% 
      mutate(otherdisaggregate = case_when(
        is.na(otherdisaggregate) ~ "total",
        TRUE ~ str_remove(otherdisaggregate, "ARV Dispensing Quantity - ")
      )
    ) %>% 
      mutate(
        indicator = if_else(
          numeratordenom == "D",
          paste0(indicator, "_D"),
          indicator
        )
      )
  }
    
  # Make VLS and VLS not INF
  calc_vl <- function(x, y) {
    ifelse(y > 0.000, x/y, NA_real_ )
  }
    
   munge_mmd <- function(df, ...) {
    df %>% 
      group_by(...) %>% 
      summarise(across(starts_with("qtr"), sum, na.rm = T), .groups = "drop") %>% 
      ungroup() %>% 
      mutate(indicator = case_when(
        indicator == "TX_CURR" & otherdisaggregate == "total" ~ "TX_CURR",
        indicator == "TX_CURR" & otherdisaggregate == "3 to 5 months" ~ "TX_MMD_3to5",
        indicator == "TX_CURR" & otherdisaggregate == "Less than 3 months" ~ "TX_MMD_3",
        indicator == "TX_CURR" & otherdisaggregate == "6 or more months" ~ "TX_MMD_6",
        indicator == "TX_CURR" & otherdisaggregate == "total" ~ "TX_CURR",
        TRUE ~ indicator
        )
      ) %>% 
       select(-otherdisaggregate) %>% 
       reshape_msd() %>% 
       spread(indicator, value)
   }
   
   create_vl <- function(df, ...) { 
       df %>% 
       group_by(...) %>% 
       mutate(TX_CURR_LAG2 = lag(TX_CURR, 2, order_by = period),
              VLC = calc_vl(TX_PVLS_D, TX_CURR_LAG2),
              VLS = calc_vl(TX_PVLS, TX_PVLS_D)) %>% 
       ungroup() %>% 
       relocate(TX_CURR_LAG2, .before = TX_CURR) 
   }
   
   create_mmd <- function(df, ...){ 
     df %>% 
     rowwise() %>% 
     mutate(tx_mmd_3plus = sum(TX_MMD_3to5, TX_MMD_6)) %>% 
     select(-TX_MMD_3) %>% 
     mutate(tx_mmd_3plus_sh = tx_mmd_3plus / TX_CURR,
            tx_mmd_6mo_sh = TX_MMD_6 / TX_CURR)
   }
   
   extract_fy <- function(x) {
     substr(x, 3, 4) %>% as.numeric()
   }
  
    