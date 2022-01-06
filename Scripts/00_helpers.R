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
