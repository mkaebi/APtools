# ================================================================= #
# create_portfolios ####
# Date: 2024-07
# ================================================================= #
# Description:
# -----------#
#   This function creates single sorted portfolios
#
# Input(s):
# --------#
#   A table in long format with a time-period column, an asset identifier column and a characteristic to sort on
#
# Output(s):
# ---------#
#   Table with return of sorted portfolios
#
# Author(s):
# ---------#
#     Mohammed Kaebi (mohammedkaebi23@gmail.com)
#
# Additional note(s):
# ------------------#
#   
# ================================================================= #

# ================================================================= #
# Environment Set Up ####
# ================================================================= #
# Packages:
# --------#
library(tidyverse)
# ================================================================= #

create_portfolios <- function(data, char_col, 
                              return_col,
                              num_groups = 5,
                              value_weighted = FALSE) {
  # This function is made for use of up to 6 groups maximum
  
  # Filter and prepare data (remove NAs and +-Inf)
  data <- data %>%
    drop_na(char_col) %>%
    drop_na(all_of(char_col)) %>%
    drop_na(all_of(return_col)) %>%
    drop_na(market_cap) %>%
    filter(is.finite(get(char_col)))
  
  # Filter groups with at least 5 observations
  data <- data %>%
    group_by(date) %>%
    filter(n() >= 5) %>%
    ungroup()
  
  if(nrow(data) == 0){
    return(list(portfolios = tibble(), break_points = tibble()))
  }
  
  #-------------------------------------------------------------#
  # Main processing to create single sorted portfolios
  # Excess Handling: We use the case_when function to handle the specific cases for distributing excess assets:
  #   
  # When excess == 1, the last asset goes into the highest group.
  # When excess == 2, one asset goes into the lowest group and one into the highest.
  # When excess == 3, one asset goes into each of the three middle groups.
  # When excess == 4, two assets go into the lowest group and two into the highest.
  # When excess == 5, one asset goes into the each group with exception of the highest group.
  #-------------------------------------------------------------#
  
  data <- data %>%
    group_by(date) %>%
    mutate(total_n = n(),
           excess = total_n %% num_groups, # excess for when the number of currencies / groups is not integer
           rank = rank(get(char_col)),
           base_group_size = floor((total_n) / num_groups)
    ) %>%
    arrange(rank) %>%
    mutate(modified_group_size = case_when(
      excess == 1 & rank >= total_n - base_group_size ~ base_group_size + 1, # When excess == 1, the last asset goes into the highest group.
      excess == 2 & (rank <= base_group_size + 1 | rank >= total_n - base_group_size) ~ base_group_size + 1, # When excess == 2, one asset goes into the lowest group and one into the highest.
      excess == 3 & (rank > base_group_size & rank <= total_n - base_group_size) ~ base_group_size + 1, # When excess == 3, one asset goes into each of the middle groups.
      excess == 4 & (rank <= base_group_size + 2 | rank > total_n - base_group_size - 2) ~ base_group_size + 2, # When excess == 4, two assets goes into the highest group and two assets into the lowest
      excess == 5 & rank <= total_n - base_group_size ~ base_group_size + 1, # When excess == 5, each group receives one more asset with exception of the highest group
      #excess == 5 & rank > base_group_size ~ base_group_size + 1, # When excess == 5, each group receives one more asset with exception of the lowest group
      #excess == 5 & rank > total_n - base_group_size - 2 ~ base_group_size + 2,
      #excess == 5 & rank <= base_group_size + 3 ~ base_group_size + 3,
      TRUE ~ base_group_size
    ),
    # Manually calculate the group boundaries
    group_end_1 = base_group_size + (excess == 2) + (excess == 4) * 2 + 1 * (excess == 5), 
    group_end_2 = 2 * base_group_size + (excess == 2) + (excess == 3) + (excess == 4) * 2 + 2 * (excess == 5),
    group_end_3 = 3 * base_group_size + (excess == 2) + 2 * (excess == 3) + (excess == 4) * 2 + 3 * (excess == 5),
    group_end_4 = 4 * base_group_size + (excess == 2) + 3 * (excess == 3) + (excess == 4) * 2 + 4 * (excess == 5),
    group_end_5 = 5 * base_group_size + (excess == 2) + 4 * (excess == 3) + (excess == 4) * 2 + 5 * (excess == 5)
    ) %>%
    mutate(
      group_num = case_when(
        rank <= group_end_1 ~ 1,
        rank <= group_end_2 ~ 2,
        rank <= group_end_3 ~ 3,
        rank <= group_end_4 ~ 4,
        rank <= group_end_5 ~ 5,
        TRUE ~ NA_integer_
      )
    ) %>%
    filter(!is.na(group_num)) # Remove rows with NA in group_num
  
  #-------------------------------------------------------------#
  # Get Portfolio Compositions per date
  #-------------------------------------------------------------#
  
  # Calculate portfolio weights in both weighting schemes
  weights_vw <- data %>%
    group_by(date, group_num) %>%
    mutate(total_market_cap = sum(market_cap)) %>%
    mutate(vw_weight = market_cap / total_market_cap) %>% 
    ungroup() %>% 
    select(date, ticker, vw_weight)
  
  weights_ew <- data %>%
    mutate(ew_weight = 1 / modified_group_size) %>% 
    ungroup()%>% 
    select(date, ticker, ew_weight)
  
  weights <- weights_vw %>% 
    full_join(weights_ew, by = c('date', 'ticker'))
  
  # Get the portfolios composition and add each stock's weight 
  port_comp <- data %>% 
    ungroup() %>% 
    select(date, ticker, group_num)%>% 
    rename(quintile_group = group_num) %>% 
    arrange(date) %>% 
    left_join(weights, by = c('date', 'ticker'))
  
  #-------------------------------------------------------------#
  # Calculate break points
  #-------------------------------------------------------------#
  break_points <- data %>%
    group_by(date) %>%
    summarise(
      group_end_1 = max(get(char_col)[rank <= group_end_1], na.rm = TRUE),
      group_end_2 = max(get(char_col)[rank <= group_end_2], na.rm = TRUE),
      group_end_3 = max(get(char_col)[rank <= group_end_3], na.rm = TRUE),
      group_end_4 = max(get(char_col)[rank <= group_end_4], na.rm = TRUE),
      group_end_5 = max(get(char_col)[rank <= group_end_5], na.rm = TRUE)
    )
  
  #-------------------------------------------------------------#
  # Calculate portfolios return
  #-------------------------------------------------------------#
  if (value_weighted) {
    portfolios <- data %>%
      group_by(date, group_num) %>%
      mutate(total_market_cap = sum(market_cap)) %>%
      mutate(weight = market_cap / total_market_cap) %>%
      summarise(port_excess_ret_rx = sum(get(return_col) * weight), .groups = 'drop') %>%
      pivot_wider(names_from = 'group_num', names_prefix = "P", values_from = 'port_excess_ret_rx') %>%
      ungroup()
  } else {
    portfolios <- data %>%
      group_by(date, group_num) %>%
      summarise(port_excess_ret_rx = mean(get(return_col)), .groups = 'drop') %>%
      pivot_wider(names_from = 'group_num', names_prefix = "P", values_from = 'port_excess_ret_rx') %>%
      ungroup()
  }
  
  return(list(portfolios = portfolios, 
              break_points = break_points,
              portfolio_comp = port_comp))
}