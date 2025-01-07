# Andrew Mandovi
# ORISE EPA - Office of Research and Development, Pacific Coastal Ecology Branch, Newport, OR
# Originally created: Jan 3, 2025
# Last updated: Jan 3, 2025

library(tidyverse)
library(dplyr)

### Below I copied specific sections from older file (early 2024) where I attempted to write QARTOD tests from when I had data from 5 NEPs (Tampa, Coastal Bend, Casco, Barnegat, and Tillamook)

read_all_csv = function(filepath){
  setwd(filepath)
  file_names = list.files(path=filepath,pattern='*Data.csv',full.names=TRUE)
  print(file_names)
  if (length(file_names) == 0) {
    stop('No CSVs found in specified directory')
  }
  all_dataframes = list() # initialize empty list to store dataframes
  for (i in seq_along(file_names)){ # read each CSV file into a dataframe
    df = read.csv(file_names[i])
    all_dataframes[[paste0('dataframe',i)]] = df
  }
  return(all_dataframes)
}

filter_columns_list = list(
  c('TEMPERATURE','SALINITY','PH','PCO2','DO'), # Tampa Bay
  c('TEMPERATURE','SALINITY','PH','PCO2'),      # Coastal Bend
  c('TEMPERATURE','SALINITY','PH','PCO2','DO'), # Casco Bay 
  c('TEMPERATURE','SALINITY','PH','DO'),        # Barnegat Bay
  c('TEMPERATURE','SALINITY','PH','DO')         # Tillamook 
)

site_names = c('Tampa Bay','Coastal Bend','Casco Bay','Barnegat Bay','Tillamook Bay')

# Function from which I attempted to write QARTOD flag scripts. 
flag_data_multi = function(all_dataframes, filter_columns_list, site_names) {
  # applies QARTOD flagging across multiple data frames (all_dataframes, a list of data frames) for specific parameters (in filter_columns list) to filter across... 
  # .. and applies a data flagging system, and converts timestamps to the nearest second
  # timestamp_format:
  #     1 = '%Y%m%d %H:%M'
  #     2 = '%m/%d/%Y %H:%M'
  # FLAGGING SYSTEM: flag 1-5
  # 1 = PASS, Data cleared all QA protocols
  # 2 = SUSPECT (e.g. between a low threshold and a high threshold for 1+ tests)
  # 3 = FAIL, Bad or missing data (NA/NaN) or data exceeds a high threshold for 1+ tests
  # 4 = TRANSPORT/OUT OF WATER 
  # 5 = Not Evaluated
  
  ## PARAMETERIZATION: Edit these prior to running, customized for the specific NEP site/region: (with default values)
  # Min-Max values for measured values in sensor - acceptable limit values for the gross range test
  ph_min = 5
  ph_max = 9
  temp_min = -1
  temp_max = 35
  sal_min = 0
  sal_max = 40
  pco2_min = 0
  pco2_max = 2500
  do_min = 0
  do_max = 20
  num_sd_for_rate_of_change = 3 # how many standard deviations to apply to the rate-of-change test (default = 3)
  time_window = 24*60*60  # window of time used for rate-of-change and spike tests (default= 24-hours in seconds)
  spike_low_threshold = 1.5
  spike_high_threshold = 3
  num_flatline_sus = 2
  num_flatline_fail = 3
  
  ###
  
  flagged_dataframes = list() # create a blank list
  for (i in seq_along(all_dataframes)){ # 'i' loops through the various data frames
    df = all_dataframes[[i]]
    df$flag = rep(5,length(df$MOORING_NAME)) # add primary Flag column, default to 5 (not evaluated)
    ## FLAG 4: Remove data that is known to be gathered with instrument out of water/in transport/conditioning based on deployment notebook and/or pressure sensor
    # TBD
    
    ## flag  2-3: SUSPECT or BAD data during instrument deployment
    # Flag any NANs as bad:
    filter_columns = filter_columns_list[[i]]
    print(filter_columns)
    for (j in seq_along(filter_columns)) {  # 'j' loops through each column title from every corresponding filter_column_list[i]
      xnan = which(is.na(df[filter_columns[j]])==TRUE)
      df$flag[xnan] = 3
    }
    
    
    #!!!2025comment# Here I clearly just made 'flag' represent gross range, even though there would be additional tests to come..
    #!!!2025comment# .. It appears I make a binary true/false array, analogous to the data frame df, which is true where it is either below the min or above the max, and then set the..
    #!!!2025comment# .. flag to FAIL (3) in those rows. No matter which parameter fails, if one of them fails the flag value for that row will read a '3'. 
    # Gross range test: pH (FAIL = flag 3)
    if ('PH' %in% filter_columns) {
      bad_ph_min = which(df$PH < ph_min)
      bad_ph_max = which(df$PH > ph_max)
      df$flag[bad_ph_min] = 3
      df$flag[bad_ph_max] = 3
    }
    # Gross range test: Temperature (FAIL = flag 3)
    if ('TEMPERATURE' %in% filter_columns) {
      bad_temp_min = which(df$TEMPERATURE < temp_min)
      bad_temp_max = which(df$TEMPERATURE > temp_max)
      df$flag[bad_temp_min] = 3
      df$flag[bad_temp_max] = 3
    }
    # Gross range test: Salinity (FAIL = flag 3)
    if ('SALINITY' %in% filter_columns) {
      bad_sal_min = which(df$SALINITY < sal_min)
      bad_sal_max = which(df$SALINITY > sal_max)
      df$flag[bad_sal_min] = 3
      df$flag[bad_sal_max] = 3
    }
    # Gross range test: pCO2(FAIL = flag 3)
    if ('PCO2' %in% filter_columns) {
      bad_pco2_min = which(df$PCO2 < pco2_min)
      bad_pco2_max = which(df$PCO2 > pco2_max)
      df$flag[bad_pco2_min] = 3
      df$flag[bad_pco2_max] = 3
    }
    # Gross range test: DO (FAIL = flag 3)
    if ('DO' %in% filter_columns) {
      bad_do_min = which(df$DO < do_min)
      bad_do_max = which(df$DO > do_max)
      df$flag[bad_do_min] = 3
      df$flag[bad_do_max] = 3
    }
    
    #!!!2025comment#  I don't know why I made "empty" flag columns with 5's for these tests but not the gross range test.
    #!!!2025comment# .. I think I had done gross range first and then realized I would need to make much more.  
    #!!!2025comment# This method seems better though. I make a flag column unique to each test and parameter. 
    
    ## Create flags for each specific measurement to be assessed and flagged (to be able to pinpoint which measurement is being flagged)
    if ('PH' %in% filter_columns) {
      df$flag_roc_ph = rep(5,nrow(df))
      df$flag_spike_ph = rep(5,nrow(df))
      df$flag_flatline_ph = rep(1,nrow(df))
      df$flag_ph = rep(5,nrow(df))
    }
    if ('TEMPERATURE' %in% filter_columns) {
      df$flag_roc_temp = rep(5,nrow(df))
      df$flag_spike_temp = rep(5,nrow(df))
      df$flag_flatline_temp = rep(1,nrow(df))
      df$flag_temp = rep(5,nrow(df))
    }
    if ('SALINITY' %in% filter_columns) {
      df$flag_roc_sal = rep(5,nrow(df))
      df$flag_spike_sal = rep(5,nrow(df))
      df$flag_flatline_sal = rep(1,nrow(df))
      df$flag_sal = rep(5,nrow(df))
    }
    if ('PCO2' %in% filter_columns) {
      df$flag_roc_pco2 = rep(5,nrow(df))
      df$flag_spike_pco2 = rep(5,nrow(df))
      df$flag_flatline_pco2 = rep(1,nrow(df))
      df$flag_pco2 = rep(5,nrow(df))
    }
    if ('DO' %in% filter_columns) {
      df$flag_roc_do = rep(5,nrow(df))
      df$flag_spike_do = rep(5,nrow(df))
      df$flag_flatline_do = rep(1,nrow(df))
      df$flag_mv_ph_do = rep(5,nrow(df))
      df$flag_do = rep(5,nrow(df))
    }
    
    ## RATE OF CHANGE TEST: whether the data is exceeding 3 st deviations within a 24 hour period 
    # Create a new flag column (flag_RoC) which indicates how the data performs on the rate of change test:
    # 1 PASS
    # 2 SUSPECT: Fails rate of change test -> SUSPECT in 'flag' column
    # 5 TEST NOT PERFORMED either due to failed prior test or insufficient 24-hour data
    
    df$flag_RoC = df$flag
    #!!!2025comment# Here I filter out data which failed the gross range test and create a filtered dataset ('valid_data') which removes failed data. 
    valid_data = df %>%
      filter(flag!=3)
    for (k in 1:nrow(valid_data)) {
      current_row = valid_data[k,]
      # subset 24-hour window data:
      window_start = current_row$timestamp_utc - time_window # 24 hours prior to timestamp
      window_end = current_row$timestamp_utc
      window_data = valid_data %>%
        filter(timestamp_utc >= window_start & timestamp_utc < window_end)
      
      # check if minimum points threshold reached:
      if (nrow(window_data) < 3) {
        df$flag[df$timestamp_utc == current_row$timestamp_utc] = 2  # !!!! How should this be handled under master 'flag'???
        df$flag_RoC[df$timestamp_utc == current_row$timestamp_utc] = 5
      } else { # if minimum threshold met, perform the test:
        if ('PH' %in% filter_columns) {
          sd_PH = sd(window_data$PH)
          mean_PH = mean(window_data$PH)
          df$ph_24h_sd[df$timestamp_utc == current_row$timestamp_utc] = sd_PH
          df$ph_24h_mean[df$timestamp_utc == current_row$timestamp_utc] = mean_PH
          if (current_row$PH > mean_PH+num_sd_for_rate_of_change*sd_PH || 
              current_row$PH < mean_PH-num_sd_for_rate_of_change*sd_PH) {
            df$flag[df$timestamp_utc == current_row$timestamp_utc] = 2
            df$flag_RoC[df$timestamp_utc == current_row$timestamp_utc] = 2
            df$flag_roc_ph[df$timestamp_utc == current_row$timestamp_utc] = 2
          } else {
            df$flag_roc_ph[df$timestamp_utc == current_row$timestamp_utc] = 1
          }
        }
        if ('TEMPERATURE' %in% filter_columns) {
          sd_temp = sd(window_data$TEMPERATURE)
          mean_temp = mean(window_data$TEMPERATURE)
          df$temp_24h_sd[df$timestamp_utc == current_row$timestamp_utc] = sd_temp
          df$temp_24h_mean[df$timestamp_utc == current_row$timestamp_utc] = mean_temp
          if (current_row$TEMPERATURE > mean_temp+num_sd_for_rate_of_change*sd_temp || 
              current_row$TEMPERATURE < mean_temp-num_sd_for_rate_of_change*sd_temp) {
            df$flag[df$timestamp_utc == current_row$timestamp_utc] = 2
            df$flag_RoC[df$timestamp_utc == current_row$timestamp_utc] = 2
            df$flag_roc_temp[df$timestamp_utc == current_row$timestamp_utc] = 2
          } else {
            df$flag_roc_temp[df$timestamp_utc == current_row$timestamp_utc] = 1
          }
        }
        if ('SALINITY' %in% filter_columns) {
          sd_sal = sd(window_data$SALINITY)
          mean_sal = mean(window_data$SALINITY)
          df$sal_24h_sd[df$timestamp_utc == current_row$timestamp_utc] = sd_sal
          df$sal_24h_mean[df$timestamp_utc == current_row$timestamp_utc] = mean_sal
          if (current_row$SALINITY > mean_sal+num_sd_for_rate_of_change*sd_sal || 
              current_row$SALINITY < mean_sal-num_sd_for_rate_of_change*sd_sal) {
            df$flag[df$timestamp_utc == current_row$timestamp_utc] = 2
            df$flag_RoC[df$timestamp_utc == current_row$timestamp_utc] = 2
            df$flag_roc_sal[df$timestamp_utc == current_row$timestamp_utc] = 2
          } else {
            df$flag_roc_sal[df$timestamp_utc == current_row$timestamp_utc] = 1
          }
        }
        if ('PCO2' %in% filter_columns) {
          sd_pco2 = sd(window_data$PCO2)
          mean_pco2 = mean(window_data$PCO2)
          df$pco2_24h_sd[df$timestamp_utc == current_row$timestamp_utc] = sd_pco2
          df$pco2_24h_mean[df$timestamp_utc == current_row$timestamp_utc] = mean_pco2
          if (current_row$PCO2 > mean_pco2+num_sd_for_rate_of_change*sd_pco2 || 
              current_row$PCO2 < mean_pco2-num_sd_for_rate_of_change*sd_pco2) {
            df$flag[df$timestamp_utc == current_row$timestamp_utc] = 2
            df$flag_RoC[df$timestamp_utc == current_row$timestamp_utc] = 2
            df$flag_roc_pco2[df$timestamp_utc == current_row$timestamp_utc] = 2
          } else {
            df$flag_roc_pco2[df$timestamp_utc == current_row$timestamp_utc] = 1
          }
        }
        if ('DO' %in% filter_columns) {
          sd_do = sd(window_data$DO)
          mean_do = mean(window_data$DO)
          df$do_24h_sd[df$timestamp_utc == current_row$timestamp_utc] = sd_do
          df$do_24h_mean[df$timestamp_utc == current_row$timestamp_utc] = mean_do
          if (current_row$DO > mean_do+num_sd_for_rate_of_change*sd_do || 
              current_row$DO < mean_do-num_sd_for_rate_of_change*sd_do) {
            df$flag[df$timestamp_utc == current_row$timestamp_utc] = 2
            df$flag_RoC[df$timestamp_utc == current_row$timestamp_utc] = 2
            df$flag_roc_do[df$timestamp_utc == current_row$timestamp_utc] = 2
          } else {
            df$flag_roc_do[df$timestamp_utc == current_row$timestamp_utc] = 1
          }
        }
      }
    }
    flag3_RoC = which(df$flag_RoC==3)
    flag5_RoC = which(df$flag_RoC==5)
    df$flag_RoC[flag5_RoC] = 1 # pass Rate of Change test: all points which still have a '5'
    df$flag_RoC[flag3_RoC] = 5 # turn all remaining '3' (failed a previous test) into '5' NOT EVALUATED for Rate of Change
    
    
    ## SPIKE TEST: Checking for a spike in value relative to the previous data point
    # Create a new flag column (flag_spike)
    # 1 PASS
    # 2 SUSPECT
    # 3 FAIL
    # 5 NOT EVALUATED (due to failing a previous test)
    df$flag_spike = rep(4,nrow(df)) # make flag_spike all = 4 (Not YET evaluated)
    flag3 = which(df$flag==3)        # index of where flag==3 (Data which FAILED gross range test)
    df$flag_spike[flag3] = 5         # make flag_spike = 5 (NOT EVALUATED), where flag==3
    df$flag_spike[1] = 5             # set first flag_spike to 5 (NOT EVALUATED) - should it be suspect though?
    df$flag_spike[nrow(df)] = 5      # set last flag_spike to 5 (NOT EVALUATED)  - should it be suspect though?
    valid_data = df %>%
      filter(flag!=3)
    # to this point:
    # flag_spike values: 4 (not YET evaluated) and 5 (will NOT be evaluated)
    # flag values: 2 (suspect in previous test), 3 (failed previous test), and 5 (thus far, passed all tests)
    for (m in 2:nrow(valid_data)-1) { # exclude first and last rows of dataset
      current_row = valid_data[m,] # m'th row of data
      window_start = current_row$timestamp_utc - time_window # 24 hours prior to timestamp
      window_end = current_row$timestamp_utc
      window_data = valid_data %>%
        filter(timestamp_utc >= window_start & timestamp_utc < window_end)
      if ('PH' %in% filter_columns) {
        if (!is.na(current_row$ph_24h_sd)) {
          # define low and high thresholds:
          THRESHOLD_LOW_PH = current_row$ph_24h_sd*spike_low_threshold 
          THRESHOLD_HIGH_PH = current_row$ph_24h_sd*spike_high_threshold 
          current_val = valid_data$PH[m]
          previous_val = valid_data$PH[m-1]
          next_val = valid_data$PH[m+1]
          SPIKE_REF = (previous_val+next_val)/2
          spike = abs(current_val-SPIKE_REF)
          if (spike >= THRESHOLD_HIGH_PH) { # FAIL
            df$flag[df$timestamp_utc == current_row$timestamp_utc] = 3
            df$flag_spike[df$timestamp_utc == current_row$timestamp_utc] = 3
            df$flag_spike_ph[df$timestamp_utc == current_row$timestamp_utc] = 3
          } else if (spike >= THRESHOLD_LOW_PH) { # SUSPECT
            df$flag[df$timestamp_utc == current_row$timestamp_utc] = 2
            df$flag_spike[df$timestamp_utc == current_row$timestamp_utc] = 2
            df$flag_spike_ph[df$timestamp_utc == current_row$timestamp_utc] = 2
          } else { # PASS
            df$flag_spike_ph[df$timestamp_utc == current_row$timestamp_utc] = 1
          }
        }
      }
      if ('TEMPERATURE' %in% filter_columns) {
        if (!is.na(current_row$temp_24h_sd)) {
          # define low and high thresholds:
          THRESHOLD_LOW_TEMP = current_row$temp_24h_sd*spike_low_threshold
          THRESHOLD_HIGH_TEMP = current_row$temp_24h_sd*spike_high_threshold
          current_val = valid_data$TEMPERATURE[m]
          previous_val = valid_data$TEMPERATURE[m-1]
          next_val = valid_data$TEMPERATURE[m+1]
          SPIKE_REF = (previous_val+next_val)/2
          spike = abs(current_val-SPIKE_REF)
          if (spike >= THRESHOLD_HIGH_TEMP) { # FAIL
            df$flag[df$timestamp_utc == current_row$timestamp_utc] = 3
            df$flag_spike[df$timestamp_utc == current_row$timestamp_utc] = 3
            df$flag_spike_temp[df$timestamp_utc == current_row$timestamp_utc] = 3
          } else if (spike >= THRESHOLD_LOW_TEMP) { # SUSPECT
            df$flag[df$timestamp_utc == current_row$timestamp_utc] = 2
            df$flag_spike[df$timestamp_utc == current_row$timestamp_utc] = 2
            df$flag_spike_temp[df$timestamp_utc == current_row$timestamp_utc] = 2
          } else { # PASS
            df$flag_spike_temp[df$timestamp_utc == current_row$timestamp_utc] = 1
          }
        }
      }
      if ('SALINITY' %in% filter_columns) {
        if (!is.na(current_row$sal_24h_sd)) {
          # define low and high thresholds:
          THRESHOLD_LOW_SAL = current_row$sal_24h_sd*spike_low_threshold
          THRESHOLD_HIGH_SAL = current_row$sal_24h_sd*spike_high_threshold
          current_val = valid_data$SALINITY[m]
          previous_val = valid_data$SALINITY[m-1]
          next_val = valid_data$SALINITY[m+1]
          SPIKE_REF = (previous_val+next_val)/2
          spike = abs(current_val-SPIKE_REF)
          if (spike >= THRESHOLD_HIGH_SAL) { # FAIL
            df$flag[df$timestamp_utc == current_row$timestamp_utc] = 3
            df$flag_spike[df$timestamp_utc == current_row$timestamp_utc] = 3
            df$flag_spike_sal[df$timestamp_utc == current_row$timestamp_utc] = 3
          } else if (spike >= THRESHOLD_LOW_SAL) { # SUSPECT
            df$flag[df$timestamp_utc == current_row$timestamp_utc] = 2
            df$flag_spike[df$timestamp_utc == current_row$timestamp_utc] = 2
            df$flag_spike_sal[df$timestamp_utc == current_row$timestamp_utc] = 2
          } else { # PASS
            df$flag_spike_sal[df$timestamp_utc == current_row$timestamp_utc] = 1
          }
        }
      }
      if ('PCO2' %in% filter_columns) {
        if (!is.na(current_row$pco2_24h_sd)) {
          # define low and high thresholds:
          THRESHOLD_LOW_PCO2 = current_row$pco2_24h_sd*spike_low_threshold
          THRESHOLD_HIGH_PCO2 = current_row$pco2_24h_sd*spike_high_threshold
          current_val = valid_data$PCO2[m]
          previous_val = valid_data$PCO2[m-1]
          next_val = valid_data$PCO2[m+1]
          SPIKE_REF = (previous_val+next_val)/2
          spike = abs(current_val-SPIKE_REF)
          if (spike >= THRESHOLD_HIGH_PCO2) { # FAIL
            df$flag[df$timestamp_utc == current_row$timestamp_utc] = 3
            df$flag_spike[df$timestamp_utc == current_row$timestamp_utc] = 3
            df$flag_spike_pco2[df$timestamp_utc == current_row$timestamp_utc] = 3
          } else if (spike >= THRESHOLD_LOW_PCO2) { # SUSPECT
            df$flag[df$timestamp_utc == current_row$timestamp_utc] = 2
            df$flag_spike[df$timestamp_utc == current_row$timestamp_utc] = 2
            df$flag_spike_pco2[df$timestamp_utc == current_row$timestamp_utc] = 2
          } else { # Pass
            df$flag_spike_pco2[df$timestamp_utc == current_row$timestamp_utc] = 1
          }
        }
      }
      if ('DO' %in% filter_columns) {
        if (!is.na(current_row$do_24h_sd)) {
          # define low and high thresholds:
          THRESHOLD_LOW_DO = current_row$do_24h_sd*spike_low_threshold
          THRESHOLD_HIGH_DO = current_row$do_24h_sd*spike_high_threshold
          current_val = valid_data$DO[m]
          previous_val = valid_data$DO[m-1]
          next_val = valid_data$DO[m+1]
          SPIKE_REF = (previous_val+next_val)/2
          spike = abs(current_val-SPIKE_REF)
          if (spike >= THRESHOLD_HIGH_DO) { # FAIL
            df$flag[df$timestamp_utc == current_row$timestamp_utc] = 3
            df$flag_spike[df$timestamp_utc == current_row$timestamp_utc] = 3
            df$flag_spike_do[df$timestamp_utc == current_row$timestamp_utc] = 3
          } else if (spike >= THRESHOLD_LOW_DO) { # SUSPECT
            df$flag[df$timestamp_utc == current_row$timestamp_utc] = 2
            df$flag_spike[df$timestamp_utc == current_row$timestamp_utc] = 2
            df$flag_spike_do[df$timestamp_utc == current_row$timestamp_utc] = 2
          } else { # Pass
            df$flag_spike_do[df$timestamp_utc == current_row$timestamp_utc] = 1
          }
        }
      }
    }
    # Turn all remaining flag_spikes (= 4) into = 1 (PASS)
    flag_spike4 = which(df$flag_spike == 4)
    df$flag_spike[flag_spike4] = 1
    ##
    
    ## FLAT LINE TEST: 
    # 1 = PASS
    # 2 = SUSPECT
    # 3 = FAIL
    # 5 = Not Evaluated (Failed prior test)
    df$flag_flatline = rep(4,nrow(df))  # set all to '4' initially
    # make valid_data from which to work with: data which has not failed previous tests
    valid_data = df %>%
      filter(flag!=3)   
    # suspect_threshold = 2
    # fail_threshold = 4
    for (n in (num_flatline_fail+1):nrow(valid_data)) { # starting on 4th row, because impossible to look back 3 rows otherwise
      current_row = valid_data[n,]
      if ('PH' %in% filter_columns) {
        if (valid_data$PH[n] == valid_data$PH[n-1]) {
          df$flag[df$timestamp_utc == current_row$timestamp_utc] = 2
          df$flag_flatline[df$timestamp_utc == current_row$timestamp_utc] = 2    # 2 in a row, SUSPECT
          df$flag_flatline_ph[df$timestamp_utc == current_row$timestamp_utc] = 2 
        }
        if (valid_data$PH[n] == valid_data$PH[n-1] & valid_data$PH[n] == valid_data$PH[n-2]) {
          df$flag[df$timestamp_utc == current_row$timestamp_utc] = 3
          df$flag_flatline[df$timestamp_utc == current_row$timestamp_utc] = 3 # 3 in a row, FAIL
          df$flag_flatline_ph[df$timestamp_utc == current_row$timestamp_utc] = 3 
        }
      }
      if ('TEMPERATURE' %in% filter_columns){
        if (valid_data$TEMPERATURE[n] == valid_data$TEMPERATURE[n-1]) {
          df$flag[df$timestamp_utc == current_row$timestamp_utc] = 2
          df$flag_flatline[df$timestamp_utc == current_row$timestamp_utc] = 2 # 2 in a row, SUSPECT
          df$flag_flatline_temp[df$timestamp_utc == current_row$timestamp_utc] = 2
        }
        if (valid_data$TEMPERATURE[n] == valid_data$TEMPERATURE[n-1] & valid_data$TEMPERATURE[n] == valid_data$TEMPERATURE[n-2]) {
          df$flag[df$timestamp_utc == current_row$timestamp_utc] = 3
          df$flag_flatline[df$timestamp_utc == current_row$timestamp_utc] = 3 # 3 in a row, FAIL
          df$flag_flatline_temp[df$timestamp_utc == current_row$timestamp_utc] = 3
        }
      }
      if ('SALINITY' %in% filter_columns){
        if (valid_data$SALINITY[n] == valid_data$SALINITY[n-1]) {
          df$flag[df$timestamp_utc == current_row$timestamp_utc] = 2
          df$flag_flatline[df$timestamp_utc == current_row$timestamp_utc] = 2 # 2 in a row, SUSPECT
          df$flag_flatline_sal[df$timestamp_utc == current_row$timestamp_utc] = 2
        }
        if (valid_data$SALINITY[n] == valid_data$SALINITY[n-1] & valid_data$SALINITY[n] == valid_data$SALINITY[n-2]) {
          df$flag[df$timestamp_utc == current_row$timestamp_utc] = 3
          df$flag_flatline[df$timestamp_utc == current_row$timestamp_utc] = 3 # 3 in a row, FAIL
          df$flag_flatline_sal[df$timestamp_utc == current_row$timestamp_utc] = 3
        }
      }
      if ('PCO2' %in% filter_columns){
        if (valid_data$PCO2[n] == valid_data$PCO2[n-1]) {
          df$flag[df$timestamp_utc == current_row$timestamp_utc] = 2
          df$flag_flatline[df$timestamp_utc == current_row$timestamp_utc] = 2 # 2 in a row, SUSPECT
          df$flag_flatline_pco2[df$timestamp_utc == current_row$timestamp_utc] = 2
        }
        if (valid_data$PCO2[n] == valid_data$PCO2[n-1] & valid_data$PCO2[n] == valid_data$PCO2[n-2]) {
          df$flag[df$timestamp_utc == current_row$timestamp_utc] = 3
          df$flag_flatline[df$timestamp_utc == current_row$timestamp_utc] = 3 # 3 in a row, FAIL
          df$flag_flatline_pco2[df$timestamp_utc == current_row$timestamp_utc] = 3
        }
      }
      if ('DO' %in% filter_columns){
        if (valid_data$DO[n] == valid_data$DO[n-1]) {
          df$flag[df$timestamp_utc == current_row$timestamp_utc] = 2
          df$flag_flatline[df$timestamp_utc == current_row$timestamp_utc] = 2 # 2 in a row, SUSPECT
          df$flag_flatline_do[df$timestamp_utc == current_row$timestamp_utc] = 2
        }
        if (valid_data$DO[n] == valid_data$DO[n-1] & valid_data$DO[n] == valid_data$DO[n-2]) {
          df$flag[df$timestamp_utc == current_row$timestamp_utc] = 3
          df$flag_flatline[df$timestamp_utc == current_row$timestamp_utc] = 3 # 3 in a row, FAIL
          df$flag_flatline_do[df$timestamp_utc == current_row$timestamp_utc] = 3
        }
      }
    }
    # Turn all remaining flag_flatlines (=4 or =5) into = 1 (PASS)
    flag_flatline4 = which(df$flag_flatline == 4)
    df$flag_flatline[flag_flatline4] = 1
    # df$flag_flatline_ph[which(df$flag_flagline_ph == 5)] = 1
    # df$flag_flatline_temp[which(df$flag_flagline_temp == 5)] = 1
    # df$flag_flatline_sal[which(df$flag_flagline_sal == 5)] = 1
    # df$flag_flatline_pco2[which(df$flag_flagline_pco2 == 5)] = 1
    
    #!!!jan2025# 
    
    #!!!jan2025# Here I remember feeling like this was incomplete. Since we aren't planning to do MV tests, it shouldn't matter. anyways. 
    ## MULTI-VARIATE TEST: Checking a pair of rate-of-change tests for pH and dissolved oxygen (DO)
    # 1 = PASS
    # 2 = SUSPECT: pH(n) fails the pH rate of change, but DO(n) does not exceed the rate of change
    # Because of the dynamic nature of pH, no fail flag is identified for this test
    valid_data = df %>%
      filter(flag!=3) 
    for (q in 1:nrow(valid_data)) {
      current_row = valid_data[q,]
      if ('PH' %in% filter_columns & 'DO' %in% filter_columns) {
        if (valid_data$flag_roc_ph[q] == 2 & valid_data$flag_roc_do[q] == 1) {
          df$flag_mv_ph_do[df$timestamp_utc == current_row$timestamp_utc] = 2
          df$flag[df$timestamp_utc == current_row$timestamp_utc] = 2
        } else {
          df$flag_mv_ph_do[df$timestamp_utc == current_row$timestamp_utc] = 1
        }
      }
    }
    
    # Create flags specific to each variable:
    for (r in 1:nrow(df)) {
      if ('PH' %in% filter_columns) {
        if (df$flag_roc_ph[r]==3|df$flag_flatline_ph[r]==3|df$flag_spike_ph[r]==3) {
          df$flag_ph[r] = 3
        } else if (df$flag_roc_ph[r]==2|df$flag_flatline_ph[r]==2|df$flag_spike_ph[r]==2) {
          df$flag_ph[r] = 2
        } else {
          df$flag_ph[r] = 1
        }
      }
      if ('TEMPERATURE' %in% filter_columns) {
        if (df$flag_roc_temp[r]==3|df$flag_flatline_temp[r]==3|df$flag_spike_temp[r]==3) {
          df$flag_temp[r] = 3
        } else if (df$flag_roc_temp[r]==2|df$flag_flatline_temp[r]==2|df$flag_spike_temp[r]==2) {
          df$flag_temp[r] = 2
        } else {
          df$flag_temp[r] = 1
        }
      }
      if ('SALINITY' %in% filter_columns) {
        if (df$flag_roc_sal[r]==3|df$flag_flatline_sal[r]==3|df$flag_spike_sal[r]==3) {
          df$flag_sal[r] = 3
        } else if (df$flag_roc_sal[r]==2|df$flag_flatline_sal[r]==2|df$flag_spike_sal[r]==2) {
          df$flag_sal[r] = 2
        } else {
          df$flag_sal[r] = 1
        }
      }
      if ('PCO2' %in% filter_columns) {
        if (df$flag_roc_pco2[r]==3|df$flag_flatline_pco2[r]==3|df$flag_spike_pco2[r]==3) {
          df$flag_pco2[r] = 3
        } else if (df$flag_roc_pco2[r]==2|df$flag_flatline_pco2[r]==2|df$flag_spike_pco2[r]==2) {
          df$flag_pco2[r] = 2
        } else {
          df$flag_pco2[r] = 1
        }
      }
      if ('DO' %in% filter_columns) {
        if (df$flag_roc_do[r]==3|df$flag_flatline_do[r]==3|df$flag_spike_do[r]==3) {
          df$flag_do[r] = 3
        } else if (df$flag_roc_do[r]==2|df$flag_flatline_do[r]==2|df$flag_spike_do[r]==2) {
          df$flag_do[r] = 2
        } else {
          df$flag_do[r] = 1
        }
      }
      
    }
    
    ## PASS all remaining data as Flag 1: rename all remaining '5' (not-evaluated) flag to '1' (pass)
    flag5 = which(df$flag == 5)
    df$flag[flag5] = 1
    flagged_dataframes[[paste0(site_names[i])]] = df  
  }
  return(flagged_dataframes)
}
