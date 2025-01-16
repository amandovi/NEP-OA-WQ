# Functions used in conjunction with nep_oa_master.R script
# Function descriptions:
#   read_first_csv(filepath): reads in first csv in filepath, and returns it as a dataframe
#   read_all_csv(filepath): reads in all files in filepath, and returns them as a list of dataframes
#   filter_data(data,filter_columns,threshold): filters a single dataframe on a list of columns where values in the 
#       ..columns must be above a given threshold
#   filter_data_multi(all_dataframes,filter_columns_list,threshold): filters multiple dataframes (the list above).. 
#       .. on a list of columns where values in the columns must be > a threshold


# Created by Andrew Mandovi (AWM) 12/14/2023
# Last update: 2/26/2024 (AWM)


# To load and process data from NEP OA Monitoring to calculate Omega and compare against state WQ standards
library(seacarb)
library(tidyverse)
library(dplyr)
library(zoo)

filepath = 'C:/Users/amandovi/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/Data/NEP_Data/Cleared'
setwd(filepath)
file_names = list.files(path=filepath,pattern='*_Data.csv')
TM_raw_data=read.csv(file_names[length(file_names)])
TM_raw_data = read.csv('C:/Users/amandovi/OneDrive - Environmental Protection Agency (EPA)/Profile/Desktop/COPY_of_Acidification Monitoring/Garibaldi/Fairchild et al 2022 OA Monitoring Manuscript/Garibaldi_SeapHOx_AWM.csv',header=TRUE)

setwd('C:/Users/amandovi/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/R')

#### Function to read in first csv in filepath as a data frame ####
read_first_csv = function(filepath) {
  setwd(filepath)
  file_names = list.files(path=filepath,pattern='*Data.csv',full.names=TRUE)
  print(file_names)
  if (length(file_names) == 0) {
    stop('No CSVs found in specified directory')
  }
  raw_data = read.csv(file_names[1],header=TRUE)
  return(raw_data)
}
testdata1=read_first_csv(filepath) # run function

#### ((1)) Function to read in ALL files at designated filepath, and compile them in a list ####
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

# from old:
# datetime_string=paste(TB_qa_data$DATE_UTC,TB_qa_data$TIME_UTC,sep=' ')
# TB_qa_data$timestamp_utc = as.POSIXct(strptime(datetime_string,format='%Y%m%d %H:%M',tz='UTC'))
# TB_qa_data$month = month(as.POSIXlt(TB_qa_data$timestamp_utc, format='%Y%m%d %H:%M')) # add month column



#### Functions to filter data after pulling it in (OLD - will likely not use) ####
filter_data = function(data,filter_columns,threshold) {
  # filters a single data frame ('data') where values are > 0 for all 'filter_columns'
  if (all(filter_columns %in% colnames(data))) {
    df = data[rowSums(data[,filter_columns,drop=FALSE]>threshold,na.rm=TRUE)==length(filter_columns),]
  } else{
    warning(paste('Some columns in filter_columns do not exist in the data.'))
  }
  filtered_data = df
  return(filtered_data)
}


filter_data_multi = function(all_dataframes, filter_columns_list, threshold) {
  # filters multiple data frames (a list of DFs) using a list of columns to filter by, 
  # corresponding to each respective data frame where data > a threshold value (must be a number)
  filtered_dataframes = list()
  for (i in seq_along(all_dataframes)){
    df = all_dataframes[[i]]
    if (!is.null(filter_columns_list[[i]])) {
      filter_columns = filter_columns_list[[i]]
      # check if specified columns exist in data frame:
      if (all(filter_columns %in% colnames(df))) {
        df = df[rowSums(df[,filter_columns,drop=FALSE]>threshold, na.rm=TRUE) == length(filter_columns),]
        mooring_name = first(df$MOORING_NAME)
        # name = if(!is.na(mooring_name)) {
        #   paste0('filtered_',mooring_name)
        # } else {
        #   paste0('unnamed_filtered_dataframe',i)
        # }
        # return(setNames(list(df), name))
      } else {
        warning(paste('some columns in filter_columns_list[[',i,']] do not exist in data frame. Skipping filter for this data frame'))
      }
    }
    filtered_dataframes[[paste0('filtered_dataframe',i)]] = df
  }
  return(filtered_dataframes)
}

#### INPUTS for functions: ####
nep_sites = c('Tampa Bay','Coastal Bend','Casco Bay','Barnegat Bay','Tillamook Bay')

filter_columns_list = list(
  c('TEMPERATURE','SALINITY','PH','PCO2','DO'),  # Tampa Bay
  c('TEMPERATURE','SALINITY','PH','PCO2'),#,'DO'),             \ # Coastal Bend (aka Mission-Aransas aka Corpus Christi)
  c('TEMPERATURE','SALINITY','PH','PCO2','DO'),  # Casco Bay (also has DO!!!)
  c('TEMPERATURE','SALINITY','PH','DO'),                              # Barnegat Bay
  # c('TEMPERATURE','SALINITY','PH','pCO2_WATER','DISSOLVED.OXYGEN'), # Tillamook Bay
  # c('TEMPERATURE','SALINITY','PH','pCO2_WATER'),                    # San Fran (1m)
  # c('TEMPERATURE','SALINITY','PH','pCO2_WATER'),                    # San Fran (17m)
  # c('TEMPERATURE','pCO2_WATER'),                    # Santa Monica (15m)
  # c('TEMPERATURE','pCO2_WATER'),                     # Santa Monica (60m)
  c('TEMPERATURE','SALINITY','PH','DO')                       # Tillamook (GOOD)
)

# Testing data filtering:
# filtered_dataframes2 = filter_data_multi(all_data_list,filter_columns_list,0) # run function
nep_carb_flags = list('21','21','21','21','8')
carb_flag_mapping = list(
  # ALL variables within data files must correspond to following nomenclature (shorthand, all CAPS):
  # PH, CO2, HCO3, PCO2, DIC, ALK
  '1' = list(var1='PH',var2='CO2'),
  '2' = list(var1='CO2',var2='HCO3'),
  '3' = list(var1='CO3',var2='CO3'),
  '4' = list(var1='CO2',var2='ALK'),
  '5' = list(var1='CO2',var2='DIC'),
  '6' = list(var1='PH',var2='HCO3'),
  '7' = list(var1='PH',var2='CO3'),
  '8' = list(var1='PH',var2='ALK'),
  '9' = list(var1='PH',var2='DIC'),
  '10' = list(var1='HCO3',var2='CO3'),
  '11' = list(var1='HCO3',var2='ALK'),
  '12' = list(var1='HCO3',var2='DIC'),
  '13' = list(var1='CO3',var2='ALK'),
  '14' = list(var1='CO3',var2='DIC'),
  '15' = list(var1='ALK',var2='DIC'),
  '21' = list(var1='PCO2',var2='PH'),
  '22' = list(var1='PCO2',var2='HCO3'),
  '23' = list(var1='PCO2',var2='CO3'),
  '24' = list(var1='PCO2',var2='ALK'),
  '25' = list(var1='PCO2',var2='DIC')
)

time_format_list = c(
  '%Y%m%d %H:%M', # Tampa Bay
  '%Y%m%d %H:%M', # Coastal Bend
  '%Y%m%d %H:%M', # Casco Bay
  '%Y%m%d %H:%M', # Barnegat Bay
  '%m/%d/%Y %H:%M' # Tillamook
)
timezone_list = list( # Possible timezones: 'UTC' 'America/New_York' 'America/Chicago' 'America/Denver' 'America/Los_Angeles'
  'UTC',
  'UTC',
  'UTC',
  'UTC',
  'America/Los_Angeles'
)
site_names = c('Tampa Bay','Coastal Bend','Casco Bay','Barnegat Bay','Tillamook Bay')
date_time_separate_list = c(1,1,1,1,0)

# Function to convert month to season
get_season <- function(m) {
  if (m %in% c(12,1,2)) {
    return('Winter')
  } else if (m %in% c(3,4,5)) {
    return('Spring')
  } else if (m %in% c(6,7,8)) {
    return('Summer')
  } else {
    return('Autumn')
  }
}
# TB_qa_data = TB_qa_data |>
#   mutate(season = sapply(month,get_season)) # add season column

#### Function to filter selected data frames from the list (OLD, Work-in-progress / backlog it) ####
filter_select_data = function(dataframes,filter_columns_list,threshold,selected_frames) {
  filtered_dataframes = list()
  for (i in seq_along(dataframes)){
    df = dataframes[[i]]
    if (i %in% selected_frames) {
      
      # check if columns exist in dataframe:
      if (!is.null(filter_columns_list[[i]])) {
        filter_columns = filter_columns_list[[i]]
        if (all(filter_columns %in% colnames(df))) {
          df_filtered = df |>
            filter(across(all_of(filter_columns), ~ . > threshold))
          return(df_filtered)
        } else {
          warning(paste('Some columns are not in filter_columns_list[[',i,']], skipping this data frame'))
          return(NULL)
        }
      } 
    }
  }
}

filtered_dataframes_test = filter_select_data(all_data_list,filter_columns_list,0,selected_frames)


for (i in seq_along(all_data_list)) {
  print(i)
} 

selected_frames = c(1,2,3,4,10)
df = all_data_list[[1]]
filter_columns = filter_columns_list[[1]]
if (all(filter_columns %in% colnames(df))) {
  df = df[rowSums(df[,filter_columns,drop=FALSE]>0, na.rm=TRUE)==length(filter_columns),]
} else {
  warning(paste('some columns do not exist'))
}




#### ((2)) TIMESTAMP Function to convert time to a universal UTC timestamp column for each data file read in: ####
timestamp_all_data = function(ALL_DATA_LIST,TIMEZONE_LIST,DATE_TIME_SEPARATE_YN,TIME_FORMAT_LIST,SITE_NAMES) {
  all_data_timestamped = list()
  for (i in seq_along(ALL_DATA_LIST)) { # for every i'th data frame in ALL_DATA_LIST
    df = ALL_DATA_LIST[[i]]
    if (DATE_TIME_SEPARATE_YN[i] == 1) { 
      # if the date and time are in separate columns:
      df$datetime_string=paste(df$DATE,df$TIME,sep=' ') # combine date and time into a single string  
      df$timestamp_local = as.POSIXct(strptime(df$datetime_string,format=TIME_FORMAT_LIST[[i]],tz=TIMEZONE_LIST[[i]]))
      df$timestamp_utc = with_tz(df$timestamp_local, tzone='UTC')
      
      # df$timestamp_local=strptime(df$TIMESTAMP,format='%m/%d/%Y %H:%M', tz=TIMEZONE_LIST[[i]])  #! Needs naming convention
      # df$timestamp_utc = with_tz(df$timestamp_local,tzone='UTC')
    } else {
      # if date and time are NOT in seaparate columns:
      df$timestamp_local=as.POSIXct(strptime(df$TIMESTAMP,format=TIME_FORMAT_LIST[[i]], tz=TIMEZONE_LIST[[i]]))  
      df$timestamp_utc = with_tz(df$timestamp_local,tzone='UTC') # works, but clunky to manually add time 
    }
    df = arrange(df,timestamp_utc)
    df = df %>%
      mutate(interval = as.numeric(difftime(timestamp_utc,lag(timestamp_utc),units='secs'))) 
    df = df %>%
      mutate(SEASON = sapply(month(timestamp_utc),get_season))
    all_data_timestamped[[paste0(SITE_NAMES[i])]] = df  
  }
  return(all_data_timestamped)
}
#### ((3))  (SINGLE) DATA FLAGGING function to perform flag/QA testing on a SINGLE DATA FRAME: ####

#### ((3))  MULTI DATA FLAGGING function to take list of data frames and perform Flag/QA Testing on specified columns: ####
flag_data_multi = function(all_dataframes, filter_columns_list, site_names) {
  # filters multiple data frames (a list of DFs) using a list of columns to filter across, applies a data flagging system, and converts timestamps
  # to the nearest second
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
  
  ##
  
  flagged_dataframes = list()
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
    # Gross range test: pH between 5-9 (FAIL = flag 3)
    if ('PH' %in% filter_columns) {
      bad_ph_min = which(df$PH < ph_min)
      bad_ph_max = which(df$PH > ph_max)
      df$flag[bad_ph_min] = 3
      df$flag[bad_ph_max] = 3
    }
    # Gross range test: Temperature between -1 and 30 (FAIL = flag 3)
    if ('TEMPERATURE' %in% filter_columns) {
      bad_temp_min = which(df$TEMPERATURE < temp_min)
      bad_temp_max = which(df$TEMPERATURE > temp_max)
      df$flag[bad_temp_min] = 3
      df$flag[bad_temp_max] = 3
    }
    # Gross range test: Salinity between 0 and 40 (FAIL = flag 3)
    if ('SALINITY' %in% filter_columns) {
      bad_sal_min = which(df$SALINITY < sal_min)
      bad_sal_max = which(df$SALINITY > sal_max)
      df$flag[bad_sal_min] = 3
      df$flag[bad_sal_max] = 3
    }
    # Gross range test: pCO2 between 0 and 1500 (FAIL = flag 3)
    if ('PCO2' %in% filter_columns) {
      bad_pco2_min = which(df$PCO2 < pco2_min)
      bad_pco2_max = which(df$PCO2 > pco2_max)
      df$flag[bad_pco2_min] = 3
      df$flag[bad_pco2_max] = 3
    }
    # Gross range test: DO between 0 and 20 (FAIL = flag 3)
    if ('DO' %in% filter_columns) {
      bad_do_min = which(df$DO < do_min)
      bad_do_max = which(df$DO > do_max)
      df$flag[bad_do_min] = 3
      df$flag[bad_do_max] = 3
    }
    
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
    # flag3 = which(df$flag==3)
    # df$flag_RoC[flag3] = 5  # makes all points which failed previous test a '5' bc they will not be evaluated
    # df$which_roc_var =rep(0,nrow(df)) # 1 = pH, 2 = Temp, 3 = Sal, 4 = pCO2
    # filter out data which failed gross range test:
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
    
    
    ## SPIKE TEST: 
    # Create a new flag column (flag_spike)
    # 1 PASS
    # 2 SUSPECT
    # 3 FAIL
    # 5 NOT EVALUATED (due to failing a previous test)
    df$flag_spike = rep(4,nrow(df)) # make flag_spike all = 4 (Not YET evaluated)
    flag3 = which(df$flag==3)        # index of where flag==3 (Previous FAIL)
    df$flag_spike[flag3] = 5         # make flag_spike = 5 (NOT EVALUATED), where flag==3
    df$flag_spike[1] = 5             # set first flag_spike to 5 (NOT EVALUATED) - should it be suspect though?
    df$flag_spike[nrow(df)] = 5      # set last flag_spike to 5 (NOT EVALUATED)  - should it be suspect though?
    valid_data = df %>%
      filter(flag!=3)
    # to this point:
    # flag_spike values: 4 (not YET evaluated) and 5 (will NOT be evaluated)
    # flag values: 2 (suspect in previous test), 3 (failed previous test), and 5 (thus far, passed all tests)
    for (m in 2:nrow(valid_data)-1) { # exclude first and last rows of dataset
      current_row = valid_data[m,]
      window_start = current_row$timestamp_utc - time_window # 24 hours prior to timestamp
      window_end = current_row$timestamp_utc
      window_data = valid_data %>%
        filter(timestamp_utc >= window_start & timestamp_utc < window_end)
      if ('PH' %in% filter_columns) {
        if (!is.na(current_row$ph_24h_sd)) {
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
    ##
    
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

#~# testing area:
df = tm_output
for (r in 1:5) {
  if ('PH' %in% filter_columns) {
    if (df$flag_roc_ph[r]==3|df$flag_flatline_ph[r]==3|df$flag_spike_ph[r]==3) {
      print('pH test: 3')
      df$flag_ph[r] = 3
    } else if (df$flag_roc_ph[r]==2|df$flag_flatline_ph[r]==2|df$flag_spike_ph[r]==2) {
      df$flag_ph[r] = 2
      print('pH test: 2')
    } else {
      df$flag_ph[r] = 1
      print('pH test: 1')
    }
  }
}

tm_output$test_col = rep(0,nrow(tm_output))
tm_output |>
  filter(!is.na(timestamp_utc))
  for (r in 1:nrow(tm_output)) {
    current_row = tm_output[r,]
    tm_output$test_col[tm_output$timestamp_utc==current_row$timestamp_utc] = tm_output$flag_roc_ph[tm_output$timestamp_utc==current_row$timestamp_utc]
  }
for (r in 1:15) {
  print(tm_output$flag_roc_ph[r])
  print(tm_output$flag_flatline_ph[r])
  print(tm_output$flag_spike_ph[r])
  print('--- ---')
}

print(tm_output$flag_ph[1:15])
#~#

#### ((4))  CARBONATE CHEMISTRY via seacarb package to calculate full carb. chem. (e.g. OmegaAragonite) on each dataframe: ####
carb_data_multi = function(all_dataframes, nep_carb_flags, carb_flag_mapping, site_names) {
  carb_dataframes = list()
  error_dataframes = list()
  for (i in seq_along(all_dataframes)){ # 'i' loops through the various data frames
    df = all_dataframes[[i]]
    carb_flag = nep_carb_flags[[i]]
    var_match = carb_flag_mapping[[carb_flag]]
    var_1 = df[[var_match$var1]]  #!! error here: "attempt to select less than one element in get1index"...
    var_2 = df[[var_match$var2]]
    carb_result = carb(flag=carb_flag,var1=var_1,var2=var_2,S=df$SALINITY,T=df$TEMPERATURE)
    carb_errors = errors(flag=carb_flag,var1=var_1,var2=var_2,S=df$SALINITY,T=df$TEMPERATURE)
    carb_dataframes[[paste0(site_names[i])]] = carb_result
    error_dataframes[[paste0(site_names[i])]] = carb_errors
  }
  carb_error_dataframes = c(carb_dataframes,error_dataframes)
  
}
tb_seacarb = carb_data_multi(flagged_data_list[[1]],nep_carb_flags[[1]],carb_flag_mapping[[1]],site_names[[1]])
tb_carb_error = carb_data_multi(flagged_data_list[[1]],nep_carb_flags[[1]],carb_flag_mapping[[1]],site_names[[1]])

# OLD Carb function:
carb_multi_data = function(dataframes, flag_list, carb_flag_mapping){
  carb_dataframes = list()
  for (i in seq_along(dataframes)) {
    df = dataframes[[i]] 
    df_flag = flag_list[[i]] # get corresponding flag for this data frame
    if (all(c('SALINITY','TEMPERATURE') %in% colnames(df))) { # check if required columns present
      if (df_flag %in% names(carb_flag_mapping)) {
        var_mapping = carb_flag_mapping[[df_flag]]
        var_1 = df[[var_mapping$var1]]
        var_2 = df[[var_mapping$var2]]
        # run carb() with extracted variables:
        carb_result = carb(flag=df_flag,var1=var_1,var2=var_2,S=df$SALINITY,T=df$TEMPERATURE)
        # result_name = paste0(df$MOORING_NAME,'_Carb')
        # return(setNames(list(carb_result),result_name))
      } else {
        warning(paste('No mapping found for flag',df_flag,'. Skipping carb() for this data frame.'))
        return(NULL)
      }
    } else {
      warning(paste('Some required columns are missing in filtered_dataframes[[',i,']]. Skipping carb() for this data frame.'))
      return(NULL)
    }
    carb_dataframes[[paste0('carb_',i)]] = carb_result
  }
  return(carb_dataframes)
} 
carb_data = carb_multi_data(filtered_dataframes2,flag_list_nep,carb_flag_mapping)


#### Run Functions: ####
all_data_list = read_all_csv(filepath) # run function
timestamped_data_list = timestamp_all_data(all_data_list,timezone_list,date_time_separate_list,time_format_list,site_names)
flagged_data_list = flag_data_multi(timestamped_data_list,filter_columns_list,nep_sites) # run function
tb_output = flagged_data_list[[1]]
tm_output = flagged_data_list[[5]]


test_flag_list = flag_data_multi(timestamped_data_list[1:2],filter_columns_list[1:2],nep_sites[1:2])
tb_output = test_flag_list[[1]]
cb_output = test_flag_list[[2]]


tm_output = flag_data_multi(timestamped_data_list[5],filter_columns_list[5],nep_sites[5])

tb_output = flag_data_multi(timestamped_data_list[1],filter_columns_list[1],nep_sites[1])
tb_output = tb_output[[1]]
# tb_output = test_flagged_data[[1]]


tb_output_23 = tb_output |>
  filter(flag_ph==2|flag_ph==3)







# TESTING: Rate of change
# CURRENT ISSUE: TEST IS FAILING EVEN THOUGH IT SHOULD BE PASSING. SOMETHING WRONG WITH THE IF() STATEMENT WITH EACH INDIVIDUAL VARIABLE
starttime = Sys.time()
df = tb_output
# Create a NEW flag column (flag_RoC) which indicates how the data performs on the rate of change test:
# 1 PASS
# 2 SUSPECT: Fails rate of change test -> SUSPECT in 'flag' column
# 5 TEST NOT PERFORMED either due to failed prior test or insufficient 24-hour data
df$flag_RoC = df$flag
flag3 = which(df$flag==3)
df$flag_RoC[flag3] == 5  # makes all flag_RoC = 5 (not evaluated) where the data failed a previous test
df$roc_flag_ph = rep(0,nrow(df))
df$roc_flag_temp = rep(0,nrow(df))
df$roc_flag_sal = rep(0,nrow(df))
df$roc_flag_pco2 = rep(0,nrow(df))

# filter out data which failed gross range test:
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
  } else {
    sd_PH = sd(window_data$PH)
    mean_PH = mean(window_data$PH)
    df$PH_24h_sd[df$timestamp_utc == current_row$timestamp_utc] = sd_PH
    df$PH_24h_mean[df$timestamp_utc == current_row$timestamp_utc] = mean_PH
    if (current_row$PH > mean_PH+3*sd_PH || 
        current_row$PH < mean_PH-3*sd_PH) {
      df$flag[df$timestamp_utc == current_row$timestamp_utc] = 2
      df$flag_RoC[df$timestamp_utc == current_row$timestamp_utc] = 2
      df$roc_flag_ph[df$timestamp_utc == current_row$timestamp_utc] = 1
    }
    sd_temp = sd(window_data$TEMPERATURE)
    mean_temp = mean(window_data$TEMPERATURE)
    df$temp_24h_sd[df$timestamp_utc == current_row$timestamp_utc] = sd_temp
    df$temp_24h_mean[df$timestamp_utc == current_row$timestamp_utc] = mean_temp
    if (current_row$TEMPERATURE > mean_temp+3*sd_temp || 
        current_row$TEMPERATURE < mean_temp-3*sd_temp) {
      df$flag[df$timestamp_utc == current_row$timestamp_utc] = 2
      df$flag_RoC[df$timestamp_utc == current_row$timestamp_utc] = 2
      df$roc_flag_temp[df$timestamp_utc == current_row$timestamp_utc] = 1
    }
    sd_sal = sd(window_data$SALINITY)
    mean_sal = mean(window_data$SALINITY)
    df$sal_24h_sd[df$timestamp_utc == current_row$timestamp_utc] = sd_sal
    df$sal_24h_mean[df$timestamp_utc == current_row$timestamp_utc] = mean_sal
    if (current_row$SALINITY > mean_sal+3*sd_sal || 
        current_row$SALINITY < mean_sal-3*sd_sal) {
      df$flag[df$timestamp_utc == current_row$timestamp_utc] = 2
      df$flag_RoC[df$timestamp_utc == current_row$timestamp_utc] = 2
      df$roc_flag_sal[df$timestamp_utc == current_row$timestamp_utc] = 1
    }
    sd_pco2 = sd(window_data$PCO2)
    mean_pco2 = mean(window_data$PCO2)
    df$pco2_24h_sd[df$timestamp_utc == current_row$timestamp_utc] = sd_pco2
    df$pco2_24h_mean[df$timestamp_utc == current_row$timestamp_utc] = mean_pco2
    if (current_row$PCO2 > mean_pco2+3*sd_pco2 || 
        current_row$PCO2 < mean_pco2-3*sd_pco2) {
      df$flag[df$timestamp_utc == current_row$timestamp_utc] = 2
      df$flag_RoC[df$timestamp_utc == current_row$timestamp_utc] = 2
      df$roc_flag_pco2[df$timestamp_utc == current_row$timestamp_utc] = 1
    }
  }
}
endtime = Sys.time()
duration = endtime-starttime
print(duration)

df = cb_output
# TESTING: SPIKE TEST
# 1 PASS
# 2 SUSPECT
# 3 FAIL
# 5 NOT EVALUATED
df$flag_spike = rep(4,nrow(df)) # make flag_spike all = 99
flag3 = which(df$flag==3)        # index of where flag==3 (Previous FAIL)
df$flag_spike[flag3] = 5         # make flag_spike = 5, where flag==3
df$flag_spike[1] = 5             # set first flag_spike to 2 
df$flag_spike[nrow(df)] = 5      # set last flag_spike to 2 
# to this point:
# flag_spike values: 4 (not YET evaluated) and 5 (will NOT be evaluated)
# flag values: 3 (failed previous test), 2 (suspect in previous test), and 1 (passed all tests)
## FOR DUMMY DATA: CONVERT all '1' to '5'
flag1 = which(df$flag==1)
df$flag[flag1]=5
## Now, flag contains 2, 3, and 5s

# dummy thresholds: sd_dev x1.5 and x3
THRESHOLD_LOW_PH = mean(df$PH_24h_sd,na.rm=TRUE)*1.5
THRESHOLD_HIGH_PH = mean(df$PH_24h_sd,na.rm=TRUE)*3
valid_data = df %>%
  filter(flag_spike!=5) # filter out flag_spike where it will not be evaluated
for (i in 2:(nrow(valid_data)-1)) { # exclude first and last rows of dataset
  current_row = valid_data[i,]
  current_val = valid_data$PH[i]
  previous_val = valid_data$PH[i-1]
  next_val = valid_data$PH[i+1]
  # calculate spike reference:
  SPIKE_REF = (previous_val+next_val)/2
  spike = abs(current_val-SPIKE_REF)
  # compare vs thresholds:
  if (spike < THRESHOLD_LOW_PH) {
    df$flag_spike[df$timestamp_utc == current_row$timestamp_utc] = 1
  } else if (spike >= THRESHOLD_HIGH_PH) {
    df$flag_spike[df$timestamp_utc == current_row$timestamp_utc] = 3
  } else {
    df$flag_spike[df$timestamp_utc == current_row$timestamp_utc] = 2
  }
}
hist(df$flag_spike)



ggplot()+
  geom_point(data = df, aes(x=df$timestamp_utc,y=df$PH),color=df$flag)+
  labs(title='Tillamook pH',x='Date',y='pH')+
  scale_color_manual(values=c('black','red'),
                     labels=c('Passes','Fails rate of change'))+
  theme_bw()







#**

# testing point & line plots of pH data & 24-hour running averages
ggplot()+
  geom_point(data=test_tb,aes(x=timestamp_utc,y=PH_test))+
  geom_line(data=test_tb,aes(x=timestamp_utc,y=pH_24hr,color='red'))+
  theme_minimal()
  
# custom SD function
custom_sd = function(x) {
  if (any(!is.na(x))) {
    sd(x, na.rm=TRUE)
  } else {
    NA_real_
  }
}
test_tb = test_tb %>%
  mutate(rolling_sd = rollapply(test_tb$PH_test,width=1:min(24,n()),FUN=custom_sd,align='right',fill=NA))


test_tb = test_tb |>
  mutate(running_avg = if_else(flag == 1, rollapply(PH, width=1:min(24,nrow(PH)),FUN=mean,align='right',fill=NA),NA_real_))

df = flagged_dataframes[[5]]
filtered_df = df %>%
  filter(flag==1)
filtered_df = filtered_df %>%
  mutate(running_avg =  rollapply(PH, width=1:min(24,nrow(PH)),FUN=mean,align='right',fill=NA))
df = df %>%
  left_join(filtered_df[,c('timestamp_utc','running_avg')], by='timestamp_utc')







#### testing: QA and Data analysis ####

casco_data = all_data_list[[3]] # Casco bay test data
casco_filter_columns = c('TEMPERATURE','SALINITY','PH','PCO2')
casco_data$flag = rep(5,length(casco_data$TEMPERATURE))
if ('PH' %in% casco_filter_columns) {
  print('yes')
}

filtered_dataframes = filter_data_multi(all_data_list,filter_columns_list) # run function
TB_filtered = filter_data(TB_raw_data,c('TEMPERATURE','SALINITY','PH','PCO2','DISSOLVED.OXYGEN'))
TM_filtered = filter_data(TM_raw_data,c('TEMPERATURE','SALINITY','PH','PCO2'),0)
TM_filtered$site = 'Tillamook Bay'
TM_carb = carb(flag=8,var1=TM_filtered$PH,var2=TM_filtered$ALK/1000000,S=TM_filtered$SALINITY,T=TM_filtered$TEMPERATURE)
TM_carb = carb(flag='8',var1=TM_filtered$PH,var2=TM_filtered$ALK/1000000,S=TM_filtered$SALINITY,T=TM_filtered$TEMPERATURE) # verifying you can use '8' instead of 8 for flag



#### Testing: plotting ####
plot_combined_scatter = function(filtered_dataframes,column_names_list) {
  combined_df = do.call(rbind,Map(cbind,filtered_dataframes,site=seq_along(filtered_dataframes)))
  # check if columns exist in dataframes:
  if ('SALINITY' %in% colnames(combined_df) && 'ALKALINITY' %in% colnames(combined_df) && 'MOORING_NAME' %in% colnames(combined_df)) {
    ggplot(combined_df, aes(x=SALINITY, y=ALKALINITY, color=MOORING_NAME))+
      geom_point()+
      labs(title='alk-sal',x='sal',y='alk')+
      theme_minimal()
  } else {
    warning('Some required columns are missing')
  }
}  
plot_combined_scatter(filtered_dataframes)

plot_combined_scatter = function(filtered_dataframes) {
  
}
  




alksal_yn = readline(prompt='Create Alk-Sal plot from data? (y/n)')
if (alksal_yn == 'y') {
  plot_alksal(TM_carb,'Tillamook Bay')
}


TM_filtered |>
  ggplot(aes(SALINITY,ALKALINITY))+
  geom_point()+
  theme_minimal()

ggplot(TM_carb, aes(x=TM_filtered$SeaphoxAlk_Timestamp,y=TM_carb$OmegaAragonite))+
  geom_point()+
  theme_minimal()



plot_alksal = function(data,sitename) {
  # paste('Now plots will be created based on user input')
  # # alksal_yn = readline(prompt='Create Alk-Sal plot from data? (y/n)')

  data |>
    ggplot(aes(S,ALK*1e6))+
    geom_point()+
    labs(title=paste(sitename,'Alkalinity-Salinity Plot'),x='Salinity (g/kg)',y='Alkalinity (umol)')+
    theme_bw()
}
  # data|>
  #   ggplot(aes(S,ALK*1e6))+
  #   geom_point()+
  #   labs(title=paste(sitename,'Alkalinity-Salinity Plot'),x='Salinity (g/kg)',y='Alkalinity (umol)')+
  #   theme_bw()
  # oarsal_yn = readline(prompt='Create Omega Ar-Sal plot from data? (y/n)')
#   data |>
#     ggplot(aes(S,OmegaAragonite))+
#     geom_point()+
#     labs(title=paste(sitename,'Omega Aragonite-Salinity Plot'),x='Salinity (g/kg)',y='Omega Aragonite')+
#     theme_bw()
#   # timeseries_yn = readline(prompt='Create time series plots from data? (y/n)')
#   # if (timeseries_yn == 'y') {
#   #   
#   # }
# }

plot_data(TM_carb,'Tillamook Bay')


