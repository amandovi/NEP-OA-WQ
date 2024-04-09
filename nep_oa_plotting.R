library(ggplot2)
library(dplyr)
library(patchwork)



plot_ts_data = function(data, plot_var, time_bin, plot_var2=NULL, filter_flag=NULL, flag_num=NULL) {
  agg_data = data
  if (!is.null(filter_flag)) {
    agg_data = agg_data |>
      filter(filter_flag == flag_num)
  }
  df_ts = data %>%
    group_by(bin) %>%
    var = {{plot_var}} %>%
    summarize(mean_value = mean(var, na.rm=TRUE),
               min_value = min(var,na.rm=TRUE),
               max_value = max(var,na.rm=TRUE),
               q25 = quantile(var,0.25,na.rm=TRUE),
               q75 = quantile(var,0.75,na.rm=TRUE))
  
  return(df_ts)
  
}

ts = seq.POSIXt(as.POSIXlt(min(agg_data$timestamp_utc),as.POSIXlt(max(agg_data$timestamp_utc))),by='')

df_test = plot_function(tm_output[[1]],'TEMPERATURE','month')

agg_data = tm_output
# filtering for a specific flag:
agg_data_sus = agg_data |>
  filter(flag == 2)
agg_data_fail = agg_data |>
  filter(flag == 3)
agg_data_23 = agg_data |>
  filter(flag == 2 | flag == 3)

for (i in 1:nrow(agg_data)) {
  current_row = agg_data[i,]
  agg_data$flag_ph = max(agg_data$flag_roc_ph, agg_data$flag_spike_ph, agg_data$flag_spike_ph)
}


agg_data = agg_data[!is.na(agg_data$timestamp_utc),]
min_timestamp = min(agg_data$timestamp_utc)
max_timestamp = max(agg_data$timestamp_utc)
time_bin = 'month'
## To match the chosen time_bin to the proper format for plotting:
time_bin_list = c('year','month','day')
time_formats = c("%Y","%Y-%m","%Y-%j")
time_format_match = data.frame(time_bin_list,time_formats)
##
time_bins = seq(as.Date(min_timestamp),as.Date(max_timestamp),by=time_bin) # a list of months in YYYY-MM-DD format
all_bins = seq(as.Date(trunc(min_timestamp,time_bin)),as.Date(trunc(max_timestamp,time_bin)),by=time_bin) # a list of ALL time bins (incl. missing ones)
time_bins = format(all_bins,"%Y-%m")
all_bins = format(all_bins,"%Y-%m")
# 
agg_data$bin_month = as.Date(agg_data$timestamp_utc)
agg_data$bin_month = format(as.Date(agg_data$timestamp_utc),"%Y-%m")
unique_months = unique(format(as.Date(agg_data$timestamp_utc),"%Y-%m"))
agg_count = agg_data %>%
  count(bin_month, ph_flag) %>%
  complete(bin_month = all_bins,ph_flag,fill=list(n=0))

agg_summary = agg_data %>%
  group_by(bin_month) %>%
  summarize(mean_value = mean(PH, na.rm=TRUE),
            min_value = min(PH,na.rm=TRUE),
            max_value = max(PH,na.rm=TRUE),
            q25 = quantile(PH,0.25,na.rm=TRUE),
            q75 = quantile(PH,0.75,na.rm=TRUE),
            mean_value2 = mean(DO, na.rm=TRUE),
            min_value2 = min(DO,na.rm=TRUE),
            max_value2 = max(DO,na.rm=TRUE),
            q25_2 = quantile(DO,0.25,na.rm=TRUE),
            q75_2 = quantile(DO,0.75,na.rm=TRUE)) %>%
  ungroup()

# join summary statistics
agg_combined = left_join(agg_count,agg_summary, by='bin_month') 

# summary statistics:
do_threshold = 8
ph_threshold = 7
pct_do_below = sum(agg_data$DO < do_threshold, na.rm=TRUE) / sum(!is.na(agg_data$DO)) * 100
pct_ph_below = sum(agg_data$PH < ph_threshold, na.rm=TRUE) / sum(!is.na(agg_data$PH)) * 100

# Calculate porporations of pass/suspect/fail
# Adjust it to plot based on specific variable flags (ph flag instead of 'flag' if just looking at pH)


# agg_combined = merge(agg_summary,agg_count,by=c('bin_month','flag'))
# # create reference dataset with all months within range of data:
# reference_data = data.frame(bin_month = seq(min(as.Date(agg_data$timestamp_utc)),max(as.Date(agg_data$timestamp_utc)),by=time_bin))
# # left join the reference dataset with agg_combined:
# agg_plus_gaps = left_join(reference_data,agg_combined,by='bin_month')
# agg_plus_gaps$n[is.na(agg_plus_gaps$n)] = 0 # replace NA counts with 0s
# 
# agg_combined2 = agg_combined


# Time-series double-plot with quartile bars on top, no. of measurements (flag coded) on bottom
double_plot = FALSE
plot_2_var = 1
if (!is.null(plot_2_var)) {
  double_plot = TRUE
}

p1 = ggplot(agg_combined, aes(x=bin_month,group=1))+
  geom_point(aes(y=mean_value,color='Mean pH'),show.legend=TRUE)+
  geom_point(aes(y=mean_value2,color='Mean DO'),show.legend=TRUE)+
  geom_hline(yintercept=do_threshold,color='orange',show.legend =TRUE)+ # DO Threshold
  geom_hline(yintercept=ph_threshold,color='dodgerblue',show.legend =TRUE)+ # pH Threshold
  
  # geom_errorbar(aes(ymin=min_value,ymax=max_value,color='Min/Max pH'),width=0.3,alpha=0.1,size=1,show.legend=TRUE)+
  geom_errorbar(aes(ymin=q25,ymax=q75,color='25-75th %ile pH'),width=0.3,alpha=0.3,size=3,show.legend=TRUE)+
  
  # geom_errorbar(aes(ymin=min_value2,ymax=max_value2,color='Min/Max DO'),width=0.3,alpha=0.1,size=1,show.legend=TRUE)+
  geom_errorbar(aes(ymin=q25_2,ymax=q75_2,color='25-75th %ile DO'),width=0.3,alpha=0.3,size=3,show.legend=TRUE)+
  
  # geom_rect(aes(xmin=left_edge,xmax=right_edge,ymin=min_value,ymax=max_value),fill='blue',alpha=0.2)+
  # geom_rect(aes(xmin=bin_numeric-0.2,xmax=bin_numeric+0.2, ymin=q25,ymax=q75, fill='blue'),alpha=0.2)
  # geom_ribbon(aes(ymin=min_value,ymax=max_value), alpha=0.2)+
  labs(title='Tillamook Bay Measurements',y='pH & Dissolved Oxygen (mg/l)')+
  # scale_y_continuous(name='pH',limits=c(7,8.5),sec.axis=sec_axis(~.*1,name='Dissolved Oxygen'))+
  scale_color_manual(name='',values=c('Mean pH'='dodgerblue',
                                      # 'Min/Max pH'='blue',
                                      '25-75th %ile pH'='dodgerblue2',
                                      'Mean DO'='orange',
                                      # 'Min/Max DO'='orange',
                                      '25-75th %ile DO'='orange'))+
  theme(axis.title.x = element_blank(),axis.text.x = element_text(angle=90,hjust=1))
print(p1)

p2 = ggplot(agg_combined, aes(x=bin_month, y=n, fill=factor(flag)))+
  geom_bar(stat='identity',position='stack')+
  labs(x='month',y='No. Measurements')+
  theme_minimal()+
  scale_fill_manual(values=c('green4','gold2','red'))+
  theme(axis.text.x=element_blank())
  #scale_x_date(date_breaks = '1 month',date_labels='%Y-%m')
p2 = p2 + labs(fill='QC Flag')

# combine plots
combined_plot = (p1 / p2) + plot_layout(heights=c(4,1))
print(combined_plot)


## Plotting two variables along time series:
library(latticeExtra)
x = agg_combined$bin_month
var1 = agg_combined$mean_value
var2 = agg_combined$mean_value2
plot_data = data.frame(x,var1,var2)
obj1 = xyplot(var1 ~ x, plot_data, type='l',lwd=2)
obj2 = xyplot(var2 ~ x, plot_data, type='l',lwd=2)
doubleYScale(obj1,obj2, text=c('pH','DO'),add.ylab2 = TRUE)



#### OLD CODE: ####

start_month = min(unique_months, na.rm=TRUE)
end_month = max(unique_months, na.rm=TRUE)
all_months = seq(as.Date(start_month),as.Date(end_month),by='month')
all_months = format(all_months,"%Y-%m")



## PLOT ALL DATA, BINNED:
df = tm_output

# bin the data by the designated time bin:
time_bin = 'month'
df = df %>%
  mutate(bin=cut(timestamp_utc, breaks=time_bin))

plot_value = df$TEMPERATURE
df$TEMPERATURE[df$TEMPERATURE == -99 | is.na(df$TEMPERATURE)] = NA


# attempting to fill in missing months on x-axis so it displays a lack of data when there is none:
unique_months = unique(format(as.Date(df$timestamp_utc),"%Y-%m"))

df$bin = factor(df$bin, levels=unique(df_ts$bin))
df$bin_numeric = as.numeric(as.character(df_ts$bin))
df$bin_date = as.Date(df$bin)
df$bin_month = format(df$bin_date, "%Y-%m")


df$left_edge = df$bin_numeric - 0.2
df$right_edge = df$bin_numeric + 0.2



df$bin_month = factor(df$bin_month, levels=unique(df$bin_month))
unique_months = unique(format(as.Date(df$timestamp_utc),"%Y-%m"))
start_month = min(unique_months, na.rm=TRUE)
end_month = max(unique_months, na.rm=TRUE)
all_months = seq(as.Date(start_month),as.Date(end_month),by='month')
all_months = format(all_months,"%Y-%m")

# aggregate data for the top plot (time series line)
df_ts = df %>%
  group_by(bin) %>%
  summarize(mean_value = mean(TEMPERATURE, na.rm=TRUE),
            min_value = min(TEMPERATURE,na.rm=TRUE),
            max_value = max(TEMPERATURE,na.rm=TRUE),
            q25 = quantile(TEMPERATURE,0.25,na.rm=TRUE),
            q75 = quantile(TEMPERATURE,0.75,na.rm=TRUE))

df_ts$bin = factor(df_ts$bin, levels=unique(df_ts$bin))
df_ts$bin_numeric = as.numeric(as.character(df_ts$bin))
df_ts$left_edge = df_ts$bin_numeric - 0.2
df_ts$right_edge = df_ts$bin_numeric + 0.2
df_ts$bin_date = as.Date(df_ts$bin)
df_ts$bin_month = format(df_ts$bin_date, "%Y-%m")
df_ts$bin_month = factor(df_ts$bin_month, levels=unique(df_ts$bin_month))
all_bins = seq.Date(min(df_ts$bin_date,na.rm=TRUE),max(df_ts$bin_date,na.rm=TRUE),by=time_bin)
all_months = format(all_bins, "%Y-%m")



df_flag = df %>%
  group_by(bin, flag) %>%
  summarize(count=n())

#df_count not working
df_count = df %>%
  group_by(bin,flag) %>%
  summarize(count=n())

df_count = df %>%
  count(bin_month = format(as.Date(df_flag$bin),"%Y-%m"),flag)
  complete(bin_month = all_months, flag, fill=list(n=0))

df_flag$bin_date = as.Date(df_flag$bin)
df_flag$bin_month = format(df_flag$bin_date, "%Y-%m")
df_flag$bin_month = factor(df_flag$bin_month, levels=unique(df_flag$bin_month))

p1 = ggplot(df_ts, aes(x=bin_month, y=mean_value,group=1))+
  geom_point(aes(color='Mean'),show.legend=TRUE)+
  # geom_rect(aes(xmin=left_edge,xmax=right_edge,ymin=min_value,ymax=max_value),fill='blue',alpha=0.2)+
  geom_errorbar(aes(ymin=min_value,ymax=max_value,color='Min/Max'),width=0.3,alpha=0.3,size=1,show.legend=TRUE)+
  geom_errorbar(aes(ymin=q25,ymax=q75,color='25th-75th %ile'),width=0.3,alpha=0.5,size=3,show.legend=TRUE)+
  # geom_rect(aes(xmin=bin_numeric-0.2,xmax=bin_numeric+0.2, ymin=q25,ymax=q75, fill='blue'),alpha=0.2)
  # geom_ribbon(aes(ymin=min_value,ymax=max_value), alpha=0.2)+
  labs(title='Tillamook Bay Measurements',y='Temperature')+
  theme_minimal()+
  scale_color_manual(name='',values=c('Mean'='black','Min/Max' = 'red', '25th-75th %ile' = 'blue'))+
  theme(axis.title.x = element_blank(),axis.text.x = element_text(angle=90,hjust=1))

p2 = ggplot(df_count, aes(x=bin,y=count,fill=flag))+
  geom_bar(stat='identity',position='stack')+
  scale_fill_manual(values = c('green4','gold2','red'))+
  labs(x='Time',y='No. Measurements')+
  theme_minimal()+
  theme(axis.text.x = element_blank())

p2_old = p2_old + labs(fill='QC flag')

p2 = ggplot(df_flag, aes(x=bin_month, y=count, fill=factor(flag))) +
  geom_bar (stat='identity',position = 'stack')+
  scale_fill_manual(values = c('green4','gold2','red'))+
  labs(x='Time',y='No. Measurements')+
  theme_minimal()+
  theme(axis.text.x = element_blank())

p2 = p2 + labs(fill='QC Flag')

# combine plots
combined_plot = p1+p2+plot_layout(ncol=1, heights =c(4,1))
print(combined_plot)

##

## DIVING INTO SPECIFIC FLAGS:



# plotting function with color-coded threshold
make_scatter_plot = function(xdata,x_name,ydata,y_name,OAr_data,OAr_threshold,title) {
  ggplot()+
    geom_point(aes(xdata,ydata,color=ifelse(OAr_data<OAr_threshold,'red','black')),alpha=0.2)+
    labs(
      title=title,
      x=x_name,
      y=y_name
    )+
    scale_color_manual(values=c('black','red'),
                      labels=c('\u03a9Ar > 1.5','\u03a9Ar < 1.5'))+
    theme_bw()+
    theme(plot.title=element_text(hjust=0.5),
        legend.position='bottom',
        legend.title=element_blank())
}

# SAL over time
make_scatter_plot(TB_qa_data$timestamp_utc,'Time',TB_qa_data$SALINITY,'Salinity (g/kg)',TB_carb$OmegaAragonite,1.5,'Tampa Bay Salinity')

# ALK-SAL (doesn't work)
# make_scatter_plot(TB_qa_data$SALINITY,'Salinity (g/kg)',TB_carb$ALK*1e6,'Alkalinity (umol/kg)',TB_carb$OmegaAragonite,1.5,'Tampa Bay Sal-ALK')
                  