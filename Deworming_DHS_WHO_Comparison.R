############## PROJECT DESCRIPTION ################
# Project: Deworming in pre-school children Data Validation (Script 2 of 2)

# Coded by Ribhav Gupta (Stanford University)
# Email: rgupta97@stanford.edu 
# Last updated: 07/07/19 

#Purpose of File: Compares coverage and assesses correlations

# Files Required:
#   1. File generated from Deworming_WHO_DHS_SetUp.R

# **Note: Directory should be updated based on where files are saved.**

#File Outputs:
# 1. 
# 2. 

############ UPDATE BEFORE EACH RUN ##################

#UPDATE BASED ON YOUR DIRECTORY
setwd("/Users/rgupta97/Documents/Lab_Work")
library(readxl)
library(lubridate)
library(ggplot2)
library(extrafont)
library(WriteXLS)
library(stats)

#TO BE ADJUSTED
#VALUE FOR LENGTH OF RECALL (IDEAL RANGE 4-8 MONTHS) [BASE = 6]
recall_length <- 6
#DHS RESPONSES TO INCLUDE (BASED ON HOW MANY WAVES IT CAN FALL UNDER)
mda_allowed <- 3
#TRIALS TO RUN IN MDA_ALLOWED == 2
trial_total <- 25
#WHETHER DHS SURVEY ON FIRST OR LAST DAY OF MONTH (USING 28 AS PROXY FOR LAST DAY)
first_day <- TRUE
#ORIGINAL ALPHA VALUE FOR FITTING PROCESS
alpha_original <- 1.0
#WHETHER TO OVERRIDE ALPHA FITTING PROCESS [BASE = FALSE]
alpha_override <- FALSE
alpha_override_val <- 0.7
#WHO CYCLE COVERGAE CUT OFF VALUE [BASE = 0.95]
who_cov_lim <- 0.95
#Conducting Sensitivity Analysis
SA <- FALSE

reset_par <- function(){
  op <- structure(list(xlog = FALSE, ylog = FALSE, adj = 0.5, ann = TRUE,
                       ask = FALSE, bg = "transparent", bty = "o", cex = 1, cex.axis = 1,
                       cex.lab = 1, cex.main = 1.2, cex.sub = 1, col = "black",
                       col.axis = "black", col.lab = "black", col.main = "black",
                       col.sub = "black", crt = 0, err = 0L, family = "", fg = "black",
                       fig = c(0, 1, 0, 1), fin = c(6.99999895833333, 6.99999895833333
                       ), font = 1L, font.axis = 1L, font.lab = 1L, font.main = 2L,
                       font.sub = 1L, lab = c(5L, 5L, 7L), las = 0L, lend = "round",
                       lheight = 1, ljoin = "round", lmitre = 10, lty = "solid",
                       lwd = 1, mai = c(1.02, 0.82, 0.82, 0.42), mar = c(5.1, 4.1,
                                                                         4.1, 2.1), mex = 1, mfcol = c(1L, 1L), mfg = c(1L, 1L, 1L,
                                                                                                                        1L), mfrow = c(1L, 1L), mgp = c(3, 1, 0), mkh = 0.001, new = FALSE,
                       oma = c(0, 0, 0, 0), omd = c(0, 1, 0, 1), omi = c(0, 0, 0,
                                                                         0), pch = 1L, pin = c(5.75999895833333, 5.15999895833333),
                       plt = c(0.117142874574832, 0.939999991071427, 0.145714307397962,
                               0.882857125425167), ps = 12L, pty = "m", smo = 1, srt = 0,
                       tck = NA_real_, tcl = -0.5, usr = c(0.568, 1.432, 0.568,
                                                           1.432), xaxp = c(0.6, 1.4, 4), xaxs = "r", xaxt = "s", xpd = FALSE,
                       yaxp = c(0.6, 1.4, 4), yaxs = "r", yaxt = "s", ylbias = 0.2), .Names = c("xlog",
                                                                                                "ylog", "adj", "ann", "ask", "bg", "bty", "cex", "cex.axis",
                                                                                                "cex.lab", "cex.main", "cex.sub", "col", "col.axis", "col.lab",
                                                                                                "col.main", "col.sub", "crt", "err", "family", "fg", "fig", "fin",
                                                                                                "font", "font.axis", "font.lab", "font.main", "font.sub", "lab",
                                                                                                "las", "lend", "lheight", "ljoin", "lmitre", "lty", "lwd", "mai",
                                                                                                "mar", "mex", "mfcol", "mfg", "mfrow", "mgp", "mkh", "new", "oma",
                                                                                                "omd", "omi", "pch", "pin", "plt", "ps", "pty", "smo", "srt",
                                                                                                "tck", "tcl", "usr", "xaxp", "xaxs", "xaxt", "xpd", "yaxp", "yaxs",
                                                                                                "yaxt", "ylbias"))
  par(op)
}

#COUNTRY OF ANALYSIS
#country_name <- "Philippines"
country_name <- "Burundi"
#country_name <- "Myanmar"
#country_name <- "Indonesia"


#DATE OF ANALYSIS (format as MMDDYY)
date_analysis <- "070719"

#WHO DATA INPUT
#who_input_data <- read_excel("PHILIPPINES_070719.xlsx", sheet = "WHO Deworming Data")
who_input_data <- read_excel("BURUNDI_070719.xlsx", sheet = "WHO Deworming Data")
#who_input_data <- read_excel("MYANMAR_070719.xlsx", sheet = "WHO Deworming Data")
#who_input_data <- read_excel("INDONESIA_070719.xlsx", sheet = "WHO Deworming Data")

#DHS DATA INPUT
#dhs_input_data <- read_excel("PHILIPPINES_070719.xlsx", sheet = "DHS Deworming Data")
dhs_input_data <- read_excel("BURUNDI_070719.xlsx", sheet = "DHS Deworming Data")
#dhs_input_data <- read_excel("MYANMAR_070719.xlsx", sheet = "DHS Deworming Data")
#dhs_input_data <- read_excel("INDONESIA_070719.xlsx", sheet = "DHS Deworming Data")

#Creates date column for each DHS dataset in form (year-month-day)
if(first_day){
  dhs_input_data$date <- with(dhs_input_data, ymd(sprintf('%04d%02d%02d', year_dhs, month_dhs, 01)))
}
if(!first_day){
  dhs_input_data$date <- with(dhs_input_data, ymd(sprintf('%04d%02d%02d', year_dhs, month_dhs, 28)))
}

########## LIMIT WHO COVERAGE ##########
#Sets WHO coverage limit at 100%
who_data <- who_input_data
for(i in 1:nrow(who_data)){
  if(who_data$coverage_who[i] > 1){
    who_data$coverage_who[i] <- 1
  }
}
who_preAna_data <- who_data


########### DEMOGRAPHICS ANALYSIS ###########

#Counts possible cycles per DHS entry & Generates demographic information
country_names <- c()
cycleNum <- c()
total_responses <- c()
dhs_data <- dhs_input_data
who_data <- who_preAna_data
below_cov_lim <- who_data$coverage_who <= who_cov_lim
who_data$below_lim <- below_cov_lim
region_ops <- unique(dhs_data$region_dhs)
dateByRegion_list <- list()
for(i in 1:length(region_ops)){
  dateByRegion_list[[i]] <- unique(who_data$date_combined[who_data$region_combined == region_ops[i]])
}
mda_coverage_count <- c()
for(i in 1:nrow(dhs_data)){
  mda_coverage_count[i] <- 0
  for(j in 1:length(region_ops)){
    if(length(dateByRegion_list[[j]]) > 0){
      for(k in 1:length(dateByRegion_list[[j]])){
        date_max <- as.Date(dateByRegion_list[[j]][k])
        date_min <- as.Date(dateByRegion_list[[j]][k])
        month(date_max) <- month(date_max) + recall_length
        if(who_data[who_data$region_combined == region_ops[j] & who_data$date_combined == dateByRegion_list[[j]][k],]$below_lim){
          if((dhs_data$region_dhs[i] == region_ops[j]) && (dhs_data$date[i] >= date_min) && (dhs_data$date[i] <= date_max)){
            mda_coverage_count[i] <- mda_coverage_count[i] + 1
          }
        }
      }
    }
  }
}
dhs_data$cycles_included <- mda_coverage_count

total_responses_one <- sum(dhs_data$med_dhs[dhs_data$cycles_included == 1] + dhs_data$no_med_dhs[dhs_data$cycles_included == 1])
total_responses_two <- sum(dhs_data$med_dhs[dhs_data$cycles_included == 2] + dhs_data$no_med_dhs[dhs_data$cycles_included == 2])
total_responses_temp <- c(total_responses_one,total_responses_two)
cycleNum_ops <- c("One deworming cycle","Two deworming cycles")
country_names[(length(country_names) + 1):(length(country_names) + 2)] <- country_name
cycleNum[(length(cycleNum) + 1):(length(cycleNum) + 2)] <- cycleNum_ops
total_responses[(length(total_responses) + 1):(length(total_responses) + 2)] <- total_responses_temp

region_ops <- unique(dhs_data$region_dhs)
region_responses <- c()
cycle_responses <- c()
total_resp <- c()
for(region in 1:length(region_ops)){
  for(cycle in 1:2){
    total <- 0
    for(row in 1:nrow(dhs_data)){
      if((dhs_data$region_dhs[row] == region_ops[region]) && (dhs_data$cycles_included[row] == cycle)){
        total <- total + dhs_data$med_dhs[row] + dhs_data$no_med_dhs[row]
      }
    }
    total_resp[length(total_resp) + 1] <- total
    region_responses[length(region_responses) + 1] <- region_ops[region]
    if(cycle == 1){
      cycle_responses[length(cycle_responses) + 1] <- "One deworming cycle"
    }
    if(cycle == 2){
      cycle_responses[length(cycle_responses) + 1] <- "Two deworming cycles"
    }
  }
}
dhs_responses_cycles_region <- data.frame(region_responses,cycle_responses,total_resp)

dhs_compare_region <- c()
dhs_compare_cycle <- c()
dhs_compare_cycleNum <- c()
dhs_compare_cycleDate <- c()
dhs_compare_coverage <- c()
if(nrow(dhs_data[dhs_data$cycles_included == 2,]) > 0){
  date_ops <- unique(who_data$date_combined)
  for(region in 1:length(region_ops)){
    for(cycle in 1:3){
      
      dhs_compare_region[length(dhs_compare_region) + 1] <- region_ops[region]
      dhs_compare_cycle[length(dhs_compare_cycle) + 1] <- cycle
      
      if(cycle == 1){
        dhs_compare_cycleDate[length(dhs_compare_cycleDate) + 1] <- date_ops[1]
        date_max <- as.Date(date_ops[1])
        date_min <- as.Date(date_ops[1])
        month(date_max) <- month(date_max) + recall_length
        dhs_compare_cycleNum[length(dhs_compare_cycleNum) + 1] <- "One cycle"
      }
      if(cycle == 2){
        dhs_compare_cycleDate[length(dhs_compare_cycleDate) + 1] <- date_ops[2]
        date_max <- as.Date(date_ops[2])
        date_min <- as.Date(date_ops[2])
        month(date_max) <- month(date_max) + recall_length
        dhs_compare_cycleNum[length(dhs_compare_cycleNum) + 1] <- "One cycle"
      }
      if(cycle == 3){
        dhs_compare_cycleDate[length(dhs_compare_cycleDate) + 1] <- 0
        dhs_compare_cycleNum[length(dhs_compare_cycleNum) + 1] <- "Two cycle"
      }
      
      dhs_med <- 0
      dhs_no_med <- 0
      
      for(i in 1:nrow(dhs_data)){
        if(cycle == 1 | cycle == 2){
          if((dhs_data$region_dhs[i] == region_ops[region]) && (dhs_data$date[i] >= date_min) && (dhs_data$date[i] <= date_max) && (dhs_data$cycles_included[i] == 1)){
            dhs_med <- dhs_med + dhs_data$med_dhs[i]
            dhs_no_med <- dhs_no_med + dhs_data$no_med_dhs[i]
          }
        }
        if(cycle == 3){
          if((dhs_data$region_dhs[i] == region_ops[region]) && (dhs_data$cycles_included[i] == 2)){
            dhs_med <- dhs_med + dhs_data$med_dhs[i]
            dhs_no_med <- dhs_no_med + dhs_data$no_med_dhs[i]
          }
        }
      }
      
      if(dhs_med == 0 && dhs_no_med == 0){
        dhs_compare_coverage[length(dhs_compare_coverage) + 1] <- 0
      }
      if(dhs_med != 0 | dhs_no_med != 0){
        dhs_compare_coverage[length(dhs_compare_coverage) + 1] <- dhs_med/(dhs_med + dhs_no_med)
      }
    }
  }
  
}

dhs_region <- unique(dhs_responses_cycles_region$region_responses)
for(i in 1:length(dhs_region)){
  if(sum(dhs_responses_cycles_region$total_resp[dhs_responses_cycles_region$region_responses == dhs_region[i]]) == 0){
    dhs_responses_cycles_region <- dhs_responses_cycles_region[-which(dhs_responses_cycles_region$region_responses == dhs_region[i]),]
  }
}

dhs_cycles_count <- dhs_data

p <- ggplot(dhs_responses_cycles_region, aes(fill=cycle_responses, y=total_resp, x=region_responses)) + 
  geom_bar( stat="identity", position="fill")
p <- p + coord_flip() + labs(title = sprintf("%s region deworming eligibility - %s month recall", country_name, recall_length), y = "Percent of observations", x = "District") + expand_limits(y=c(0,1)) + scale_y_continuous(breaks=seq(0, 1, 0.1))
print(p)

r <- ggplot(dhs_responses_cycles_region, aes(fill=cycle_responses, y=total_resp, x=region_responses)) + 
  geom_bar( stat="identity")
r <- r + coord_flip() + labs(title = sprintf("%s deworming eligibility (%s month recall)", country_name, recall_length), y = "Number of observations", x = "District") +
  #BURUNDI, MYANMAR, PHILIPPINES
  #expand_limits(y=c(0,700)) + scale_y_continuous(breaks=seq(0, 700, 100)) +
  #INDONESIA
  expand_limits(y=c(0,1400)) + scale_y_continuous(breaks=seq(0, 1400, 200)) +
  #expand_limits(y=c(0,signif(max(aggregate(dhs_responses_cycles_region$total_resp, by=list(Category=dhs_responses_cycles_region$region_responses), FUN = sum)$x) + 50))) + 
  #scale_y_continuous(breaks=seq(0, signif(max(aggregate(dhs_responses_cycles_region$total_resp, by=list(Category=dhs_responses_cycles_region$region_responses), FUN = sum)$x) + 50), 100)) +
  theme(text=element_text(family="Times New Roman")) +
  #BURUNDI & MYANMAR
  theme(axis.text.x =element_text(size=25), axis.text.y =element_text(size=25), axis.title=element_text(size=25,face="bold"), plot.title = element_text(size=25))
  #PHILIPPINES
  #theme(axis.text.x =element_text(size=25), axis.text.y =element_text(size=23), axis.title=element_text(size=29,face="bold"), plot.title = element_text(size=25))
print(r)

dhs_data_mda_compare_country <- data.frame(country_names, total_responses, cycleNum)
colnames(dhs_data_mda_compare_country) <- c("country","responses","cycleNum")
q <- ggplot(dhs_data_mda_compare_country, aes(fill=cycleNum, y=responses, x=country)) + 
  geom_bar( stat="identity", position="fill")
q <- q + labs(title = sprintf("Overall deworming eligibility - %s month recall", recall_length), y = "Percent of responses", x = "District") + expand_limits(y=c(0,1)) + scale_y_continuous(breaks=seq(0, 1, 0.1))
print(q)



########## COMPARISON ANALYSIS ###########
#FINAL ANALYSIS (ONLY IF MDA_ALLOWED == 3)
if(mda_allowed == 3){
  dhs_data <- dhs_cycles_count
  who_data <- who_preAna_data
  
  dhs_data <- dhs_data[dhs_data$cycles_included > 0,]
  who_data <- who_data[who_data$coverage_who <= who_cov_lim,]
  if(nrow(dhs_data) > 0){
    region_ops <- unique(who_data$region_combined)
    date_ops <- list()
    for(i in 1:length(region_ops)){
      date_ops[[i]] <- unique(who_data$date_combined[who_data$region_combined == region_ops[i]])
    }
    
    coverage_total_combine <- c()
    coverage_total_who <- c()
    med_total_dhs <- c()
    no_med_total_dhs <- c()
    region_total <- c()
    date_total <- c()
    source_total <- c()
    
    for(region in 1:length(region_ops)){
      for(date in 1:length(date_ops[[region]])){
        
        region_total[(length(region_total) + 1):(length(region_total) + 2)] <- region_ops[region]
        date_total[(length(date_total) + 1):(length(date_total) + 2)] <- date_ops[[region]][date]
        source_total[(length(source_total) + 1):(length(source_total) + 2)] <- c("dhs","who")
        med_total_dhs[length(med_total_dhs) + 1] <- 0
        no_med_total_dhs[length(no_med_total_dhs) + 1] <- 0
        coverage_total_who[length(coverage_total_who) + 1] <- 0
        
        date_max <- as.Date(date_ops[[region]][date])
        date_min <- as.Date(date_ops[[region]][date])
        month(date_max) <- month(date_max) + recall_length
        
        for(i in 1:nrow(dhs_data)){
          if((dhs_data$region_dhs[i] == region_ops[region]) && (dhs_data$date[i] >= date_min) && (dhs_data$date[i] <= date_max)){
            med_total_dhs[length(med_total_dhs)] <- med_total_dhs[length(med_total_dhs)] + dhs_data$med_dhs[i]
            no_med_total_dhs[length(no_med_total_dhs)] <- no_med_total_dhs[length(no_med_total_dhs)] + dhs_data$no_med_dhs[i]
          }
        }
        for(i in 1:nrow(who_data)){
          if((who_data$region_combined[i] == region_ops[region]) && (who_data$date_combined[i] == date_ops[[region]][date])){
            coverage_total_who[length(coverage_total_who)] <- who_data$coverage_who[i]
          }
        }
        coverage_total_dhs <- (med_total_dhs[length(med_total_dhs)])/(med_total_dhs[length(med_total_dhs)] + no_med_total_dhs[length(no_med_total_dhs)])
        coverage_total_combine[(length(coverage_total_combine) + 1):(length(coverage_total_combine) + 2)] <- c((coverage_total_dhs*100),(coverage_total_who[length(coverage_total_who)]*100))
      }
    }
    coverage_frame <- data.frame(region_total,date_total,source_total,coverage_total_combine)
    coverage_name <- c()
    for(i in 1:nrow(coverage_frame)){
      coverage_name[i] <- sprintf("%s - %s", coverage_frame$region_total[i], coverage_frame$date_total[i])
    }
    coverage_frame$name <- coverage_name
    
    #Prep frame for Bland-Altman Plot
    #Use coverage_frame to start
    #Only include region-dates for which DHS & WHO data is available
    #One vector for difference of adjacent rows
    #One vector for mean of adjacent rows
    mean_cov <- c()
    dif_cov <- c()
    for(i in 1:(nrow(coverage_frame)/2)){
      if(!is.na(coverage_frame$coverage_total_combine[(i*2) - 1]) & !is.na(coverage_frame$coverage_total_combine[(i*2)])){
        temp_val <- c(coverage_frame$coverage_total_combine[(i*2) - 1],coverage_frame$coverage_total_combine[(i*2)])
        mean_cov[length(mean_cov) + 1] <- mean(temp_val)
        dif_cov[length(dif_cov) + 1] <- temp_val[1] - temp_val[2]
      }
    }
  }
  bland_frame <- data.frame(mean_cov,dif_cov)
  
  coverage_combined <- coverage_frame
  
  p <- ggplot(coverage_frame, aes(fill=source_total, y=coverage_total_combine, x=name)) + 
    geom_bar(position="dodge", stat="identity")
  p <- p + coord_flip() + labs(title = sprintf("Coverage (%s - %s month recall) [all cycle elgibility]", country_name, recall_length), y = "Coverage (%)", x = "District") + expand_limits(y=c(0,100)) + scale_y_continuous(breaks=seq(0, 100, 10))
  print(p)
  
  q <- ggplot(bland_frame, aes(y = dif_cov, x = mean_cov)) + geom_point()
  q <- q + labs(title = sprintf("Bland-Altman plot (%s - %s month recall)", country_name, recall_length), y = "Difference in coverage (%)", x = "Mean coverage (%)") + expand_limits(y=c(-100,100)) + scale_y_continuous(breaks=seq(-100,100,20))
  q <- q + geom_hline(yintercept = 0, size = 1)
  print(q)
  
}
if(mda_allowed == 3){
  dhs_data <- dhs_cycles_count
  who_data <- who_preAna_data
  
  dhs_data <- dhs_data[dhs_data$cycles_included > 0,]
  who_data <- who_data[who_data$coverage_who <= who_cov_lim,]
  if(nrow(dhs_data) > 0){
    date_ops_mda2 <- unique(who_data$date_combined)
    region_ops <- unique(who_data$region_combined)
    who_cycles_include <- list()
    for(region in 1:length(region_ops)){
      date_list <- c()
      for(date in 1:length(date_ops_mda2)){
        date_count <- c()
        date_max <- as.Date(date_ops_mda2[date])
        date_min <- as.Date(date_ops_mda2[date])
        month(date_max) <- month(date_max) + recall_length
        for(row in 1:nrow(dhs_data)){
          date_count[row] <- 0
          if((dhs_data$date[row] >= date_min) && (dhs_data$date[row] <= date_max)){
            date_count[row] <- date_count[row] + 1
          }
        }
        dhs_data$date_count <- date_count
        if(sum(dhs_data[dhs_data$region_dhs == region_ops[region],]$date_count) > 0){
          date_list[length(date_list) + 1] <- date_ops_mda2[date]
        }
      }
      who_cycles_include[[region]] <- date_list
    }
    #Work around to ensure if last region tested is NULL it will still populate list
    who_cycles_include[[length(region_ops) + 1]] <- c(2)
    who_cycles_include[[length(region_ops) + 1]] <- NULL
    region_ops <- unique(who_data$region_combined)
    region_mda2 <- c()
    med_mda2 <- c()
    no_med_mda2 <- c()
    who_max <- c()
    who_min <- c()
    who_max_sd <- c()
    who_min_sd <- c()
    who_max_treat <- c()
    who_min_treat <- c()
    who_pop <- c()
    for(region in 1:length(region_ops)){
      region_mda2[region] <- region_ops[region]
      med_mda2[region] <- 0
      no_med_mda2[region] <- 0
      #EDIT TO RESTRICT WHO CYCLES TO ONLY THOSE WITH CORRESPONDING DHS DATA INCLUDED
      for(row in 1:nrow(dhs_data)){
        if(region_ops[region] == dhs_data$region_dhs[row]){
          med_mda2[region] <- med_mda2[region] + dhs_data$med_dhs[row]
          no_med_mda2[region] <- no_med_mda2[region] + dhs_data$no_med_dhs[row]
        }
      }
      who_temp <- c()
      who_sd_temp <- c()
      who_treat_temp <- c()
      who_pop_temp <- c()
      for(row in 1:nrow(who_data)){
        if((who_data$region_combined[row] == region_ops[region]) && (who_data$date_combined[row] %in% who_cycles_include[[region]])){
          who_temp[length(who_temp) + 1] <- who_data$coverage_who[row]
          who_treat_temp[length(who_treat_temp) + 1] <- who_data$tr_combined[row]
          who_pop_temp[length(who_pop_temp) + 1] <- who_data$pop_combined[row]
          who_sd_temp[length(who_sd_temp) + 1] <- sqrt(((who_data$tr_combined[row]/who_data$pop_combined[row])*(1 - who_data$tr_combined[row]/who_data$pop_combined[row]))/(who_data$pop_combined[row]))
        }
      }
      if(length(who_temp) < 2){
        who_temp[(length(who_temp) + 1):2] <- 0
        who_treat_temp[(length(who_treat_temp) + 1):2] <- 0
        who_pop_temp[(length(who_pop_temp) + 1):2] <- 0
        who_sd_temp[(length(who_sd_temp) + 1):2] <- 0
      }
      if(who_temp[1] >= who_temp[2]){
        who_max[region] <- who_temp[1]
        who_min[region] <- who_temp[2]
        who_max_treat[region] <- who_treat_temp[1]
        who_min_treat[region] <- who_treat_temp[2]
        who_max_sd[region] <- who_sd_temp[1]
        who_min_sd[region] <- who_sd_temp[2]
      }
      if(who_temp[1] <= who_temp[2]){
        who_min[region] <- who_temp[1]
        who_max[region] <- who_temp[2]
        who_min_treat[region] <- who_treat_temp[1]
        who_max_treat[region] <- who_treat_temp[2]
        who_min_sd[region] <- who_sd_temp[1]
        who_max_sd[region] <- who_sd_temp[2]
      }
      who_pop[region] <- mean(who_pop_temp)
      if(0 %in% who_pop_temp){
        who_pop[region] <- max(who_pop_temp)
      }
    }
    dhs_mda2 <- data.frame(region_mda2,med_mda2,no_med_mda2)
    dhs_mda2$coverage <- (med_mda2)/(med_mda2 + no_med_mda2)
    who_mda2 <- data.frame(region_mda2,who_max,who_min,who_max_sd,who_min_sd,who_max_treat,who_min_treat,who_pop)
    
    for(region in nrow(who_mda2):1){
      if(who_mda2$who_max[region] == 0){
        region_remove <- who_mda2$region_mda2[region]
        who_mda2 <- who_mda2[-region,]
        dhs_mda2 <- dhs_mda2[-which(dhs_mda2$region_mda2 == region_remove),]
      }
    }
    
  }
  dhs_by_cycle <- dhs_data
  analysis_dates <- date_ops_mda2
  dhs_mda2 <- dhs_mda2
  who_mda2 <- who_mda2
  
  dhs_data <- dhs_mda2
  who_data <- who_mda2
  dhs_by_cycle <- dhs_by_cycle
  
  if(nrow(dhs_by_cycle) >0){
    # Determine Standard Deviation for DHS data
    dhs_sd <- sqrt(((dhs_data$med_mda2/(dhs_data$med_mda2+dhs_data$no_med_mda2))*(1 - dhs_data$med_mda2/(dhs_data$med_mda2+dhs_data$no_med_mda2)))/(dhs_data$med_mda2+dhs_data$no_med_mda2))
    dhs_data$sd <- dhs_sd
    
    alpha <- alpha_original #Original was 0.2
    lsre <- 100000000000000000000000000000000
    region_who_ops <- na.omit(unique(who_data$region_mda2))
    who_data <- who_data[with(who_data, order(region_mda2)),]
    dhs_data <- dhs_data[with(dhs_data, order(region_mda2)),]
    alpha_history <- c()
    lsre_history <- c()
    lsre_accepted <- c()
    alpha_accepted <- c()
    iteration_accepted <- c()
    for(trial in 1:trial_total){
      alpha_proposed <- runif(1, min = (alpha - 0.01), max = (alpha + 0.01))
      if(alpha_proposed < -1){
        alpha_proposed <- -1
      }
      if(alpha_proposed > 1){
        alpha_proposed <- 1
      }
      #NEED TO CONFIRM
      rho_proposed <- ((1 - who_data$who_max)*(1 - who_data$who_min)) + ((alpha_proposed)*sqrt(who_data$who_max*(1 - who_data$who_max)*who_data$who_min*(1 - who_data$who_min)))
      cycle_combined <- 1- rho_proposed
      
      #ORIGINAL
      #cycle_combined <- (who_data$who_max) + (alpha_proposed*who_data$who_min)
      for(i in 1:length(cycle_combined)){
        if(cycle_combined[i] > 1){
          cycle_combined[i] <- 1
        }
        if(cycle_combined[i] < 0){
          cycle_combined[i] <- 0
        }
      }
      correlation_proposed <- cor(cycle_combined,dhs_data$coverage)
      lsre_propose <- cycle_combined - dhs_data$coverage
      lsre_propose <- lsre_propose^2
      lsre_propose <- sum(lsre_propose)
      if(abs(lsre_propose) <= abs(lsre)){
        lsre <- lsre_propose
        alpha <- alpha_proposed
        iteration_accepted[length(iteration_accepted) + 1] <- trial
        lsre_accepted[length(lsre_accepted) + 1] <- lsre
        alpha_accepted[length(alpha_accepted) + 1] <- alpha_proposed
      }
      alpha_history[trial] <- alpha_proposed
      lsre_history[trial] <- lsre_propose
    }
    
    splineResults <- spline(iteration_accepted,alpha_accepted,1000)
    iteration_spline <- splineResults$x
    alpha_spline <- splineResults$y
    splineConverge <- data.frame(iteration_spline,alpha_spline)
    parameter_history <- data.frame(seq(1,trial_total),alpha_history,lsre_history)
    parameter_accepted <- data.frame(seq(1,length(alpha_accepted)),alpha_accepted,lsre_accepted)
    #NEED TO CONFIRM
    alpha_final <- alpha_accepted[length(alpha_accepted)]
    if(alpha_override){
      alpha_final <- alpha_override_val
    }
    rho_final <- ((1 - who_data$who_max)*(1 - who_data$who_min)) + ((alpha_final)*sqrt(who_data$who_max*(1 - who_data$who_max)*who_data$who_min*(1 - who_data$who_min)))
    cycle_final <- 1 - rho_final
    for(i in 1:length(cycle_final)){
      if(cycle_final[i] == 0){
        cycle_final[i] <- who_data$who_max[i]
      }
      if(cycle_final[i] < who_data$who_max[i]){
        cycle_final[i] <- who_data$who_max[i]
      }
    }

    #ORIGINAL
    #cycle_final <- (who_data$who_max) + (alpha*who_data$who_min)
    cycle_final <- replace(cycle_final, cycle_final > 1.0, 1.0)
    cycle_final_sd <- rep(NA,nrow(who_data))
    #cycle_final_sd <- sqrt((who_data$who_max_sd)^2 + (alpha*who_data$who_min_sd)^2)
    correlation_bar_region <- c()
    correlation_bar_source <- c()
    correlation_bar_value <- c()
    correlation_bar_sd <- c()
    for(i in 1:length(cycle_final)){
      correlation_bar_region[(length(correlation_bar_region) + 1):(length(correlation_bar_region) + 2)] <- as.character(dhs_data$region_mda2[i])
      correlation_bar_source[(length(correlation_bar_source) + 1):(length(correlation_bar_source) + 2)] <- c("WHO estimation","DHS estimation")
      correlation_bar_value[(length(correlation_bar_value) + 1):(length(correlation_bar_value) + 2)] <- c(cycle_final[i],dhs_data$coverage[i])
      correlation_bar_sd[(length(correlation_bar_sd) + 1):(length(correlation_bar_sd) + 2)] <- c(cycle_final_sd[i],dhs_data$sd[i])
    }
    correlation_bar_value <- correlation_bar_value*100
    correlation_bar_sd <- correlation_bar_sd*100
    proposed_frame <- data.frame(correlation_bar_region,correlation_bar_source,correlation_bar_value,correlation_bar_sd)
    colnames(proposed_frame) <- c("region","source","value","sd")
    cor_test <- cor(cycle_final,dhs_data$coverage)
    proposed_frame_cor <- data.frame(dhs_data$region_mda2, cycle_final*100, dhs_data$coverage*100)
    colnames(proposed_frame_cor) <- c("region","WHO_cov","DHS_cov")
    bland_altman_who <- proposed_frame[proposed_frame$source == "WHO estimation",]$value
    bland_altman_dif <- proposed_frame[proposed_frame$source == "WHO estimation",]$value - proposed_frame[proposed_frame$source == "DHS estimation",]$value
    bland_altman_region <- unique(proposed_frame$region)
    bland_altman_frame <- data.frame(bland_altman_region,bland_altman_who,bland_altman_dif)
    colnames(bland_altman_frame) <- c("region","WHO_value","difference")
    
    p <- ggplot(parameter_history, aes(y = alpha_history, x = seq.1..trial_total.)) + geom_point()
    p <- p + labs(title = sprintf("Convergence graph (%s - %s month recall)", country_name, recall_length), y = "alpha value", x = "Count") + expand_limits(y=c(0,1)) + scale_y_continuous(breaks=seq(0,1,0.1)) +
      theme(text=element_text(family="Times New Roman")) +
      theme(axis.text.x =element_text(size=25), axis.text.y =element_text(size=28), axis.title=element_text(size=28,face="bold"), plot.title = element_text(size=28))
    print(p)
    
    p <- ggplot(proposed_frame, aes(fill=source, y=value, x=region, ymin=value-(1.96*sd), ymax=value+(1.96*sd))) + 
      geom_bar(position="dodge", stat="identity") +
      geom_errorbar (position=position_dodge(0.9), colour="gray44", width = 0.5, size = 1.4)
    p <- p + coord_flip() + labs(title = sprintf("%s deworming coverage (%s month recall)", country_name, recall_length), y = "Coverage (%)", x = "District") 
    p <- p + expand_limits(y=c(0,100)) + scale_y_continuous(breaks=seq(0, 100, 10)) +
      theme(text=element_text(family="Times New Roman")) +
      #BURUNDI & MYANMAR
      theme(axis.text.x =element_text(size=25), axis.text.y =element_text(size=28), axis.title=element_text(size=28,face="bold"), plot.title = element_text(size=28))
      #PHILIPPINES
      #theme(axis.text.x =element_text(size=25), axis.text.y =element_text(size=27), axis.title=element_text(size=31,face="bold"), plot.title = element_text(size=27))
    print(p)
    
    p <- ggplot(proposed_frame_cor, aes(y=DHS_cov, x=WHO_cov)) + 
      geom_point()
    p <- p + labs(title = sprintf("Coverage plot (%s - %s month recall)", country_name, recall_length), y = "DHS reported coverage (%)", x = "WHO reported coverage (%)") 
    p <- p + expand_limits(y=c(50,100), x=c(50,100)) + scale_y_continuous(breaks=seq(50, 100, 10)) + scale_x_continuous(breaks=seq(50, 100, 5))
    p <- p + geom_smooth(method = "lm", se = FALSE) + geom_label(label= as.character(proposed_frame_cor$region), nudge_x = 0.25, nudge_y = 2, check_overlap = F)
    print(p)
    
    p <- ggplot(proposed_frame_cor, aes(y=DHS_cov, x=WHO_cov)) + 
      geom_point(size = 4)
    p <- p + labs(title = sprintf("Correlation plot (%s - %s month recall)", country_name, recall_length), y = "Coverage estimated by DHS data (%)", x = "Coverage reported to WHO (%)") 
    p <- p + expand_limits(y=c(20,100), x=c(20,100)) + scale_y_continuous(breaks=seq(20, 100, 10)) + scale_x_continuous(breaks=seq(20, 100, 10))
    p <- p + geom_smooth(method = "lm", se = FALSE, size = 4) + theme(text=element_text(family="Times New Roman")) +
      theme(axis.text.x =element_text(size=34), axis.text.y =element_text(size=34), axis.title=element_text(size=36,face="bold"), plot.title = element_text(size=36))
    print(p)
    
    #NOTE THIS IS NOT AN ACTUAL BLAND ALTMAN AS WE DO NOT USE MEAN COVERAGE ON X-AXIS
    q <- ggplot(bland_altman_frame, aes(y = difference, x = WHO_value)) + geom_point()
    q <- q + labs(title = sprintf("Bland-Altman plot (%s - %s month recall)", country_name, recall_length), y = "Difference in coverage (%)", x = "WHO coverage (%)") + expand_limits(y=c(-100,100)) + scale_y_continuous(breaks=seq(-100,100,20))
    q <- q + geom_hline(yintercept = 0, size = 1)
    print(q)
    
  }
}

####WRITE EXCEL SHEET FOR ALL DATA ####

WriteXLS(c("proposed_frame","dhs_responses_cycles_region"),ExcelFileName = sprintf("%s_%s_Coverage_Demographics.xlsx", country_name, date_analysis), SheetNames = c("Deworming Coverage","Demographics DHS"))


####FOR SENSITIVITY ANALYSIS ####
if(SA){
  #Coverage Reliability Threshold
  p <- ggplot(proposed_frame, aes(fill=source, y=value, x=region, ymin=value-(1.96*sd), ymax=value+(1.96*sd))) + 
    geom_bar(position="dodge", stat="identity") +
    geom_errorbar (position=position_dodge(0.9), colour="gray44", width = 0.5, size = 1.4)
  p <- p + coord_flip() + labs(title = sprintf("%s deworming coverage (%s%% threshold)", country_name, who_cov_lim*100), y = "Coverage (%)", x = "District") 
  p <- p + expand_limits(y=c(0,100)) + scale_y_continuous(breaks=seq(0, 100, 10)) +
    theme(text=element_text(family="Times New Roman")) +
    theme(axis.text.x =element_text(size=25), axis.text.y =element_text(size=28), axis.title=element_text(size=28,face="bold"), plot.title = element_text(size=28))
  print(p)
  
  #Treatment Correlation Parameter
  p <- ggplot(proposed_frame, aes(fill=source, y=value, x=region, ymin=value-(1.96*sd), ymax=value+(1.96*sd))) + 
    geom_bar(position="dodge", stat="identity") +
    geom_errorbar (position=position_dodge(0.9), colour="gray44", width = 0.5, size = 1.4)
  p <- p + coord_flip() + labs(title = sprintf("%s deworming coverage (%s%% campaign corrleation)", country_name, alpha_override_val*100), y = "Coverage (%)", x = "District") 
  p <- p + expand_limits(y=c(0,100)) + scale_y_continuous(breaks=seq(0, 100, 10)) +
    theme(text=element_text(family="Times New Roman")) +
    #BURUNDI & MYANMAR
    theme(axis.text.x =element_text(size=25), axis.text.y =element_text(size=28), axis.title=element_text(size=28,face="bold"), plot.title = element_text(size=28))
  #PHILIPPINES
  #theme(axis.text.x =element_text(size=25), axis.text.y =element_text(size=27), axis.title=element_text(size=31,face="bold"), plot.title = element_text(size=27))
  print(p)
  
  #Maybe Receipts Status - Yes
  p <- ggplot(proposed_frame, aes(fill=source, y=value, x=region, ymin=value-(1.96*sd), ymax=value+(1.96*sd))) + 
    geom_bar(position="dodge", stat="identity") +
    geom_errorbar (position=position_dodge(0.9), colour="gray44", width = 0.5, size = 1.4)
  p <- p + coord_flip() + labs(title = sprintf("%s deworming coverage (maybe receipt: +1)", country_name), y = "Coverage (%)", x = "District") 
  p <- p + expand_limits(y=c(0,100)) + scale_y_continuous(breaks=seq(0, 100, 10)) +
    theme(text=element_text(family="Times New Roman")) +
    theme(axis.text.x =element_text(size=25), axis.text.y =element_text(size=28), axis.title=element_text(size=28,face="bold"), plot.title = element_text(size=28))
  print(p)
  
  #Maybe Receipts Status - No
  p <- ggplot(proposed_frame, aes(fill=source, y=value, x=region, ymin=value-(1.96*sd), ymax=value+(1.96*sd))) + 
    geom_bar(position="dodge", stat="identity") +
    geom_errorbar (position=position_dodge(0.9), colour="gray44", width = 0.5, size = 1.4)
  p <- p + coord_flip() + labs(title = sprintf("%s deworming coverage (maybe receipt: +0)", country_name), y = "Coverage (%)", x = "District") 
  p <- p + expand_limits(y=c(0,100)) + scale_y_continuous(breaks=seq(0, 100, 10)) +
    theme(text=element_text(family="Times New Roman")) +
    theme(axis.text.x =element_text(size=25), axis.text.y =element_text(size=28), axis.title=element_text(size=28,face="bold"), plot.title = element_text(size=28))
  print(p)
  
}


###GENERATE GENDER DEWORMING COVERAGE GRAPHS####
gender <- c("girls","boys")
burundi_gender <- c(74.6,76.5)
myanmar_gender <- c(49.2,44.8)
philippines_gender <- c(48.4,47.6)
country_gender <- c("Burundi","Burundi","Myanmar","Myanmar","Philippines","Philippines")
gender_all <- c(gender,gender,gender)
gender_cov <- c(burundi_gender,myanmar_gender,philippines_gender)
bur_gen_frame <- data.frame(gender,burundi_gender)
mya_gen_frame <- data.frame(gender,myanmar_gender)
phi_gen_frame <- data.frame(gender,philippines_gender)
gender_frame <- data.frame(country_gender,gender_all,gender_cov)

p <- ggplot(bur_gen_frame, aes(y=burundi_gender, x=gender, ymin=0, ymax=100)) +
  geom_bar(stat="identity", fill = "cyan3") +
  labs(title = "Burundi deworming coverage by child gender", y = "Coverage (%)", x = "Child gender") +
  expand_limits(y=c(0,100)) + scale_y_continuous(breaks=seq(0, 100, 10)) +
  theme(text=element_text(family="Times New Roman")) +
  theme(axis.text.x =element_text(size=25), axis.text.y =element_text(size=28), axis.title=element_text(size=28,face="bold"), plot.title = element_text(size=28))
print(p)
p <- ggplot(mya_gen_frame, aes(y=myanmar_gender, x=gender, ymin=0, ymax=100)) +
  geom_bar(stat="identity", fill = "cyan3") +
  labs(title = "Myanmar deworming coverage by child gender", y = "Coverage (%)", x = "Child gender") +
  expand_limits(y=c(0,100)) + scale_y_continuous(breaks=seq(0, 100, 10)) +
  theme(text=element_text(family="Times New Roman")) +
  theme(axis.text.x =element_text(size=25), axis.text.y =element_text(size=28), axis.title=element_text(size=28,face="bold"), plot.title = element_text(size=28))
print(p)
p <- ggplot(phi_gen_frame, aes(y=philippines_gender, x=gender, ymin=0, ymax=100)) +
  geom_bar(stat="identity", fill = "cyan3") +
  labs(title = "Philippines deworming coverage by child gender", y = "Coverage (%)", x = "Child gender") +
  expand_limits(y=c(0,100)) + scale_y_continuous(breaks=seq(0, 100, 10)) +
  theme(text=element_text(family="Times New Roman")) +
  theme(axis.text.x =element_text(size=25), axis.text.y =element_text(size=28), axis.title=element_text(size=28,face="bold"), plot.title = element_text(size=28))
print(p)
p <- ggplot(gender_frame, aes(fill=gender_all, y=gender_cov, x=country_gender)) + 
  geom_bar(position="dodge", stat="identity")
p <- p + labs(title = "Deworming coverage by child gender", y = "Coverage (%)", x = "Country") 
p <- p + expand_limits(y=c(0,100)) + scale_y_continuous(breaks=seq(0, 100, 10)) +
  theme(text=element_text(family="Times New Roman")) +
  theme(axis.text.x =element_text(size=25), axis.text.y =element_text(size=28), axis.title=element_text(size=28,face="bold"), plot.title = element_text(size=28))
print(p)

####RUN FOR QUANTITATIVE RESULTS ONLY ####

#NEED TO CHANGE DATAFRAME NAME BEFORE RUN EACH TIME
# mean(mya_proposed_frame$value[mya_proposed_frame$source == 'WHO estimation'])
# sd(mya_who100$value[mya_who100$source == 'WHO estimation'])
# mean(mya_proposed_frame$value[mya_proposed_frame$source == 'DHS estimation'])
# sd(mya_who100$value[mya_who100$source == 'DHS estimation'])
# mean(mya_proposed_frame$value[mya_proposed_frame$source == 'WHO estimation'] - mya_proposed_frame$value[mya_proposed_frame$source == 'DHS estimation'])
# sd(mya_who100$value[mya_who100$source == 'WHO estimation'] - mya_who100$value[mya_who100$source == 'DHS estimation'])
# mean(abs(mya_who100$value[mya_who100$source == 'WHO estimation'] - mya_who100$value[mya_who100$source == 'DHS estimation']))
# sd(abs(mya_who100$value[mya_who100$source == 'WHO estimation'] - mya_who100$value[mya_who100$source == 'DHS estimation']))
# range(abs(mya_who100$value[mya_who100$source == 'WHO estimation'] - mya_who100$value[mya_who100$source == 'DHS estimation']))
# range(mya_who100$value[mya_who100$source == 'WHO estimation'] - mya_who100$value[mya_who100$source == 'DHS estimation'])


####RUNS UPDATED SD CALCULATIONS ####
dhs_data$coverage

dhs_data$med_mda2 + dhs_data$no_med_mda2

test_min <- c()
test_max <- c()
for(i in 1:nrow(dhs_data)){
  test <- prop.test(dhs_data$med_mda2[i], (dhs_data$med_mda2[i] + dhs_data$no_med_mda2[i]), conf.level=0.95, correct = FALSE)
  test_min[i] <- test$conf.int[1]
  test_max[i] <- test$conf.int[2]
}

mean(dhs_data$coverage) - 1.96*sd(dhs_data$coverage)
mean(dhs_data$coverage) + 1.96*sd(dhs_data$coverage)

sum(dhs_data$med_mda2 + dhs_data$no_med_mda2)
sum(dhs_data$med_mda2)/sum(dhs_data$med_mda2 + dhs_data$no_med_mda2)
prop.test(sum(dhs_data$med_mda2), sum(dhs_data$med_mda2 + dhs_data$no_med_mda2), conf.level=0.95, correct = FALSE)