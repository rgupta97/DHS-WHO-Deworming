############ PROJECT DESCRIPTION ##################
# Project: Deworming in pre-school children Data Validation (Script 1 of 2)

# Coded by Ribhav Gupta (Stanford University)
# Email: rgupta97@stanford.edu 
# Last updated: 07/12/19 

#Purpose of File: Clean Up Deworming Data Sets

# Files Required:
#   1. Files from WHO on deworming program
#   2. Files from DHS on individual data

# **Note: Directory should be updated based on where files are saved.**

#File Outputs:
# 1. 
# 2. 

############ UPDATE BEFORE EACH RUN ##################

#UPDATE BASED ON YOUR DIRECTORY
setwd("/Users/rgupta97/Documents/Lab_Work")

library(haven)
library(readxl)
library(lubridate)
library(WriteXLS)
library(labelled)

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

#PARAMETERS TO CHANGE
maybeToMaybe <- TRUE
maybeToYes <- FALSE
maybeToNo <- FALSE

#DATE OF ANALYSIS (format as MMDDYY)
date_analysis <- "070719"

#COUNTRY NAME (CHANGE BASED ON COUNTRY)
#country_name <- "PHILIPPINES"
country_name <- "BURUNDI"
#country_name <- "MYANMAR"
#country_name <- "INDONESIA"

#DHS DATA INPUT (CHANGE .DTA FILE TO COUNTRY SPECIFIC INDIVIDUAL FILE)
#DHS_ind <- read_dta("PHIR70FL.DTA")
DHS_ind <- read_dta("BUIR70FL.DTA")
#DHS_ind <- read_dta("MMIR71FL.DTA")
#DHS_ind <- read_dta("IDIR71FL.DTA")

#WHO DATA (CHANGE EXCEL TO COUNTRY SPECIFIC SHEET)
#WHO_raw <- read_excel("PreSAC_PH_IN.xlsx", sheet = "PHILIPPINES")
WHO_raw <- read_excel("PreSAC_BI_MM.xlsx", sheet = "BURUNDI")
#WHO_raw <- read_excel("PreSAC_BI_MM.xlsx", sheet = "MYANMAR")
#WHO_raw <- read_excel("PreSAC_ID_PH.xlsx", sheet = "INDONESIA")

#MASTER REGION LIST (CHANGE BASED ON COUNTRY) (MUST BE IN SAME ORDER AS DHS LIST)
#Philippines
#region <- c("ilocos","cagayan valley","central luzon","calabarzon","bicol","western visayas","central visayas","eastern visayas","zamboanga peninsula","northern mindanao","davao","soccsksargen","national capital","cordillera","bangsamoro","caraga","mimaropa","negros island region")
#BURUNDI
region <- c("bubanza","bujumbura rural","bururi","cankuzo","cibitoke","gitega","karusi","kayanza","kirundo","makamba","muramvya","muyinga","mwaro","ngozi","rutana","ruyigi","bujumbura mairie","rumonge")
#Myanmar
#region <- c("kachin","kayah","kayin","chin","sagaing","taninthayi","bago","magway","mandalay","mon","rakhine","yangon","shan","ayeyarwaddy","naypyitaw")
#Indonesia
#region <- c("aceh","north sumatera","west sumatera","riau","jambi","south sumatera","bengkulu","lampung","bangka belitung","riau islands","jakarta","west java","central java","yogyakarta","east java","banten","bali","west nusa tenggara","east nusa tenggara","west kalimantan","central kalimantan","south kalimantan","east kalimantan","north kalimantan","north sulawesi","central sulawesi","south sulawesi","southeast sulawesi","gorontalo","west sulawesi","maluku","north maluku","west papua","papua")
#LIST OF REGIONS IN DHS
DHS_ind$v101[1]

#REGION LIST USED BY WHO FILE (MATCHES ONE TO ONE FOR EACH UNIQUE ADMIN 1 OPTION) (ALPHABETICAL ORDER OF WHO FILE)
#Philippines
#WHO_trans <- c("cordillera","caraga","caraga","western visayas","bicol","western visayas","cordillera","central luzon","bangsamoro","central luzon","cagayan valley","calabarzon","cordillera","eastern visayas","central visayas","northern mindanao","central luzon","cagayan valley","bicol","bicol","northern mindanao","western visayas","bicol","calabarzon","central visayas","davao","soccsksargen","davao","davao","davao","caraga","eastern visayas","western visayas","cordillera","ilocos","ilocos","western visayas","cagayan valley","cordillera","ilocos","calabarzon","northern mindanao","bangsamoro","eastern visayas","bangsamoro","mimaropa","bicol","national capital","northern mindanao","northern mindanao","cordillera","western visayas","central visayas","eastern visayas","central luzon","cagayan valley","mimaropa","mimaropa","mimaropa","central luzon","ilocos","calabarzon","cagayan valley","calabarzon","mimaropa","eastern visayas","soccsksargen","central visayas","bicol","soccsksargen","eastern visayas","soccsksargen","bangsamoro","caraga","caraga","central luzon","bangsamoro","central luzon","zamboanga peninsula","zamboanga peninsula","zamboanga peninsula")
#Burundi
WHO_trans <- c("bubanza","bujumbura mairie","bujumbura rural","bururi","cankuzo","cibitoke","gitega","karusi","kayanza","kirundo","makamba","muramvya","muyinga","mwaro","ngozi","rumonge","rutana","ruyigi")
#Myanmar
#WHO_trans <- c("ayeyarwaddy","bago","chin","kachin","kachin","kayah","kayin","magway","mandalay","mon","naypyitaw","rakhine","sagaing","shan","taninthayi","yangon")
#Indonesia
#WHO_trans <- c("aceh","bali","banten","bengkulu","yogyakarta","jakarta","gorontalo","jambi","west java","central java","east java","west kalimantan","south kalimantan","central kalimantan","east kalimantan","north kalimantan","bangka belitung","riau islands","lampung","maluku","north maluku","west nusa tenggara","east nusa tenggara","papua","west papua","riau","west sulawesi","south sulawesi","central sulawesi","southeast sulawesi","north sulawesi","west sumatera","south sumatera","north sumatera")
#LIST OF REGIONS IN WHO FILE
sort(unique(WHO_raw$ADMIN1))


#DHS QUESTION FOR DEWORMING
deworming_question <- "h43"
month_question <- "v006"
year_question <- "v007"
region_question <- "v101"
age_question <- "b19" #CAN ALSO BE "hw1_" (Myanmar) or "b19" (Burundi, Philippines, Indonesia)


##############CREATES DATA FROM FROM WHO DATA################

WHO_raw2 <- data.frame(WHO_raw$ADMIN1,WHO_raw$PSAC_POP,WHO_raw$TR_R1,WHO_raw$DATE_R1,WHO_raw$TR_R2,WHO_raw$DATE_R2)
colnames(WHO_raw2) <- c("region_report","population","tr_r1","date_r1","tr_r2","date_r2")
WHO_regions <- sort(unique(WHO_raw2$region_report))
WHO_translation <- list()
for(i in 1:length(WHO_regions)){
  temp_relation <- c(as.character(WHO_regions[i]),WHO_trans[i])
  WHO_translation[[i]] <- temp_relation
}
region_translated <- c()
for(i in 1:nrow(WHO_raw2)){
  for(j in 1:length(WHO_translation)){
    if(as.character(WHO_raw2$region_report[i]) == WHO_translation[[j]][1]){
      region_translated[i] <- WHO_translation[[j]][2]
    }
  }
}
WHO_raw2$region_translated <- region_translated


for(treat in 1:2){
  tr_all <- c()
  pop_all <- c() 
  region_all <- c()
  date_who <- c()
  if(treat == 1){
    date_all <- na.omit(unique(as.character(WHO_raw2$date_r1)))
  }
  if(treat == 2){
    date_all <- na.omit(unique(as.character(WHO_raw2$date_r2)))
  }
  region_unique <- na.omit(unique(as.character(WHO_trans)))
  for(region_WHO in 1:length(region_unique)){
    for(i in 1:length(date_all)){
      region_all[(length(region_all) + 1)] <- region_unique[region_WHO]
      date_who[(length(date_who) + 1)] <- date_all[i]
      tr_all[(length(tr_all) + 1)] <- 0
      pop_all[(length(pop_all) + 1)] <- 0
      for(row in 1:nrow(WHO_raw2)){
        if(treat == 1){
          if(!is.na(WHO_raw2$date_r1[row])){
            if((WHO_raw2$region_translated[row] == region_unique[region_WHO]) && (as.character(WHO_raw2$date_r1[row]) == date_all[i])){
              tr_all[length(tr_all)] <- tr_all[length(tr_all)] + WHO_raw2$tr_r1[row]
              pop_all[length(pop_all)] <- pop_all[length(pop_all)] + WHO_raw2$population[row]
            }
          }
        }
        if(treat == 2){
          if(!is.na(WHO_raw2$date_r2[row])){
            if((WHO_raw2$region_translated[row] == region_unique[region_WHO]) && (as.character(WHO_raw2$date_r2[row]) == date_all[i])){
              tr_all[length(tr_all)] <- tr_all[length(tr_all)] + WHO_raw2$tr_r2[row]
              pop_all[length(pop_all)] <- pop_all[length(pop_all)] + WHO_raw2$population[row]
            }
          }
        }
      }
    }
  }
  if(treat == 1){
    region_all_t1 <- region_all
    date_who_t1 <- date_who
    tr_all_t1 <- tr_all
    pop_all_t1 <- pop_all
  }
  if(treat == 2){
    region_all_t2 <- region_all
    date_who_t2 <- date_who
    tr_all_t2 <- tr_all
    pop_all_t2 <- pop_all
  }
}
region_combined <- c(region_all_t1,region_all_t2)
date_combined <- c(date_who_t1,date_who_t2)
tr_combined <- c(tr_all_t1,tr_all_t2)
pop_combined <- c(pop_all_t1,pop_all_t2)
coverage_who <- tr_combined/pop_combined
WHO_summary <- data.frame(region_combined,date_combined,pop_combined,tr_combined, coverage_who)
for(i in nrow(WHO_summary):1){
  if(WHO_summary$tr_combined[i] == 0){
    WHO_summary <- WHO_summary[-i,]
  }
}
WHO_summary <- WHO_summary[order(WHO_summary$region_combined),]

##############FORMATS DHS DATA################
unsure_counter <- 0
#Total children in interview (not necessarily asked question or of age)
total_counter <- 0
#Total children asked deworming question (not necessarily of age)
num_questionAsked <- 0


#DETERMINES COLUMNS FOR DEWORMING DATA
use_col <- c()
for (i in 1:ncol(DHS_ind)) {
  if (!is.na(pmatch(deworming_question, colnames(DHS_ind)[i]))) {
    use_col[length(use_col) + 1] <- i
  }
}

#BEYOND THIS POINT ONLY USING INDIVIDUAL RECORDS
month_col <- pmatch(month_question, colnames(DHS_ind))
year_col <- pmatch(year_question, colnames(DHS_ind))
region_col <- pmatch(region_question, colnames(DHS_ind))
sum_deworm <- c()
sum_no_med <- c()

age <- c()
for(i in 1:ncol(DHS_ind)){
  if(!is.na(pmatch(age_question, colnames(DHS_ind)[i]))){
    age[length(age) + 1] <- i
  }
}

region_order <- c()
region_uni <- c()
for(i in 1:nrow(unique(DHS_ind[,region_col]))){
  region_uni[i] <- as.numeric(unique(DHS_ind[,region_col])[i,1])
}
region_uni <- sort(region_uni)
for(i in 1:nrow(DHS_ind)){
  region_order[i] <- match(as.numeric(DHS_ind[i,region_col]),region_uni)
}
DHS_ind[,region_col] <- region_order

#COUNTS NUMBER OF CHILDREN VACCINATED AND NOT VACCINATED PER RESPONDER
for(i in 1:nrow(DHS_ind)){
  sum_deworm[i] <- 0
  sum_no_med[i] <- 0
  question_asked <- FALSE
  age_med_col_dif <- age[1] - use_col[1]
  recall_ageThreshold_temp <- c()
  recall_ageThreshold_val <- 0
  below_ageThreshold <- 0
  WHO_ageThreshold <- WHO_summary[WHO_summary$region_combined == region[as.numeric(DHS_ind[i,region_col])],]
  for(ageThreshold in 1:nrow(WHO_ageThreshold)){
    DHS_date_temp <- ymd(sprintf('%04d%02d%02d', as.numeric(DHS_ind[i,year_col]), as.numeric(DHS_ind[i,month_col]), 01))
    recall_ageThreshold_temp[ageThreshold] <- abs(as.numeric(round(difftime(as.character(WHO_ageThreshold$date_combined[ageThreshold]), DHS_date_temp)/30)))
  }
  recall_ageThreshold_val <- min(recall_ageThreshold_temp)
  if(is.na(recall_ageThreshold_val)){
    recall_ageThreshold_val <- 0
  }
  
  for(j in use_col[1]:use_col[length(use_col)]){
    age_criteria <- FALSE
    total_counter <- total_counter + 1
    if((!is.na(DHS_ind[i,(j + age_med_col_dif)]))){
      if(!is.na(DHS_ind[i,j]) && (DHS_ind[i,(j + age_med_col_dif)] < (60 + recall_ageThreshold_val))){
        num_questionAsked <- num_questionAsked + 1
      }
      if((DHS_ind[i,(j + age_med_col_dif)] > (11 + recall_ageThreshold_val)) && (DHS_ind[i,(j + age_med_col_dif)] < (60 + recall_ageThreshold_val))){
        age_criteria <- TRUE
      }
    }
    if(is.na(DHS_ind[i,(j + age_med_col_dif)])){
      age_criteria <- FALSE
    }
    if(!is.na(DHS_ind[i,j]) && age_criteria){
      
      if(DHS_ind[i,j] == 8){
        unsure_counter <- unsure_counter + 1
        if(maybeToMaybe){
          sum_deworm[i] <- sum_deworm[i] + as.numeric(DHS_ind[i,j]/16)
          sum_no_med[i] <- sum_no_med[i] + as.numeric(DHS_ind[i,j]/16)
        }
        if(maybeToYes){
          sum_deworm[i] <- sum_deworm[i] + as.numeric(DHS_ind[i,j]/8)
        }
        if(maybeToNo){
          sum_no_med[i] <- sum_no_med[i] + as.numeric(DHS_ind[i,j]/8)
        }
        question_asked <- TRUE
      }
      if(DHS_ind[i,j] != 8){
        sum_deworm[i] <- sum_deworm[i] + as.numeric(DHS_ind[i,j])
        question_asked <- TRUE
        if(DHS_ind[i,j] == 0){
          sum_no_med[i] <- sum_no_med[i] + 1
        }
      }
    }
    # if(!question_asked){
    #   sum_deworm[i] <- as.numeric(DHS_ind[i,j])
    #   sum_no_med[i] <- as.numeric(DHS_ind[i,j])
    # }
  }
}

Individual_Records_Relevant <- DHS_ind[,c(region_col,month_col,year_col)]
Individual_Records_Relevant$deworm_sum <- sum_deworm
Individual_Records_Relevant$not_deworm <- sum_no_med
colnames(Individual_Records_Relevant) <- c("region","month","year","med_count","not_med")

#CREATES SUMMARY BY REGION & DATE OF TOTAL DEWORMING CASES  
med_dhs <- c()
no_med_dhs <- c() 
region_dhs <- c()
year_dhs <- c()
month_dhs <- c()
month_all <- na.omit(unique(Individual_Records_Relevant$month))
year_all <- na.omit(unique(Individual_Records_Relevant$year))
for(reg in 1:length(region)){
  for(i in 1:length(year_all)){
    for(j in 1:length(month_all)){
      region_dhs[(length(region_dhs) + 1)] <- region[reg]
      month_dhs[(length(month_dhs) + 1)] <- month_all[j]
      year_dhs[(length(year_dhs) + 1)] <- year_all[i]
      med_dhs[(length(med_dhs) + 1)] <- 0
      no_med_dhs[(length(no_med_dhs) + 1)] <- 0
      for(row in 1:nrow(Individual_Records_Relevant)){
        if((Individual_Records_Relevant$month[row] == month_all[j]) && (Individual_Records_Relevant$year[row] == year_all[i]) && (Individual_Records_Relevant$region[row] == reg)){
          if(!is.na(Individual_Records_Relevant$med_count[row])){
            med_dhs[length(med_dhs)] <- med_dhs[length(med_dhs)] + as.numeric(Individual_Records_Relevant$med_count[row])
          }
          if(!is.na(Individual_Records_Relevant$not_med[row]))
            no_med_dhs[length(no_med_dhs)] <- no_med_dhs[length(no_med_dhs)] + as.numeric(Individual_Records_Relevant$not_med[row]) 
        }
      }
    }
  }
}
coverage_dhs <-  med_dhs/(med_dhs + no_med_dhs)
Individual_Records_Summary_date <- data.frame(region_dhs,month_dhs,year_dhs,med_dhs,no_med_dhs, coverage_dhs)
for(i in nrow(Individual_Records_Summary_date):1){
  if(is.na(Individual_Records_Summary_date$coverage_dhs[i])){
    Individual_Records_Summary_date <- Individual_Records_Summary_date[-i,]
  }
}
Individual_Records_Summary_date <- Individual_Records_Summary_date[order(Individual_Records_Summary_date$region_dhs),]

Individual <- Individual_Records_Relevant
Summary_date <- Individual_Records_Summary_date

################## NEED TO CHANGE BASED ON DATES THAT WORK #################

WriteXLS(c("WHO_summary","Summary_date"),ExcelFileName = sprintf("%s_%s.xlsx", country_name, date_analysis), SheetNames = c("WHO Deworming Data","DHS Deworming Data"))
