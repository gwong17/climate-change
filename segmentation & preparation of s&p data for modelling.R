# *****************************************************************************
# Segmenting S&P Data to Generate Z-Scores 
# *****************************************************************************

# Install Packages 

library(dplyr)
library(plyr)
library(lubridate)
library(MQGmigrations)
library(MQGCreditMisc)
library(xlsx)
library(readxl)
library(data.table)
library(ggplot2)

# ANZICS to GIC mapping

anzsic_codes <- read_excel(paste("C:/Users/gwong17/OneDrive - Macquarie Group/Personal Folders/Documents/Climate Change 2019/Data/segmentation ANZSIC and GICs mapping v3.xlsx", sep = ""), sheet = "ANZSIC")
gics_codes <- read_excel(paste("C:/Users/gwong17/OneDrive - Macquarie Group/Personal Folders/Documents/Climate Change 2019/Data/segmentation ANZSIC and GICs mapping v3.xlsx", sep = ""), sheet = "GICS")

corp_segment <- readRDS("C:/Users/gwong17/OneDrive - Macquarie Group/Personal Folders/Documents/Climate Change 2019/Data/Corporate.AMERICAS.MQ1-MQ16.all.Rds")

source("C:/Users/gwong17/OneDrive - Macquarie Group/Personal Folders/Documents/r-scripts/Segmentation/region_mapping.r")

data(rating_mapping, package="MQGCreditMisc")
data(MQ_ratings, package="MQGCreditMisc")
data(SandP_ratings, package="MQGCreditMisc")
data(country_map, package="MQGCreditMisc")

# Import most recent S&P Data 

Issuer_mydata <- read.csv(paste("C:/Users/gwong17/OneDrive - Macquarie Group/Personal Folders/Documents/Climate Change 2019/Data/Issuer_mydata_25_08_2021.csv",sep=""), stringsAsFactors=FALSE)

SandP_history <- adply(Issuer_mydata,1,function(d){SandP2ts(d)},.expand = F,.id = NULL)
SandP_history$Date <- as.Date(SandP_history$Date,"%m/%d/%Y")
SandP_history <- cbind(SandP_history, year = year(SandP_history$Date))

SandP_snaps <- NULL
date <- as.Date("1980-12-31")
# date <- as.Date("1981-06-30")
while(date < "2019-01-01"){
  SandP_snaps <- if(is.null(SandP_snaps)) SandP_rating_snap(date,SandP_history) else rbind(SandP_snaps,SandP_rating_snap(date,SandP_history))
  date <- date + years(1)
}

SandP_snaps$rating <- factor(SandP_snaps$rating,levels=SandP_ratings)
SandP_snaps <- left_join(SandP_snaps,dplyr::select(SandP_snaps,SnapDate,ORG_ID,rating) %>% dplyr::rename(ratingPrev=rating) %>% mutate(SnapDate=SnapDate+years(1)),by=c("SnapDate"="SnapDate","ORG_ID"="ORG_ID"))

# convert the S&P set into a mapped MQ set
SandP_snaps <- SandP_snaps %>%
  dplyr::left_join(rating_mapping %>% dplyr::select(SandP,MQRating),by=c("defaultRating"="SandP")) %>%
  dplyr::left_join(rating_mapping %>% dplyr::select(SandP,MQprev=MQRating),by=c("ratingPrev"="SandP")) %>%
  dplyr::mutate(Date=SnapDate,defaultRating=MQRating)

SandP_snaps <- SandP_snaps %>% left_join(country_map %>% dplyr::select(SandP_country,country_region), by = c("COUNTRY" = "SandP_country")) %>%
  dplyr::left_join(region_mapping, by = "country_region") %>%
  dplyr::mutate(calib_region = as.character(calib_region))

# add segments
setDT(SandP_snaps)
setDT(gics_codes)

SandP_snaps <- merge(SandP_snaps, gics_codes[,.(GIC, `IFRS9 Segment`)], by.x=c("GIC"), by.y=c("GIC"), all.x = TRUE)

SandP_snaps_DT <- copy(SandP_snaps)

# *****************************************************************************
# Segment Data as per each model  
# *****************************************************************************

ind_list <- c("Aerospace/Automotive/Capital Goods/Metal",
              "Energy and Natural Resources",
              "Transportation",
              'Utility')

# Oil & Gas 
OG_gic_list <- c("10102010", 
  "10102020", 
  "10102030", 
  "10102040", 
  "10101020", 
  "10101010", 
  "55102010")
OG_subind_list <- c("Gas",
                    "Integrated Oil & Gas",
                    "Natural Gas Distributors",
                    "Natural Gas Pipelines",
                    "Oil & Gas Exploration & Production",
                    "Oil Companies - Independent",
                    "Oil Refining & Marketing",
                    "Oilfield Service Cos (1380)")

SandP_snaps_OG <- SandP_snaps_DT[GIC %in% OG_gic_list|
                                   SUB_IND %in% OG_subind_list & INDUSTRY %in% ind_list]

# Metals & Mining
mm_subind_list <- c("Metal Mining",
                    "Metals",
                    "Metals - Nonferrous",
                    "Mining")
SandP_snaps_MM <- SandP_snaps_DT[GIC %in% c("15104010","15104020","15104025","15104030","15104040","15104045","15104050","10102050") |
                                   SIC1 %in% c(1220, 1221, 2990) |
                                   NAIC1 %in% c(212111, 212112, 212113) |
                                   ORG_ID %in% c(566633) |
                                   SUB_IND %in% ag_subind_list & INDUSTRY %in% ind_list]

# Coal 
SandP_snaps_coal <- SandP_snaps_DT[GIC == 10102050 |
                                     SIC1 %in% c(1220, 1221, 2990) |
                                     NAIC1 %in% c(212111, 212112, 212113) |
                                     ORG_ID %in% c(566633)]

# Power Generation
Power_subind_list <- c("Electric",
                       "Electric Utility")
SandP_snaps_PowerGen <- SandP_snaps_DT[(GIC %in% c(55101010 & NAIC1 != 221122) |
                                          NAIC1 == 221112 |
                                          NAIC2 == 221112 |
                                          NAIC3 == 221112 ) | GIC %in% c(55103010, 55105010, 55105020) | 
                                         SUB_IND %in% Power_subind_list & INDUSTRY %in% ind_list]

# Agriculture
ag_subind_list <- c("Agribusiness",
                    "Beverages",
                    "Food & Kindred Products",
                    "Food Service & Restaurants",
                    "Packaged And Branded Food",
                    "Supermarkets")
SandP_snaps_Ag <- SandP_snaps_DT[GIC == 30202010 | SUB_IND %in% ag_subind_list]

# Financial Institutions 
fi_gic_list <- c(40101010,
                 40101015,
                 40102010,
                 40201020,
                 40201040,
                 40202010,
                 40203010,
                 40203020,
                 40203030,
                 40203040,
                 40301010,
                 40301020,
                 40301030,
                 40301040,
                 40301050)
SandP_snaps_FI <- SandP_snaps_DT[GIC %in%  fi_gic_list | INDUSTRY == "Financial Institutions"]

# Property 
prop_gic_list <- c(40204010,
                   60101010,
                   60101020,
                   60101030,
                   60101040,
                   60101050,
                   60101060,
                   60101070,
                   60101080,
                   60102010,
                   60102020,
                   60102030)

SandP_snaps_Prop <- SandP_snaps_DT[GIC %in% prop_gic_list | INDUSTRY == "Real Estate"]

# Healthcare 
SandP_snaps_Hea <- SandP_snaps_DT[GIC == 60101050 | INDUSTRY == "Health Care/Chemicals"]

# General Corporate
remove_OG <- SandP_snaps_DT[!SandP_snaps_DT$ORG_ID%in%SandP_snaps_OG$ORG_ID,]
remove_MM <- remove_OG[!remove_OG$ORG_ID%in%SandP_snaps_MM$ORG_ID,]
remove_coal <- remove_MM[!remove_MM$ORG_ID%in%SandP_snaps_coal$ORG_ID,]
remove_Power <- remove_coal[!remove_coal$ORG_ID%in%SandP_snaps_PowerGen$ORG_ID,]
remove_Ag <- remove_Power[!remove_Power$ORG_ID%in%SandP_snaps_Ag$ORG_ID,]
remove_FI <- remove_Ag[!remove_Ag$ORG_ID%in%SandP_snaps_FI$ORG_ID,]
remove_Prop <- remove_FI[!remove_FI$ORG_ID%in%SandP_snaps_Prop$ORG_ID,]
SandP_snaps_GC <- remove_Prop[!remove_Prop$ORG_ID%in%SandP_snaps_Hea$ORG_ID,]

# *****************************************************************************
# Segment Data as per each model  
# *****************************************************************************

sand_P_ratings <- SandP_ratings

grade_blocks = list(MQGCreditMisc::MQ_ratings[1:16],
                    MQGCreditMisc::MQ_ratings[1:8],
                    MQGCreditMisc::MQ_ratings[9:16])

weight_funs <- list("all" = ls_count_weights, "def" = ls_default_weights,
                    "dgd" = ls_downgrade_weights)

# *****************************************************************************
# Generate Z-Score Inputs
# *****************************************************************************

Z_obj_GC <- hist2Zts2(SandP_snaps_GC, data_source = "MQ",
                      grade_block = grade_blocks[[1]], smooth = F,
                      Z_ls_weight_fun = weight_funs[[1]],
                      start_year = 1990, end_year = 2018)

Z_obj_Ag <- hist2Zts2(SandP_snaps_Ag, data_source = "MQ",
                      grade_block = grade_blocks[[1]], smooth = F,
                      Z_ls_weight_fun = weight_funs[[1]],
                      start_year = 1990, end_year = 2018)

Z_obj_OG <- hist2Zts2(SandP_snaps_OG, data_source = "MQ",
                      grade_block = grade_blocks[[1]], smooth = F,
                      Z_ls_weight_fun = weight_funs[[1]],
                      start_year = 1990, end_year = 2018)

Z_obj_MM <- hist2Zts2(SandP_snaps_MM, data_source = "MQ",
                      grade_block = grade_blocks[[1]], smooth = F,
                      Z_ls_weight_fun = weight_funs[[1]],
                      start_year = 1990, end_year = 2018)

Z_obj_coal <- hist2Zts2(SandP_snaps_coal, data_source = "MQ",
                        grade_block = grade_blocks[[1]], smooth = F,
                        Z_ls_weight_fun = weight_funs[[1]],
                        start_year = 1990, end_year = 2018)

Z_obj_PowerGen <- hist2Zts2(SandP_snaps_PowerGen, data_source = "MQ",
                            grade_block = grade_blocks[[1]], smooth = F,
                            Z_ls_weight_fun = weight_funs[[1]],
                            start_year = 1990, end_year = 2018)

# *****************************************************************************
# Save as RDS 
# *****************************************************************************

saveRDS(Z_obj_GC,          "C:/Users/gwong17/OneDrive - Macquarie Group/Personal Folders/Documents/Climate Change 2019/Data/Z_obj_GC_2021V1.Rds")
saveRDS(Z_obj_Ag,          "C:/Users/gwong17/OneDrive - Macquarie Group/Personal Folders/Documents/Climate Change 2019/Data/Z_obj_Ag_2021V2.Rds")
saveRDS(Z_obj_OG,          "C:/Users/mnograd/OneDrive - Macquarie Group/Personal Folders/Documents/Work Stuff/Climate Change 2019/Data/Z_obj_OG.Rds")
saveRDS(Z_obj_MM,          "C:/Users/mnograd/OneDrive - Macquarie Group/Personal Folders/Documents/Work Stuff/Climate Change 2019/Data/Z_obj_MM.Rds")
saveRDS(Z_obj_coal,        "C:/Users/mnograd/OneDrive - Macquarie Group/Personal Folders/Documents/Work Stuff/Climate Change 2019/Data/Z_obj_coal.Rds")
saveRDS(Z_obj_PowerGen,    "C:/Users/mnograd/OneDrive - Macquarie Group/Personal Folders/Documents/Work Stuff/Climate Change 2019/Data/Z_obj_powerGen.Rds")