# orion-4 lipid pre-screening analysis


# Load libraries ------
library(Rcpp)
library(DBI)
library(odbc)
library(RODBC)
library(dplyr)
library(readxl)

library(readr) # import text files (such as csv)
library(readxl) # import excel files
library(tidyverse) # tidyverse package ecosystem for data handling, namely dplyr package
library(VennDiagram) # generate Venn Diagrams
library(RColorBrewer) # generate color palettes
library(ggplot2) # data visualizations
library(patchwork) # combine plots
library(skimr) # general data inspection
library(stringr) # handling strings
library(ggrepel) # repel labels in ggplot
library(ggridges) # generate ridge plots with ggplot
library(broom) # convert shapefile files for use in ggplot
library(magrittr) # get %<>% operator to facilitate synthax
library(hexbin) # required for hexagonal bin ggplots
library(beepr) # get an alarm when code scripts finish
library(scales) # adjust color scales in graphs
library(dtplyr) # data.table backend for dplyr verbs for faster processing
library(tictoc) # for code benchmarking
library(forcats) # to reorder factors
library(tidytext) # reordering characters according to a grouping variable (for plots)
library(ggthemes) # themes for ggplot 
# library(plotly) # generate interactive plots (for inspection of individual points); commented as only needed ad hoc during development and conflicts with ggplot
library(ggh4x) # additional functions to manipulate facets in ggplot
library(ggalluvial) # river plots in ggplot
# library(mmtable2) # to build tables; can be installed as remotes::install_github("ianmoran11/mmtable2")
library(gt) # to build tables
# library(ggsankey) # river plots in ggplot
library(ggpubr) # adding stats to plot
library(ggdark) # themes for ggplot 
library(ggvenn) # venn diagrams in ggplot
library(fmsb) # calculations of K
library(tidyfst) # additional functions for tidyverse syntax
library(xfun)
library(formattable) # formatting html tables
library(tableHTML) # exporting html tables
library(htmltools) # exporting html tables
library(webshot)# exporting html tables
library(gtsummary) # handling html tables; can be installed as remotes::install_github("ddsjoberg/gtsummary")
library(ggside) # for plotting parallel graphs
library(flextable) # for exporting tables to word
library(gtsummary) # alternative method to produce tables
library(officer) # for exporting flextable objects to word
library(lubridate) # dealing with dates
library(survival) # survival analyses
library(survminer) # survival analyses
library(ggfortify) # survival plots
library(irr) # for intraclass coefficient calculations
library(haven) # load stata/sas files
library(maps) # for geospatial visualization
library(viridis) # color gradients for plots
library(Cairo) # export vector images
library(rvg) # exporting vector images
library(svglite)  # exporting vector images 
library(ggsci) # color palletes for dark backgrounds
library(grDevices) # color handling
library(beepr) # sound beep after code run
library(stringr)
library(eeptools)
# source("tools/Table templates/customtab.R") # some tools to customize flextables
# customtab_defaults() 


# Establish database connections ------
con_uk_mirror <- dbConnect(odbc::odbc(),
                 dsn = "orion4_uk_mirror",
                 uid = "guilhermep",
                 pwd = "",
                 timeout = 10)

con_main_mirror <- dbConnect(odbc::odbc(),
                 dsn="orion4_mirror_64",
                 uid = "guilhermep",
                 pwd = "",
                 timeout = 10,
                 believeNRows = FALSE,
                 rows_at_time=999999999)

con_dmc_freeze <- dbConnect(odbc::odbc(),
                  dsn="orion4_2309_blind",
                  uid = "guilhermep",
                  pwd = "",
                  timeout = 10)



# System settings ------

# apply system settings
options(scipen=10000) 

# set working directory 

setwd("K:/QNAP/Orion-4/Team folders/Guilherme/orion4_lipid_pre_screening")



# hardcode temporary directory within QNAP
tempdir <- function() { "K:/QNAP/Orion-4/Team folders/Guilherme/orion4_lipid_pre_screening/Temporary directory" }

# prevent scientific notation in plots
options(scipen=10000) 



# Oxpop theme for powerpoint
oxpop_blue_panel<- (
  # dark_mode(
  #theme_fivethirtyeight(base_size = 20))+
  theme(plot.background = element_rect(fill = "transparent", color=NA), # the color argument removes white margin
        panel.background = element_rect(fill = "transparent"), # for ppt BG color use #0d1d41
        
        panel.grid.major = element_line(color = "grey90", size=0.1, linetype = "dashed"),
        panel.grid.minor = element_line(color = "grey90", size=0.1, linetype = "dashed"),
        
        strip.background = element_blank(),
        legend.background = element_blank(),
        legend.key=element_blank(),
        
        axis.title.y = element_text(family="Mulish",
                                    color = "white"),
        axis.title.x = element_text(family="Mulish",
                                    color = "white"),
        axis.text.y = element_text(family="Mulish",
                                   color = "white"),
        axis.text.x = element_text(family="Mulish",
                                   color = "white"),
        plot.subtitle = element_text(hjust=0),
        plot.caption = element_text(hjust=0),
        
        strip.text = element_text(size=20, color="white"),
        
        axis.ticks = element_line(color="white"),
        
        text = element_text(family="Mulish",color = "White", size=25),
        panel.border = element_blank()
  )
)





## default plotting settings ------


update_geom_defaults("text", list(colour="black",
                                  family="Mulish"))

theme_set(theme_gray(base_size=25))
theme_update(text=element_text(family="Mulish"))

# Extract data -------


## study centres-----
centres<-dbGetQuery(con_uk_mirror,
                    "select 
                    address_id, 
                    centre_id, 
                    centre_name, 
                    country_id 
                    
                    
                    from centre
                    
                    where country_id = 11 and centre_id <41500")%>%
  mutate(centre_id=as.character(centre_id))

ORION_4_sites <- read_excel("Tools/ORION-4 sites.xlsx", 
                            col_types = c("text", "text", "text", 
                                          "text", "skip"))
centres%<>%
  left_join(ORION_4_sites, by=c("centre_id"="LCC ID"))

rm(ORION_4_sites)

centres_all<-dbGetQuery(con_uk_mirror,
                       "select 
                    address_id, 
                    centre_id, 
                    centre_name, 
                    country_id 
                    
                    
                    from centre")%>%
  mutate(centre_id=as.character(centre_id))



## participant details -----

participant_details<-
  dbGetQuery(
    con_dmc_freeze,
    "select 
    participant_id,
    date_randomized,
    last_visit_date,
    study_status,
    date_screened
    
    from participant_status"
  )%>%
  filter(!is.na(date_screened))%>%
  mutate(across(c(date_randomized, last_visit_date, date_screened), ~as.Date(., format="%Y-%m-%d")))%>%
  # mutate(rand_month = as.Date(paste0(str_sub(as.character(date_randomized), 1, 7), "-01"),format="%Y-%m-%d"))%>%
  mutate(participant_id=as.character(participant_id))%>%
  
  left_join(
    
    dbGetQuery(con_dmc_freeze,
               "select
               usubjid as participant_id,
               siteid as centre_id,
               age,
               sex,
               country
               
               from dm")%>%
      mutate(participant_id=as.character(participant_id))
    
    
    
  )%>%
  filter(country=="GBR")




rand_dates<-participant_details%>%
  filter(!is.na(date_randomized))%>%
  select(participant_id, date_randomized)

### all invites ------

# participant_details_all<-
#   dbGetQuery(
#     con_main_mirror,
#     "select participant_id as pt_id, centre_id from participant_centre"
#   )%>%
#   mutate(pt_id=as.character(pt_id))

## identify screenings in England -----

scottish_sites<-c(
  "41157",  
  "41171" ,
  "41183" ,
  "41214" ,
  "41258" ,
  "41259" ,
  "41405" ,
  "41420" ,
  "41439")

# Scottish sites:
# 41157  Edinburgh
# 41171 Aberdeen
# 41183 Kircaldy
# 41214 Dundee
# 41258 Lanarkshire
# 41259 Inverness
# 41405 Crosshouse
# 41420 Glasgow
# 41439 Forth Valley

welsh_sites<-c(
  "41180",
  "41233",
  "41274",
  "41404")
# Welsh sites:
# 41180 Swansea/Singleton
# 41233 Cardiff
# 41274 Glan Clwyd
# 41404 Wrexham 



screening_ids_england<-
  participant_details%>%
  left_join(centres)%>%
  filter(!centre_id %in% union(scottish_sites, welsh_sites),
         centre_id<41500)%>%
  distinct(participant_id)%>%
  .[[1]]


length(screening_ids_england)
# 23972

## trust cholesterol -----


trust_labs<-
  dbGetQuery(con_uk_mirror,
             "select 
             pt_id, 
             chol_value,
             chol_type,
             ANSIDATE(chol_date) as chol_date,
             trust_code
             
             from pt_uk_chol
             
             ")%>%
  filter(trust_code!="7A1")


## screening cholesterol -----

# screening_labs<-
#   dbGetQuery(con_main_mirror,
#              "select 
#              pt_id, 
#             nvarchar(deunicode64(var_value_u64)) as result,
# 
#              
#              var_name,
#              changed_when
#              
#              from form_screening
#              
#              where var_name = 'cholResult'
#              
#              ")%>%
#   mutate(pt_id=as.character(pt_id),
#          date=as.Date(changed_when, format="%Y-%m-%d"))%>%
#   select(-var_name, -changed_when)%>%
#   filter(pt_id %in% screening_ids_england)

screening_labs<-
  read.csv("Datasets/screening_labs.csv")%>%
  mutate(pt_id=as.character(pt_id),
         date=as.Date(changed_when, format="%Y-%m-%d"))%>%
  select(-var_name, -changed_when)%>%
  filter(pt_id %in% screening_ids_england)





## visit record -----


# visits_table<-
#   dbGetQuery(con_main_mirror,
#              "select participant_id,
# visit_id,
# form, 
# visit_end,
# how_conducted
# 
# from visits")%>%
#   mutate(participant_id=as.character(participant_id))%>%
#   mutate(visit_date = as.Date(visit_end, format="%Y-%m-%d"))


visits_table<-
  read.csv("Datasets/visits.csv")%>%
  mutate(participant_id=as.character(participant_id))%>%
  mutate(visit_date = as.Date(visit_end, format="%Y-%m-%d"))






## medical history (at screening) -----

medical_history<-
  dbGetQuery(con_main_mirror,
             "select * from mh")%>%
  rename(pt_id=usubjid)%>%
  filter(pt_id %in% screening_ids_england)

## medications at screening ------

# drug categories
# drugs<-read_csv("tools/drugsman_categories.csv")%>%
#   rename(med_drug_text=name_u64)%>%
#   mutate(across(c(term_id, read_code), ~as.character(.)))%>%
#   select(-cat_id)
# 
# pt_med_drug<-
#   dbGetQuery(
#     con_main_mirror,
#     "select
#     med_form_id,
#     med_drug_code as term_id,
#     nvarchar(deunicode64(med_drug_text_u64)) as med_drug_text,
#     drug_dosage,
#     changed_when
# 
#     from pt_med_drug"
#   )
# 
# pt_med_form<-
#   dbGetQuery(
#     con_main_mirror,
#     "select
#     pt_id,
#     med_form_id,
#     visit_type,
#     form_id,
#     changed_when
# 
#     from pt_med_form"
#   )
# 
# # visit types: 2 - screening, 3 - randomisation, 4 - follow-up
# 
# 
# # load list of coded free-text medications
# 
# free_text_dictionary<-read_csv("tools/cosmas_coded_terms.csv",
#                                trim_ws = F)
# 
# 
# 
# 
# # drug labels
# 
# 
# drug_labels_order<-c("Statins",
#                      "Ezetimibe",
#                      "PCSK9 inhibitors",
#                      "Inclisiran",
#                      "Bempedoic acid",
#                      "Icosapent ethyl",
#                      "Fibrates",
#                      "Resins",
#                      "Insulin",
#                      "Metformin",
#                      "Sulphonylureas",
#                      "Thiazolideniones",
#                      "DPP-4 inhibitors",
#                      "GLP-1 agonists",
#                      "SGLT2 inhibitors",
#                      "Acarbose or similar",
#                      "ACE inhibitors",
#                      "Angiotensin-receptor blockers",
#                      "Mineralocorticoid receptor antagonists",
#                      "Beta-adrenergic blockers",
#                      "Calcium-channel blockers",
#                      "Loop diuretics",
#                      "Thiazide diuretics",
#                      "Aspirin",
#                      "Other antiplatelets",
#                      "Vitamin K antagonists",
#                      "NOACs"
# )
# 
# 
# orion_drugs <- tibble(
#   cat_name = c("acarbose or simi",
#                "ace inhibitor",
#                "aldosterone anta",
#                "arb",
#                "beta-blocker",
#                "biguanide",
#                "ca channel block",
#                "dpp4 inhibitor",
#                "ezetimibe",
#                "GLP-1 agonist",
#                "Insulin",
#                "loop diuretic",
#                "inclisiran",
#                "PCSK9 inhibitors",
#                "SGLT2 Inhibitors",
#                "statin",
#                "Sulphonylurea",
#                "thiazide(like)",
#                "thiazolidinedion",
#                "fibrate",
#                "resin",
#                "aspirin",
#                "other antiplatel",
#                "warfarin-type",
#                "noac",
#                "atorvastatin",
#                "fluvastatin",
#                "pravastatin",
#                "simvastatin",
#                "rosuvastatin",
#                "lovastatin",
#                "pitavastatin",
#                "icopsapent ethyl",
#                "bempedoic acid"),
#   labels = c("Acarbose or similar",
#              "ACE inhibitors",
#              "Mineralocorticoid receptor antagonists",
#              "Angiotensin-receptor blockers",
#              "Beta-adrenergic blockers",
#              "Metformin",
#              "Calcium-channel blockers",
#              "DPP-4 inhibitors",
#              "Ezetimibe",
#              "GLP-1 agonists",
#              "Insulin",
#              "Loop diuretics",
#              "Inclisiran",
#              "PCSK9 inhibitors",
#              "SGLT2 inhibitors",
#              "Statins",
#              "Sulphonylureas",
#              "Thiazide diuretics",
#              "Thiazolideniones",
#              "Fibrates",
#              "Resins",
#              "Aspirin",
#              "Other antiplatelets",
#              "Vitamin K antagonists",
#              "NOACs",
#              "Atorvastatin",
#              "Fluvastatin",
#              "Pravastatin",
#              "Simvastatin",
#              "Rosuvastatin",
#              "Lovastatin",
#              "Pitavastatin",
#              "Icosapent ethyl",
#              "Bempedoic acid"))%>%
#   mutate(labels=factor(labels, levels=drug_labels_order))
# 
# # export into dataframe
# screening_drugs<-
# 
#   # start with med forms
#   pt_med_form%>%
#   select(participant_id=pt_id, med_form_id, visit_type, form_id)%>%
#   mutate(participant_id=as.character(participant_id))%>%
# 
#   # join meds recorded in each form
#   left_join(pt_med_drug%>%
#               select(med_form_id, term_id, med_drug_text, drug_dosage))%>%
# 
#   # join free text coding
#   left_join(free_text_dictionary,
#             by=c("med_drug_text"="freetext_desc_u64"))%>%
#   rename(read_code_free_text = read_code)%>%
# 
#   # join read codes
#   ungroup()%>%
#   left_join(drugs%>%distinct(term_id, read_code)%>%mutate(term_id=as.integer(term_id)), by=c("term_id"))%>%
#   mutate(read_code_free_text=as.character(read_code_free_text))%>%
#   mutate(read_code = if_else(is.na(read_code), read_code_free_text, read_code))%>%
# 
#   # join drug categories
#   ungroup()%>%
#   left_join(drugs%>%distinct(read_code, cat_name), by=c("read_code"))%>%
# 
# 
#   select(participant_id,
#          med_drug_text,
#          drug_dosage,
#          visit_type,
#          form_id,
#          cat_name)%>%
# 
#   # join drug group labels
#   left_join(orion_drugs)%>%
#   filter(!is.na(labels))%>%
# 
# 
#   # restrict to screening visit
# 
#   filter(visit_type==2)%>%
# 
#   # join visit dates
#   left_join(visits_table%>%select(participant_id, form, visit_id, visit_date)%>%mutate(participant_id=as.character(participant_id)),
#             by=c("form_id"="visit_id", "participant_id"="participant_id"))%>%
#   distinct(participant_id,
#            med_drug_text,
#            drug_dosage,
#            labels,
#            visit_date)%>%
#   filter(participant_id %in% screening_ids_england)



## screening statin

screening_statins<-
  dbGetQuery(
  con_main_mirror,
  "select
    form_id,
    visit_type_id,
    intensity

    from pt_meds_at_visit
  
  where visit_type_id = 2"
)%>%
  left_join(
  dbGetQuery(
    con_main_mirror,
    "select
    pt_id,
    -- med_form_id,
    visit_type,
    form_id,
    changed_when

    from pt_med_form"),
  by=c("form_id")
  )%>%
  select(pt_id, statin_intensity=intensity)%>%
  mutate(pt_id=as.character(pt_id),
         statin_intensity=case_when(statin_intensity=="highTreatEligible" ~ "High",
                                    statin_intensity=="modTreatEligible" ~ "Moderate/low",
                                    statin_intensity=="noTreatEligible" ~ "None"))
  




## invites -----


mailings<-dbGetQuery(con_uk_mirror,
                     "select 
                     m.mailing_id as mailing_id,
                     m.list_id as list_id, 
                     m.recipient_id as pt_id,
                     ml.date_last_merged as date
                     
                     
                     from mailing m
                     
                     left join mailing_list ml 
                     
                     on m.list_id = ml.list_id")%>%
  mutate(pt_id=as.character(pt_id))%>%
  left_join(
    dbGetQuery(con_uk_mirror,
               "select list_id, centre_id from invite_mailing_list")%>%
      mutate(centre_id=as.character(centre_id)))%>%
  left_join(centres_all%>%select(centre_id, centre_name))%>%
  left_join(centres%>%select(centre_id, trust_code=`NHS Trust Code`))%>%
  mutate(date=as.Date(date, format="%Y-%m-%d"))%>%
  distinct(centre_id, centre_name, trust_code, date)


mailings_individual<-dbGetQuery(con_uk_mirror,
                                "select 
                     m.mailing_id as mailing_id,
                     m.list_id as list_id, 
                     m.recipient_id as pt_id,
                     ml.date_last_merged as date
                     
                     
                     from mailing m
                     
                     left join mailing_list ml 
                     
                     on m.list_id = ml.list_id")%>%
  mutate(pt_id=as.character(pt_id))

  

## invite_data

invite_data<-
  dbGetQuery(con_uk_mirror,
             "select 
           
           pt.pt_id,
           dob = pt.date_of_birth,
           pt.sex,
           centre_id,
           mi_present,
           mi_date,
           stroke_present,
           stroke_date,
           pvd_surgery_present,
           pvd_surgery_date,
           date_valid_from
           
           from pt_uk pt
           
           left join pt_uk_baseline b 
           
           on pt.pt_id = b.pt_id
             
            left join pt_uk_raw r 
            
            on pt.pt_id = r.pt_id
             
             ")

invite_data%<>%
  mutate(invited=if_else(pt_id %in% mailings_individual$pt_id, "Y", "N"))

invite_data%<>%
  mutate(pt_id=as.character(pt_id))

invite_data%<>%
  left_join(mailings_individual%>%select(pt_id, invite_date=date))






## staff -----




## centrallised pre-screening dates -----


pre_screening_details <- read_excel("Tools/trust_details.xlsx", 
                            col_types = c("text", "text", "text", 
                                          "numeric", "date", "date", "numeric", 
                                          "text", "text", "text", "numeric", 
                                          "text"))




## recruitment (by site) -----

recruitment <- read_csv("Datasets/recruitment.csv", 
                        col_types = cols(run_in_prop = col_skip()))%>%
  filter(!centre_id %in% union(scottish_sites, welsh_sites),
         centre_id<41500)





## failed screenings -----



participant_status<-
  read.csv("Datasets/participant_status.csv")%>%
  # filter(study_status!=10)%>%
  left_join(read.csv("Datasets/study_status_lookup.csv")%>%
              select(status_id, status),
            by=c("study_status"="status_id"))%>%
  mutate(study_status_cat= case_when(study_status==10 ~ "Registered",
                                     study_status%in%c(18,19,20)~"Screen failure/incomplete", 
                                     study_status%in%c(21,22,23,24,25,30,40)~"Withdrawal/stopped during run-in",
                                     study_status==50~"Randomised",
                                     study_status==60 ~ "Consent withdrawal"),
         pt_id=as.character(participant_id))%>%
  select(-participant_id)%>%
  filter(centre_id<41500)





# 
# 
# participant_status<-
#   dbGetQuery(con_main_mirror,
#              "select 
#              
#              participant_id,
#              study_status,
#              date_screened,
#              date_randomized
#              
#              from participant_status")%>%
#   # filter(study_status %in% c(20,50))%>%
#   filter(study_status!=10)%>%
#   mutate(study_status_cat= case_when(study_status%in%c(18,19,20)~"Screen failure/incomplete", 
#                                      study_status==50~"Randomised",
#                                      TRUE~"Other"),
#          pt_id=as.character(participant_id))%>%
#   select(-participant_id)
# 









# SQL script to extract screening appointments cancelled as failed pre-screening (presumably due to cholesterol)

# select 
# 
# ps.participant_id, 
# date_registered,
# reason_id,
# inserted_when as cancelled_date,
# prw.who_id,
# prw.descr as who_refused
# 
# 
# from participant_status ps
# 
# left join pt_refusal pr on ps.participant_id = pr.participant_id
# 
# left join pt_refusal_who prw on pr.who_id = prw.who_id 
# 
# where 
# reason_id =5 and 
# date_registered is not null and 
# date_registered < inserted_when

cancelled_screening_appts <- read_csv("Datasets/cancelled_screening_appts.csv", 
                                      col_types = cols(reason_id = col_skip(), 
                                                       who_id = col_skip()))
participant_status%<>%
  mutate(failed_pre_screening=if_else(pt_id %in% cancelled_screening_appts$participant_id,"Y","N"))

rm(cancelled_screening_appts)

# General overview ------

## study population -----


hospital_ids<-
  trust_labs%>%
  distinct(pt_id)%>%
  .[[1]]

length(hospital_ids)
# 161712


screening_labs_ids<-
  screening_labs%>%
  filter(!is.na(result))%>%
  distinct(pt_id)%>%
  .[[1]]

length(screening_labs_ids)
# 14509


overlapping_ids<-intersect(hospital_ids, screening_labs_ids)

length(overlapping_ids)
# 3528








## trusts -----

trust_labs%>%distinct(trust_code)%>%nrow()
# 16 (excludes one Welsh trust)


## number of cholesterol results (per participant) ----

trust_labs%>%
  count(pt_id)%>%
  count(n)%>%
  mutate(prop=round(nn/sum(nn)*100,1))%>%
  # arrange(desc(n))%>%

  ggplot(aes(n, nn))+
  geom_col(color="black")+
  geom_text(aes(label=paste0(nn, " (", prop,"%)")),
            vjust=-1)


trust_labs%>%
  count(pt_id)%>%
  count(n)%>%
  mutate(group=case_when(n<=5 ~ as.character(n),
                         between(n,6,10) ~"6-10",
                         n>10 ~ ">10" ))%>%
  group_by(group)%>%
  summarise(count=sum(nn))%>%
  mutate(prop=round(count/sum(count)*100,1))%>%View()


## distributions (total) ------

individual_chol_values_all<-screening_labs%>%
  select(pt_id, result, date)%>%
  mutate(Source="POC")%>%
  rbind(
    trust_labs%>%
      select(pt_id, result=chol_value, date=chol_date)%>%
      mutate(Source="Hospital")
    
    
  )%>%
  mutate(result=as.numeric(result))

individual_chol_values_all%>%
  ggplot(aes(result, 
             fill=Source))+
  geom_density(color="black",
               alpha=0.2)+
  geom_vline(data=  
               individual_chol_values_all%>%
               filter(!is.na(result))%>%
               group_by(Source)%>%
               summarise(median=median(result),
                         Q1=quantile(result,0.25),
                         Q3=quantile(result,0.75)),
             aes(xintercept=median,
                 color=Source),
             linetype="dashed",
             size=2)+
  scale_x_continuous(breaks=seq(0,15,1))+
  theme(legend.position = "bottom")



# 1. Feasibility ------

feasibility <- read_excel("Datasets/feasibility.xlsx", 
                          col_types = c("text", "text", "text", 
                                        "text", "text", "date", "date", "date", 
                                        "numeric", "date", "numeric", "date"))%>%
  filter(!centre_id %in% union(scottish_sites, welsh_sites),
         centre_id<41500)%>%
  filter(centre_id%in%recruitment$centre_id)


setdiff(recruitment$centre_id, feasibility$centre_id) # oxford central site

## time to data reception -----

feasibility%>%
  distinct(trust_code,
           date_first_contact, 
           date_received)%>%
  mutate(interval=difftime(date_received,date_first_contact, units="days"))%>%
  mutate(interval=interval/30)%>%
  ggplot(aes(interval))+
  geom_histogram(color="black")+
  scale_x_continuous(limits=c(0,NA))+
  labs(x="Interval between first contact and data reception (in months)",
       y="Number of trusts")

feasibility%>%
  distinct(trust_code,
           date_first_contact, 
           date_received)%>%
  mutate(interval=difftime(date_received,date_first_contact, units="days"))%>%
  mutate(interval=as.numeric(round(interval/30,1)))%>%
  filter(!is.na(interval))%>%
  summarise(min=min(interval),
            max=max(interval),
            mean=round(mean(interval),1),
            SD=round(sd(interval),1),
            median=median(interval),
            Q1=quantile(interval, 0.25),
            Q3=quantile(interval, 0.75))%>%
  View()




feasibility%>%
  filter(!is.na(date_first_contact))%>%
  select(trust_code,
         outcome,
         date_first_contact, 
         date_received,
         date_finalised,
         date_contract_received)%>%
  mutate(time_to_data_reception=as.numeric(difftime(date_received,date_first_contact, units="days")),
         time_to_contract_reception=as.numeric(difftime(date_contract_received,date_first_contact, units="days")),
         time_to_finalised=as.numeric(difftime(date_finalised,date_first_contact, units="days")))%>%
  mutate(time_contract_to_data_extraction=time_to_data_reception-time_to_contract_reception)%>%
  pivot_longer(starts_with("time"), names_to = "metric", values_to = "value")%>%
  filter(!is.na(value))%>%
  group_by(outcome, metric)%>%
  summarise(min=min(value),
            max=max(value),
            mean=round(mean(value),1),
            SD=round(sd(value),1),
            median=median(value),
            Q1=quantile(value, 0.25),
            Q3=quantile(value, 0.75))%>%
  View()


### timelines for trusts with data----

# ordered by site number
feasibility%>%
  filter(!is.na(date_first_contact))%>%
  select(trust_code,
         outcome,
         date_first_contact, 
         date_received,
         date_finalised,
         date_contract_received)%>%
  mutate(time_to_data_reception=as.numeric(difftime(date_received,date_first_contact, units="days")),
         time_to_contract_reception=as.numeric(difftime(date_contract_received,date_first_contact, units="days")),
         time_to_finalised=as.numeric(difftime(date_finalised,date_first_contact, units="days")))%>%
  mutate(time_contract_to_data_extraction=time_to_data_reception-time_to_contract_reception)%>%
  mutate(trust = reorder(trust_code, as.numeric(time_to_finalised)))%>%
  pivot_longer(starts_with("time"), names_to = "metric", values_to = "value")%>%
  filter(!is.na(value),
         outcome %in% c("Data uploaded", "Issues with data"))%>%
  distinct(trust_code, metric, .keep_all=T)%>%

  filter(metric %in% c("time_to_contract_reception", "time_contract_to_data_extraction"))%>%
  mutate(metric=factor(if_else(metric=="time_to_contract_reception", "Contract", "Data extraction"),
                          levels=c("Contract","Data extraction")))%>%
  mutate(value=value/30)%>%
  
  ggplot(aes(trust, value, fill=fct_rev(metric)))+
  geom_col()+
  theme(axis.text.x=element_blank(),
        legend.position="bottom")+
  labs(x="Hospital Trust",
       y="Months",
       fill="Process")+
  guides(fill = guide_legend(reverse=TRUE))+
  scale_y_continuous(limits=c(0,7))


ggsave("outputs/figures/paper/timelines_sites_with_data_by_total_time.png", 
       last_plot(),
       width=40,
       height=25,
       dpi = "retina",
       units="cm")


## slide version


# ordered by site number
feasibility%>%
  filter(!is.na(date_first_contact))%>%
  select(trust_code,
         outcome,
         date_first_contact, 
         date_received,
         date_finalised,
         date_contract_received)%>%
  mutate(time_to_data_reception=as.numeric(difftime(date_received,date_first_contact, units="days")),
         time_to_contract_reception=as.numeric(difftime(date_contract_received,date_first_contact, units="days")),
         time_to_finalised=as.numeric(difftime(date_finalised,date_first_contact, units="days")))%>%
  mutate(time_contract_to_data_extraction=time_to_data_reception-time_to_contract_reception)%>%
  mutate(trust = reorder(trust_code, as.numeric(time_to_finalised)))%>%
  pivot_longer(starts_with("time"), names_to = "metric", values_to = "value")%>%
  filter(!is.na(value),
         outcome %in% c("Data uploaded", "Issues with data"))%>%
  distinct(trust_code, metric, .keep_all=T)%>%
  
  filter(metric %in% c("time_to_contract_reception", "time_contract_to_data_extraction"))%>%
  mutate(metric=factor(if_else(metric=="time_to_contract_reception", "Contract", "Data extraction"),
                       levels=c("Contract","Data extraction")))%>%
  mutate(value=value/30)%>%
  
  ggplot(aes(trust, value, fill=fct_rev(metric)))+
  oxpop_blue_panel+
  geom_col()+
  theme(axis.text.x=element_blank(),
        legend.position="right")+
  labs(x="Hospital Trust",
       y="Months",
       fill="Process")+
  guides(fill = guide_legend(reverse=TRUE))+
  scale_y_continuous(limits=c(0,7))


ggsave("outputs/figures/slides/timelines_sites_with_data_by_total_time.png", 
       last_plot(),
       width=40,
       height=25,
       dpi = "retina",
       units="cm")


# ordered by site size

feasibility%>%
  filter(!is.na(date_first_contact))%>%
  select(trust_code,
         outcome,
         date_first_contact, 
         date_received,
         date_finalised,
         date_contract_received)%>%
  mutate(time_to_data_reception=as.numeric(difftime(date_received,date_first_contact, units="days")),
         time_to_contract_reception=as.numeric(difftime(date_contract_received,date_first_contact, units="days")),
         time_to_finalised=as.numeric(difftime(date_finalised,date_first_contact, units="days")))%>%
  mutate(time_contract_to_data_extraction=time_to_data_reception-time_to_contract_reception)%>%
  mutate(trust = reorder(trust_code, as.numeric(time_to_finalised)))%>%
  pivot_longer(starts_with("time"), names_to = "metric", values_to = "value")%>%
  filter(!is.na(value),
         outcome %in% c("Data uploaded", "Issues with data"))%>%
  distinct(trust_code, metric, .keep_all=T)%>%
  
  filter(metric %in% c("time_to_contract_reception", "time_contract_to_data_extraction"))%>%
  mutate(metric=factor(if_else(metric=="time_to_contract_reception", "Contract", "Data extraction"),
                       levels=c("Contract","Data extraction")))%>%
  mutate(value=value/30)%>%
  
  left_join(pre_screening_details%>%
              select(trust_code, n_requested))%>%
  mutate(trust=reorder(trust_code, n_requested))%>%
  
  ggplot(aes(trust, value, fill=fct_rev(metric)))+
  geom_col()+
  theme(axis.text.x=element_blank(),
        legend.position="bottom")+
  labs(x="Trust (ordered by size of invitee pool)",
       y="Months",
       fill="Process")+
  guides(fill = guide_legend(reverse=TRUE))+
  scale_y_continuous(limits=c(0,7))




# ordered by invite date

feasibility%>%
  filter(!is.na(date_first_contact))%>%
  select(trust_code,
         outcome,
         date_first_contact, 
         date_received,
         date_finalised,
         date_contract_received)%>%
  mutate(time_to_data_reception=as.numeric(difftime(date_received,date_first_contact, units="days")),
         time_to_contract_reception=as.numeric(difftime(date_contract_received,date_first_contact, units="days")),
         time_to_finalised=as.numeric(difftime(date_finalised,date_first_contact, units="days")))%>%
  mutate(time_contract_to_data_extraction=time_to_data_reception-time_to_contract_reception)%>%
  mutate(trust = reorder(trust_code, as.numeric(time_to_finalised)))%>%
  pivot_longer(starts_with("time"), names_to = "metric", values_to = "value")%>%
  filter(!is.na(value),
         outcome %in% c("Data uploaded", "Issues with data"))%>%
  distinct(trust_code, metric, .keep_all=T)%>%
  
  filter(metric %in% c("time_to_contract_reception", "time_contract_to_data_extraction"))%>%
  mutate(metric=factor(if_else(metric=="time_to_contract_reception", "Contract", "Data extraction"),
                       levels=c("Contract","Data extraction")))%>%
  mutate(value=value/30)%>%
  
  mutate(trust=reorder(trust_code, date_first_contact))%>%
  
  ggplot(aes(trust, value, fill=fct_rev(metric)))+
  geom_col()+
  theme(axis.text.x=element_blank(),
        legend.position="bottom")+
  labs(x="Trust (ordered by date of first contact)",
       y="Months",
       fill="Process")+
  guides(fill = guide_legend(reverse=TRUE))+
  scale_y_continuous(limits=c(0,7))


























# summary stats

feasibility%>%
  filter(!is.na(date_first_contact))%>%
  select(trust_code,
         outcome,
         date_first_contact, 
         date_received,
         date_finalised,
         date_contract_received)%>%
  mutate(time_to_data_reception=as.numeric(difftime(date_received,date_first_contact, units="days")),
         time_to_contract_reception=as.numeric(difftime(date_contract_received,date_first_contact, units="days")),
         time_to_finalised=as.numeric(difftime(date_finalised,date_first_contact, units="days")))%>%
  mutate(time_contract_to_data_extraction=time_to_data_reception-time_to_contract_reception)%>%
  mutate(trust = reorder(trust_code, as.numeric(time_to_finalised)))%>%
  pivot_longer(starts_with("time"), names_to = "metric", values_to = "value")%>%
  filter(!is.na(value),
         outcome %in% c("Data uploaded", "Issues with data"))%>%
  distinct(trust_code, metric, .keep_all=T)%>%
  
  filter(metric %in% c("time_to_contract_reception", "time_contract_to_data_extraction"))%>%
  mutate(metric=factor(if_else(metric=="time_to_contract_reception", "Contract", "Data extraction"),
                       levels=c("Contract","Data extraction")))%>%
  mutate(value=round(value/30,1))%>%
  
  rename(Process=metric)%>%
  group_by(Process)%>%
  summarise(min=min(value),
            max=max(value),
            mean=round(mean(value),1),
            SD=round(sd(value),1),
            median=median(value),
            Q1=quantile(value, 0.25),
            Q3=quantile(value, 0.75))%>%
  View()


## time to finalised (by outcome) ----



feasibility%>%
  filter(!is.na(date_first_contact))%>%
  select(trust_code,
         outcome,
         date_first_contact, 
         date_received,
         date_finalised,
         date_contract_received)%>%
  mutate(time_to_finalised=as.numeric(difftime(date_finalised,date_first_contact, units="days")))%>%
  mutate(outcome=if_else(str_detect(outcome, "Not"), "Not feasible/willing", outcome))%>%
  mutate(outcome=if_else(outcome  %in% c("Data uploaded", "Issues with data"), "Data uploaded/\nissues with data", outcome))%>%
  mutate(outcome=if_else(outcome=="Ongoing", "Abandoned",outcome))%>%
  mutate(outcome=factor(outcome, levels=c("Data uploaded/\nissues with data",
                                          "Not feasible/willing",
                                          "Abandoned",
                                          "Ran out of data")))%>%
  distinct(trust_code, time_to_finalised, .keep_all=T)%>%
  mutate(time_to_finalised=round(time_to_finalised/30,1))%>%
  group_by(outcome)%>%
  summarise(min=min(time_to_finalised),
            max=max(time_to_finalised),
            mean=round(mean(time_to_finalised),1),
            SD=round(sd(time_to_finalised),1),
            median=median(time_to_finalised),
            Q1=quantile(time_to_finalised, 0.25),
            Q3=quantile(time_to_finalised, 0.75))%>%
  View()

  

feasibility%>%
  filter(!is.na(date_first_contact))%>%
  select(trust_code,
         outcome,
         date_first_contact, 
         date_received,
         date_finalised,
         date_contract_received)%>%
  mutate(time_to_finalised=as.numeric(difftime(date_finalised,date_first_contact, units="days")))%>%
  mutate(outcome=if_else(str_detect(outcome, "Not"), "Not feasible/willing", outcome))%>%
  mutate(outcome=if_else(outcome  %in% c("Data uploaded", "Issues with data"), "Data uploaded/\nissues with data", outcome))%>%
  mutate(outcome=if_else(outcome=="Ongoing", "Abandoned",outcome))%>%
  distinct(trust_code, time_to_finalised, .keep_all=T)%>%
  
  mutate(outcome=factor(outcome, levels=c("Data uploaded/\nissues with data",
                                          "Not feasible/willing",
                                          "Abandoned",
                                          "Ran out of data")))%>%
  mutate(time_to_finalised=time_to_finalised/30)%>%
  
  ggplot(aes(x=time_to_finalised,y=outcome, fill=outcome))+
  geom_boxplot()+
  stat_summary(fun=mean, colour="black", geom="point", 
               shape=18, size=3, show.legend=FALSE) + 
  theme(legend.position="none")+
  scale_y_discrete(limits=rev)+
  labs(x="Time to process finalised (in months)",
       y="Process outcome")+
  scale_x_continuous(limits=c(0,NA))

  
  

## % of pts returned -----

pre_screening_details%>%
  filter(!is.na(n_received))%>%
  distinct(trust_code,n_requested, n_received)%>%
  mutate(prop=round(n_received/n_requested*100),1)%>%
  ggplot(aes(prop))+
  geom_histogram(color="black")+
  scale_x_continuous(limits=c(0,100))+
  labs(x="Proportion of requested individuals with returned data",
       y="Number of trusts")


pre_screening_details%>%
  filter(!is.na(n_received))%>%
  distinct(trust_code,n_requested, n_received)%>%
  mutate(prop=round(n_received/n_requested*100),1)%>%
  summarise(min=min(prop),
            max=max(prop),
            mean=round(mean(prop),1),
            SD=round(sd(prop),1),
            median=median(prop),
            Q1=quantile(prop, 0.25),
            Q3=quantile(prop, 0.75))%>%
  View()


## with contract signed (by outcome) ----

feasibility%>%
  group_by(outcome)%>%
  summarise(count=n(outcome[!is.na(date_contract_received)]),
            prop=count/n())


feasibility%>%
  arrange(outcome, date_contract_received)%>%
  View()  



## baseline characteristics tables -----

### data at invitation for cholesterol available vs not, and invited vs not invited -------

sect_properties <- prop_section(
  page_size = page_size(orient = "landscape",
                        # width = 30,
                        # height = 11.7  
  ),
  type = "continuous",
  page_margins = page_mar())

invite_data%>%
  distinct(pt_id, .keep_all = T)%>%
  # head()%>%
  # filter(centre_id %in%
  #        (feasibility%>%
  #        filter(outcome=="Data uploaded")%>%
  #        select(centre_id)%>%
  #        .[[1]]))%>%
  # filter(pt_id %in% trust_labs$pt_id)%>%
  
  mutate(centre_with_data = if_else(
    centre_id %in%
      (feasibility%>%
         filter(outcome=="Data uploaded")%>%
         select(centre_id)%>%
         .[[1]]),
    "Y","N"),
    pt_with_data = if_else(
      pt_id %in% trust_labs$pt_id,
      "Y", "N"))%>%
  mutate(invited=if_else(invited=="Y","Invited", "Not invited"))%>%
  mutate(invited=factor(invited, c("Invited","Not invited")))%>%
  
  mutate(data_availability=case_when(
    centre_with_data == "Y" & pt_with_data == "Y" ~"Data available",
    TRUE~"Data unavailable"))%>%
  
  mutate(age_at_data_extraction=as.integer(age_calc(as.Date(dob, format="%y-%m-%d"),enddate=as.Date(date_valid_from, format="%Y-%m-%d"),units="years")))%>%
  select(age_at_data_extraction,
         data_availability,
         sex,
         mi_present,
         stroke_present,
         pvd_surgery_present,
         invited)%>%
  mutate(mi_present=if_else(mi_present==1, "Yes", "No"),
         stroke_present=if_else(stroke_present==1, "Yes", "No"),
         pvd_surgery_present=if_else(pvd_surgery_present==1, "Yes", "No"))%>%
  mutate(mi_present=replace_na(mi_present, "No"),
         stroke_present=replace_na(stroke_present, "No"),
         pvd_surgery_present=replace_na(pvd_surgery_present, "No"))%>%

  tbl_strata(
    strata=data_availability,
    ~.x%>%
    tbl_summary(by="invited",
              missing="no",
              statistic = list(all_continuous() ~ "{mean} ({sd})"),
              value=list(sex~"F",
                         mi_present~"Yes",
                         stroke_present~"Yes",
                         pvd_surgery_present~"Yes"),
              label=list(age_at_data_extraction ~ "Age (at point of central data extraction)",
                         sex ~ "Female",
                         mi_present ~ "Myocardial infarction (previous)",
                         stroke_present ~ "Stroke (previous)",
                         pvd_surgery_present ~ "Peripheral vascular disease (previous)"))
    )%>%
  # modify_spanning_header(c("stat_1", "stat_2") ~ "**Invited**") %>%
  as_flex_table()%>%
  save_as_docx(path="Outputs/Tables/baseline_characteristics_pre_screening.docx",
               pr_section = sect_properties)




### data at screening for cholesterol available vs not (sites with data) vs sites without data ------

medical_history%>%
  # head()%>%
  select(pt_id,mhterm, mhoccur, mhpresp)%>%
  mutate(mhoccur=if_else(mhpresp=="N","Missing",mhoccur))%>%
  mutate(pt_id=as.character(pt_id))%>%
  filter(mhterm %in% c("Diabetes",
                       "Type 2 diabetes",
                       "Type 1 diabetes",
                       "CABG",
                       "PCI",
                       "Myocardial infarction",
                       # "Stroke",
                       "Haemorrhagic stroke",
                       "Ischaemic stroke",
                       "Unknown aetiology stroke",
                       "Lower limb arterial revascularisation",
                       "Aortic aneurysm repair (surgery or stent)"))%>%
  pivot_wider(pt_id, names_from = "mhterm", values_from="mhoccur")%>%
  mutate(Diabetes=if_else(is.na(Diabetes) & (`Type 2 diabetes` == "Y" | `Type 1 diabetes` == "Y"), "Y", Diabetes))%>%
  # mutate(mhoccur=replace_na(mhoccur, "Missing"))%>%
  select(-c(`Type 1 diabetes`,`Type 2 diabetes`))%>%
  
    
  left_join(participant_details%>%
              select(pt_id=participant_id, centre_id))%>%
  
  mutate(centre_with_data = if_else(
    centre_id %in%
      (feasibility%>%
         filter(outcome=="Data uploaded")%>%
         select(centre_id)%>%
         .[[1]]),
    "Y","N"),
    pt_with_data = if_else(
      pt_id %in% trust_labs$pt_id,
      "Y", "N"))%>%
  left_join(participant_details%>%
              select(pt_id=participant_id,
                     age,
                     sex))%>%
  select(-centre_id, -pt_id)%>%
  # mutate(across(!pt_id, ~if_else(.=="Y",T,F)))%>%
  mutate(group = case_when(centre_with_data=="N" ~ "Participant with no data (centre with no data)",
                           centre_with_data == "Y" & pt_with_data =="Y" ~ "Participant with data",
                           centre_with_data == "Y" & pt_with_data =="N" ~ "Participant with no data (centre with data)",
  ))%>%
  select(-centre_with_data, -pt_with_data)%>%
  select(Age=age, 
         Sex=sex,
         Diabetes,
         `Myocardial infarction`,
         CABG,
         PCI,
         `Ischaemic stroke`,
         `Haemorrhagic stroke`,
         `Unknown aetiology stroke`,
         `Lower limb arterial revascularisation`,
         `Aortic aneurysm repair (surgery or stent)`,
         group)%>%
  mutate(across(-Age, ~replace_na(.,"N")))%>%
  # tbl_strata(
  #   strata=centre_with_data,
  #   ~.x%>%
      tbl_summary(by="group",
                  missing="no",
                  statistic = list(all_continuous() ~ "{mean} ({sd})"),
                  value=list(Sex~"F",
                             Diabetes~"Y",
                             CABG~"Y",
                             PCI~"Y",
                             `Myocardial infarction`~"Y",
                             `Haemorrhagic stroke`~"Y",
                             `Ischaemic stroke`~"Y",
                             `Unknown aetiology stroke`~"Y",
                             `Lower limb arterial revascularisation`~"Y",
                             `Aortic aneurysm repair (surgery or stent)`~"Y"),
                  # label=list(age_at_data_extraction ~ "Age (at point of central data extraction)",
                  #            sex ~ "Female",
                  #            mi_present ~ "Myocardial infarction (previous)",
                  #            stroke_present ~ "Stroke (previous)",
                  #            pvd_surgery_present ~ "Peripheral vascular disease (previous)")
                  )%>%
# )%>%
  # modify_spanning_header(c("stat_1", "stat_2") ~ "**Invited**") %>%
  as_flex_table()%>%
  save_as_docx(path="Outputs/Tables/baseline_characteristics_at_screening.docx",
               pr_section = sect_properties)
  
  
  


  



### data at invitation for site with vs without data  ------


invite_data%>%
  distinct(pt_id, .keep_all = T)%>%
  # head()%>%
  # filter(centre_id %in%
  #        (feasibility%>%
  #        filter(outcome=="Data uploaded")%>%
  #        select(centre_id)%>%
  #        .[[1]]))%>%
  # filter(pt_id %in% trust_labs$pt_id)%>%
  
  mutate(centre_with_data = if_else(
    centre_id %in%
      (feasibility%>%
         filter(outcome=="Data uploaded")%>%
         select(centre_id)%>%
         .[[1]]),
    "Y","N"))%>%
  mutate(age_at_data_extraction=as.integer(age_calc(as.Date(dob, format="%y-%m-%d"),enddate=as.Date(date_valid_from, format="%Y-%m-%d"),units="years")))%>%
  select(age_at_data_extraction,
         sex,
         mi_present,
         stroke_present,
         pvd_surgery_present,
         centre_with_data)%>%
  mutate(mi_present=if_else(mi_present==1, "Yes", "No"),
         stroke_present=if_else(stroke_present==1, "Yes", "No"),
         pvd_surgery_present=if_else(pvd_surgery_present==1, "Yes", "No"))%>%
  mutate(mi_present=replace_na(mi_present, "No"),
         stroke_present=replace_na(stroke_present, "No"),
         pvd_surgery_present=replace_na(pvd_surgery_present, "No"))%>%
  
  tbl_summary(by="centre_with_data",
                  missing="no",
                  statistic = list(all_continuous() ~ "{mean} ({sd})"),
                  value=list(sex~"F",
                             mi_present~"Yes",
                             stroke_present~"Yes",
                             pvd_surgery_present~"Yes"),
                  label=list(age_at_data_extraction ~ "Age (at point of central data extraction)",
                             sex ~ "Female",
                             mi_present ~ "Myocardial infarction (previous)",
                             stroke_present ~ "Stroke (previous)",
                             pvd_surgery_present ~ "Peripheral vascular disease (previous)"))%>%
  # modify_spanning_header(c("stat_1", "stat_2") ~ "**Invited**") %>%
  as_flex_table()%>%
  save_as_docx(path="Outputs/Tables/baseline_characteristics_pre_screening_by_site.docx",
               pr_section = sect_properties)
























# 2. Agreement ------

## Individual-level comparisons ----

### distributions -----

#### all results (overlapping) -------

individual_chol_values_overlapping<-screening_labs%>%
  select(pt_id, result, date)%>%
  mutate(Source="POC")%>%
  rbind(
    trust_labs%>%
      select(pt_id, result=chol_value, date=chol_date)%>%
      mutate(Source="Hospital")
    
    
  )%>%
  filter(pt_id %in% overlapping_ids)%>%
  mutate(result=as.numeric(result))

individual_chol_values_overlapping%>%
  ggplot(aes(result, 
             # y=Source,
             fill=Source))+
  
  # geom_point(aes(color=Source),
  #            position=position_jitter())+
  
  # geom_boxplot(alpha=0.5)+
  geom_density(color="black",
               alpha=0.2)+
  geom_vline(data=  
               individual_chol_values_overlapping%>%
               filter(!is.na(result))%>%
               group_by(Source)%>%
               summarise(median=median(result),
                         Q1=quantile(result,0.25),
                         Q3=quantile(result,0.75)),
             aes(xintercept=median,
                 color=Source),
             linetype="dashed",
             size=2)+
  scale_x_continuous(breaks=seq(0,15,1),
                     limits=c(0,NA))+
  theme(legend.position = "bottom")
# scale_fill_discrete()+
# facet_wrap(~Source,
#            ncol=1)



#### most recent results (overlapping) -----


individual_chol_values_overlapping%>%
  group_by(pt_id, Source)%>%
  slice_max(date)%>%
  ggplot(aes(result, 
             # y=Source,
             fill=Source))+
  
  # geom_point(aes(color=Source),
  #            position=position_jitter())+
  
  # geom_boxplot(alpha=0.5)+
  geom_density(color="black",
               alpha=0.2)+
  geom_vline(data=  
               individual_chol_values_overlapping%>%
               group_by(pt_id, Source)%>%
               slice_max(date)%>%
               filter(!is.na(result))%>%
               group_by(Source)%>%
               summarise(median=median(result),
                         Q1=quantile(result,0.25),
                         Q3=quantile(result,0.75)),
             aes(xintercept=median,
                 color=Source),
             linetype="dashed",
             size=2)+
  scale_x_continuous(breaks=seq(0,15,1),
                     limits=c(0,NA))+
  theme(legend.position = "bottom")
# scale_fill_discrete()+
# facet_wrap(~Source,
#            ncol=1)


### scatter plots -----

#### most recent results-----


individual_chol_values_overlapping%>%
  filter(Source=="POC")%>%
  group_by(pt_id)%>%
  filter(date==max(date))%>%
  ungroup()%>%
  select(-Source)%>%
  rename(result_poc=result,
         date_poc=date)%>%
  
  left_join(
    individual_chol_values_overlapping%>%
      filter(Source=="Hospital")%>%
      group_by(pt_id)%>%
      filter(date==max(date))%>%
      ungroup()%>%
      rename(result_hospital=result,
             date_hospital=date),
    by=c("pt_id"))%>%
  
ggplot(aes(result_hospital,
             result_poc))+
  
  geom_point(alpha=0.2)+
  scale_x_continuous(limits=c(0,10),
                     breaks=seq(0,10,1))+
  scale_y_continuous(limits=c(0,NA),
                     breaks=seq(0,10,1))+
  coord_fixed()+
  geom_smooth(method="lm")+
  stat_cor(method="pearson",
           size=6)+
  labs(x="Hospital result",
       y="POC result")
  # geom_vline(xintercept = 4, linetype="dashed",
  #            color="red",
  #            size=1
  #            )
  # geom_hline(yintercept = 4, linetype="dashed",
  #            color="red",
  #            size=1)+
  # geom_vline(xintercept = 3.5, linetype="dashed",
  #            color="blue",
  #            size=1)+
  # geom_hline(yintercept = 3.5, linetype="dashed",
  #            color="blue",
  #            size=1)+
  # geom_vline(xintercept = 3.0, linetype="dashed",
  #            color="orange",
  #            size=1)+
  # geom_hline(yintercept = 3.0, linetype="dashed",
  #            color="orange",
  #            size=1)+
  # geom_abline(slope=1, intercept=0, linetype="dashed")


## same as above, but excluding POC 2.59


individual_chol_values_overlapping%>%
  filter(Source=="POC")%>%
  group_by(pt_id)%>%
  filter(date==max(date))%>%
  ungroup()%>%
  select(-Source)%>%
  rename(result_poc=result,
         date_poc=date)%>%
  
  left_join(
    individual_chol_values_overlapping%>%
      filter(Source=="Hospital")%>%
      group_by(pt_id)%>%
      filter(date==max(date))%>%
      ungroup()%>%
      rename(result_hospital=result,
             date_hospital=date),
    by=c("pt_id"))%>%
  
  filter(result_poc>2.59)%>%
  View()
  
  ggplot(aes(result_hospital,
             result_poc))+
  
  geom_point(alpha=0.2)+
  scale_x_continuous(limits=c(0,10),
                     breaks=seq(0,10,1))+
  scale_y_continuous(limits=c(0,NA),
                     breaks=seq(0,10,1))+
  coord_fixed()+
  geom_smooth(method="lm")+
  stat_cor(method="pearson",
           size=6)+
  labs(x="Hospital result",
       y="POC result")



### bland-altman plots -----


bland_altman_plot_data<-individual_chol_values_overlapping%>%
  filter(Source=="POC")%>%
  distinct(pt_id, result, date)%>%
  group_by(pt_id)%>%
  filter(date==max(date))%>%
  ungroup()%>%
  rename(result_poc=result,
         date_poc=date)%>%
  filter(!is.na(result_poc))%>%
  
  left_join(
    individual_chol_values_overlapping%>%
      filter(Source=="Hospital")%>%
      distinct(pt_id, result, date)%>%
      group_by(pt_id)%>%
      filter(date==max(date))%>%
      ungroup()%>%
      rename(result_hospital=result,
             date_hospital=date),
    by=c("pt_id"))%>%
  select(result_poc, result_hospital)%>%
  rowwise()%>%
  mutate(diff=round(result_hospital-result_poc,2),
         average=mean(result_hospital, result_poc))
  

bland_altman_plot_stats<-
  bland_altman_plot_data%>%
  ungroup()%>%
  summarise(mean_diff=mean(diff),
            lower_bound=mean_diff-1.96*sd(diff),
            upper_bound=mean_diff+1.96*sd(diff))
  
  

bland_altman_plot_data%>%
  mutate(across(c(result_poc, result_hospital, average, diff), ~as.numeric(.)))%>%
  ggplot(aes(average, diff))+
  geom_point(alpha=0.2,
             size=1)+
  geom_hline(data=bland_altman_plot_stats,
             aes(yintercept=mean_diff),
             linetype="dashed") +
  geom_hline(yintercept=0, color="black") +
  geom_hline(data=bland_altman_plot_stats,
             aes(yintercept=lower_bound), 
             color = "red", linetype="dashed") +
  geom_hline(data=bland_altman_plot_stats,
             aes(yintercept=upper_bound), 
             color = "red", linetype="dashed")+
  labs(y="Difference (Hospital minus POC)",
       x="Average")+
  geom_text(data=bland_altman_plot_stats,
            aes(y=mean_diff+0.5,x=8.5,
                label = paste0("Mean difference: ", round(mean_diff, 2))),
            size=5,
            color="black",
            hjust=0)+
  geom_text(data=bland_altman_plot_stats,
            aes(y=lower_bound-0.5,x=8.5,
                label=paste0("95% LA: ", round(lower_bound, 2))), 
            size=5,
            color="red",
            hjust=0)+
  geom_text(data=bland_altman_plot_stats,
            aes(y=upper_bound+0.5,x=8.5,
                label=paste0("95% LA: ", round(upper_bound, 2))), 
            size=5,
            color="red",
            hjust=0)

  














### cutoff tables -----
  
  #### simple -----

individual_chol_values_overlapping%>%
  filter(Source=="POC")%>%
  distinct(pt_id, result, date)%>%
  group_by(pt_id)%>%
  filter(date==max(date))%>%
  ungroup()%>%
  rename(result_poc=result,
         date_poc=date)%>%
  filter(!is.na(result_poc))%>%
  
  left_join(
    individual_chol_values_overlapping%>%
      filter(Source=="Hospital")%>%
      group_by(pt_id)%>%
      filter(date==max(date))%>%
      ungroup()%>%
      rename(result_hospital=result,
             date_hospital=date),
    by=c("pt_id"))%>%
  select(result_poc, result_hospital)%>%
  mutate(poc_above_equal_4.0= if_else(result_poc>=4.0,1,0),
         hospital_above_equal_4.0=if_else(result_hospital>=4.0,1,0),
         poc_above_equal_3.5=if_else(result_poc>=3.5,1,0),
         hospital_above_equal_3.5=if_else(result_hospital>=3.5,1,0),
         poc_above_equal_3.0=if_else(result_poc>=3.0,1,0),
         hospital_above_equal_3.0=if_else(result_hospital>=3.0,1,0))%>%
  count(poc_above_equal_3.0,
        hospital_above_equal_3.0)%>%
    mutate(prop=n/sum(n)*100)
  

  
  #### with adjustment for bias ----
  
  individual_chol_values_overlapping%>%
    filter(Source=="POC")%>%
    distinct(pt_id, result, date)%>%
    group_by(pt_id)%>%
    filter(date==max(date))%>%
    ungroup()%>%
    rename(result_poc=result,
           date_poc=date)%>%
    filter(!is.na(result_poc))%>%
    
    left_join(
      individual_chol_values_overlapping%>%
        filter(Source=="Hospital")%>%
        group_by(pt_id)%>%
        filter(date==max(date))%>%
        ungroup()%>%
        rename(result_hospital=result,
               date_hospital=date),
      by=c("pt_id"))%>%
    select(result_poc, result_hospital)%>%
    mutate(poc_above_equal_4.0= if_else(result_poc>=4.0,1,0),
           hospital_above_equal_4.2=if_else(result_hospital>=4.2,1,0),
           poc_above_equal_3.5=if_else(result_poc>=3.5,1,0),
           hospital_above_equal_3.7=if_else(result_hospital>=3.7,1,0),
           poc_above_equal_3.0=if_else(result_poc>=3.0,1,0),
           hospital_above_equal_3.2=if_else(result_hospital>=3.2,1,0))%>%
    count(poc_above_equal_4.0,
          hospital_above_equal_4.2)%>%
    mutate(prop=n/sum(n)*100)
  
  

## Time-based comparison -----

  
### all results ------
time_based_comparisons<-individual_chol_values_overlapping%>%
  filter(Source=="POC")%>%
  select(-Source)%>%
  rename(result_poc=result,
         date_poc=date)%>%
  
  left_join(
    individual_chol_values_overlapping%>%
      filter(Source=="Hospital")%>%
      rename(result_hospital=result,
             date_hospital=date),
    by=c("pt_id"))%>%
  mutate(time_diff=as.numeric(difftime(date_hospital, date_poc, units = "days")))%>%
  mutate(value_diff=round(result_hospital-result_poc,2))
# 
# time_based_comparisons%>%
#   ggplot(aes(time_diff,value_diff))+
#   geom_point(alpha=0.2)+
#   geom_hline(yintercept = 0, linetype="dashed")
    
    
    
time_based_comparisons%>%
  ggplot(aes(time_diff,value_diff))+
  geom_bin_2d(color="black",binwidth=c(90,0.5))+
  stat_bin2d(geom = "text", aes(label = ..count..), size=6,binwidth = c(90,0.5))+
  geom_hline(yintercept = 0, linetype="dashed", 
             size=1
             )+
  geom_vline(xintercept = 0, linetype="dashed", 
             size=1
             )+
  scale_y_continuous(breaks=seq(-7,6,0.5))+
  scale_x_continuous(breaks=seq(-2970, 360,90))+
  scale_fill_gradient(low = "white", high = "red",
                      limits=c(0,NA),
                      breaks=c(1,50,100,150))+
  labs(x="Interval between measurements in days (negative values represent hospital measurements recorded before screening)",
       y="Difference between measurements in mmol/L (hospital minus screening)",
       fill="Count")+
  theme(panel.grid.minor = element_blank(),
        axis.title = element_text(size=20),
        axis.text=element_text(size=10))
  
### more recent result -----

individual_chol_values_overlapping%>%
  filter(Source=="POC")%>%
  group_by(pt_id)%>%
  filter(date==max(date))%>%
  ungroup()%>%
  select(-Source)%>%
  rename(result_poc=result,
         date_poc=date)%>%
  
  left_join(
    individual_chol_values_overlapping%>%
      filter(Source=="Hospital")%>%
      group_by(pt_id)%>%
      filter(date==max(date))%>%
      ungroup()%>%
      rename(result_hospital=result,
             date_hospital=date),
    by=c("pt_id"))%>%
  mutate(time_diff=as.numeric(difftime(date_hospital, date_poc, units = "days")))%>%
  mutate(value_diff=round(result_hospital-result_poc,2))%>%
  
  ggplot(aes(time_diff,value_diff))+
  geom_bin_2d(color="black",binwidth=c(90,0.5))+
  stat_bin2d(geom = "text", aes(label = ..count..), size=6,binwidth = c(90,0.5))+
  geom_hline(yintercept = 0, linetype="dashed", 
             size=1
  )+
  geom_vline(xintercept = 0, linetype="dashed", 
             size=1
  )+
  scale_y_continuous(breaks=seq(-7,6,0.5))+
  scale_x_continuous(breaks=seq(-2970, 360,90))+
  scale_fill_gradient(low = "white", high = "red",
                      limits=c(0,NA),
                      breaks=c(1,50,100,150))+
  labs(x="Interval between measurements in days (negative values represent hospital measurements recorded before screening)",
       y="Difference between measurements in mmol/L (hospital minus screening)",
       fill="Count")+
  theme(panel.grid.minor = element_blank(),
        axis.title = element_text(size=20),
        axis.text=element_text(size=10))


## calculate average intervals -----


individual_chol_values_overlapping%>%
  filter(Source=="POC")%>%
  group_by(pt_id)%>%
  filter(date==max(date))%>%
  ungroup()%>%
  select(-Source)%>%
  rename(result_poc=result,
         date_poc=date)%>%
  
  left_join(
    individual_chol_values_overlapping%>%
      filter(Source=="Hospital")%>%
      group_by(pt_id)%>%
      filter(date==max(date))%>%
      ungroup()%>%
      rename(result_hospital=result,
             date_hospital=date),
    by=c("pt_id"))%>%
  mutate(time_diff=as.numeric(difftime(date_hospital, date_poc, units = "days")))%>%
  mutate(value_diff=round(result_hospital-result_poc,2))%>%
  select(time_diff)%>%
  mutate(time_diff=time_diff/30)%>%
  summarise(min=min(time_diff),
            max=max(time_diff),
            mean=round(mean(time_diff),1),
            SD=round(sd(time_diff),1),
            median=median(time_diff),
            Q1=quantile(time_diff, 0.25),
            Q3=quantile(time_diff, 0.75))



individual_chol_values_overlapping%>%
  filter(Source=="POC")%>%
  group_by(pt_id)%>%
  filter(date==max(date))%>%
  ungroup()%>%
  select(-Source)%>%
  rename(result_poc=result,
         date_poc=date)%>%
  
  left_join(
    individual_chol_values_overlapping%>%
      filter(Source=="Hospital")%>%
      group_by(pt_id)%>%
      filter(date==max(date))%>%
      ungroup()%>%
      rename(result_hospital=result,
             date_hospital=date),
    by=c("pt_id"))%>%
  mutate(time_diff=as.numeric(difftime(date_hospital, date_poc, units = "days")))%>%
  mutate(value_diff=round(result_hospital-result_poc,2))%>%
  select(time_diff)%>%
  mutate(time_diff=time_diff/30)%>%
  ggplot(aes(time_diff))+
  geom_histogram(color="black")+
  scale_x_continuous(breaks=seq(-120,0,10))+
  labs(x="Time interval between most recent hospital lab result and first screening appointment (in months)",
       y="Participants")






# 3. Utility (Before vs after) ------

# total ids requested

# list of dates when cholesterol data was received
# 
# data_reception_dates<-tibble(
#   trust_code=c("RAE",
#                "RTG", # check if data from both hospitals included here
#                "RM1",
#                "RTX",
#                "RTX",
#                "RWE",
#                "RTE",
#                # "RMC", not uploaded
#                "RGR",
#                "RBK",
#                "RJL",
#                "RWY",
#                "RA7",
#                "RX1",
#                "RBD",
#                "RXW"),
#   site_code=c("41318",
#               "41292",
#               "41312",
#               "41411",
#               "41422",
#               "41238",
#               "41270",
#               # "41203", not uploaded
#               "41413",
#               "41169",
#               "41224",
#               "41165",
#               "41161",
#               "41343",
#               "41356",
#               "41162"),
#   date=c("17/12/2019",
#          "11/12/2019",
#          "09/12/2019",
#          "22/11/2019",
#          "22/11/2019",
#          "12/11/2019",
#          "11/11/2019",
#          # "31/10/2019", not uploaded
#          "29/10/2019",
#          "01/10/2019",
#          "01/10/2019",
#          "17/09/2019",
#          "10/09/2019",
#          "21/08/2019",
#          "10/12/2019"))

# list of dates when manual pre-screening stopped



# list of screening dates





# sites who ran out of data
# 41292
# 41203

## compute invitation start and finish ----


site_invite_dates<-mailings%>%
  filter(!is.na(trust_code))%>%
  group_by(centre_id)%>%
  summarise(invite_first=min(date),
            invite_last=max(date))%>%
  ungroup()%>%
  right_join(pre_screening_details%>%
               select(centre_id=site_code,
                      trust_code,
                      site_name,
                      date_received,
                      date_uploaded)%>%
               filter(!is.na(date_received)))

site_invite_dates%>%
  mutate(time_after_activation = as.numeric(difftime(date_received, invite_first, units="days")))%>%
  mutate(time_after_activation=time_after_activation/30)%>%
  ggplot(aes(time_after_activation))+
  geom_histogram(color="black")+
  scale_x_continuous(limits=c(0,NA))+
  labs(x="Interval between site recruitment initiation and data reception (in months)",
       y="Number of sites")

# all sites received data while already recruiting (but some very shortly after)

## % invite pre vs post screening -----


mailings_individual%>%
  left_join(participant_details%>%select(pt_id=participant_id, centre_id))%>%
  right_join(site_invite_dates%>%select(centre_id, date_uploaded))%>%
  filter(!is.na(date_uploaded))%>%
  mutate(invite_pre_screened=if_else(date<date_uploaded, "Invite not pre-screened", "Invite pre-screened"))%>%
  count(centre_id, invite_pre_screened)%>%
  group_by(centre_id)%>%
  mutate(prop=round(n/sum(n)*100,1))%>%
  filter(invite_pre_screened=="Invite pre-screened")%>%
  ggplot(aes(prop))+
  geom_histogram(color="black")+
  scale_x_continuous(limits=c(0,100))+
  labs(x="Proportion pre-screened",
       y="Number of sites")
  



## % appointments cancelled -----

  

participant_status%>%
  
  select(date_registered,
         participant_id=pt_id,
         centre_id,
         date_screened,
         cancelled_failed_pre_screening=failed_pre_screening)%>%
  mutate(centre_id=as.character(centre_id))%>%
  mutate(date_registered=as.Date(date_registered, format="%Y-%m-%d"))%>%
  right_join(site_invite_dates%>%
               select(centre_id, date_uploaded))%>%
  left_join(mailings_individual%>%
              select(participant_id=pt_id,invite_date=date)%>%
              mutate(invite_date=as.Date(invite_date, format="%Y-%m-%d")))%>%
  filter(!is.na(invite_date),
         !is.na(date_uploaded))%>%
  mutate(invite_pre_screened=if_else(invite_date<date_uploaded, "Invite not pre-screened", "Invite pre-screened"))%>%
  
  group_by(invite_pre_screened)%>%
  
  select(participant_id,
         centre_id,
         date_registered,
         date_uploaded,
         invite_date,
         invite_pre_screened,
         cancelled_failed_pre_screening,
         date_screened)%>%
  mutate(date_screened=if_else(date_screened=="", NA, date_screened))%>%
  summarise(cancelled=length(participant_id[cancelled_failed_pre_screening=="Y"]),
            confirmed=length(participant_id[!is.na(date_screened)]),
            total=length(participant_id))%>%
  mutate(prop_cancelled_vs_confirmed=round(cancelled/confirmed*100,2),
         SE_cancelled_vs_confirmed=round(sqrt(prop_cancelled_vs_confirmed/100*(1-prop_cancelled_vs_confirmed/100)/confirmed)*100,2),
         prop_cancelled_vs_total=round(cancelled/total*100,2),
         SE_cancelled_vs_total=round(sqrt(prop_cancelled_vs_total/100*(1-prop_cancelled_vs_total/100)/total)*100,1))%>%
  rename(cancelled_LRC=cancelled)%>%
  View()












  
## % cholesterol failure -------

### all participants in sites with data ------

participant_status%>%
  left_join(screening_labs%>%select(pt_id, result))%>%
  filter(study_status!=10)%>%
  select(pt_id, study_status_cat, date_screened, date_randomized, result)%>%
  mutate(chol_fail=if_else(study_status_cat == "Screen failure/incomplete" & ((date_screened<"2021-07-13 12:00:00" & result<4)|
                                                   (date_screened<"2022-06-07 12:00:00" &  date_screened>="2021-07-13 12:00:00" & result<3.5)|
                                                    (date_screened>="2022-06-07 12:00:00" & result<3)),"Y","N"),
         fail_total=if_else(study_status_cat == "Screen failure/incomplete","Y","N"))%>%
  mutate(chol_fail=replace_na(chol_fail,"NA"))%>%
  mutate(chol_fail=if_else(study_status_cat!="Screen failure/incomplete", "N", chol_fail))%>%
  
  left_join(participant_details%>%select(pt_id=participant_id, centre_id))%>%
  
  right_join(site_invite_dates%>%
               select(centre_id, date_uploaded))%>%
  
  
  left_join(mailings_individual%>%
              select(pt_id,invite_date=date)%>%
              mutate(invite_date=as.Date(invite_date, format="%Y-%m-%d")))%>%
  
  
  filter(!is.na(invite_date),
         !is.na(date_uploaded))%>%
  mutate(invite_pre_screened=if_else(invite_date<date_uploaded, "Invite not pre-screened", "Invite pre-screened"))%>%
  group_by(invite_pre_screened)%>%
  # summarise(n=)
  summarise(failed_chol=length(date_screened[chol_fail=="Y"]),
            failed_total=length(date_screened[fail_total=="Y"]),
            total=length(date_screened))%>%
  mutate(prop_chol=round(failed_chol/total*100,1),
         SE_chol=round(sqrt(prop_chol/100*(1-prop_chol/100)/total)*100,1),
         prop_total=round(failed_total/total*100,1),
         SE_total=round(sqrt(prop_total/100*(1-prop_total/100)/total)*100,1),
         prop_chol_over_total=round(failed_chol/failed_total*100,1),
         SE_chol_over_total=round(sqrt(prop_chol_over_total/100*(1-prop_chol_over_total/100)/total)*100,1))%>%
  
  View()
  
# calculate total % of chol fails (across England)
  
  
  participant_status%>%
    left_join(screening_labs%>%select(pt_id, result))%>%
    filter(study_status!=10)%>%
    select(pt_id, study_status_cat, date_screened, date_randomized, result)%>%
    mutate(chol_fail=if_else(study_status_cat == "Screen failure/incomplete" & ((date_screened<"2021-07-13 12:00:00" & result<4)|
                                                                                  (date_screened<"2022-06-07 12:00:00" &  date_screened>="2021-07-13 12:00:00" & result<3.5)|
                                                                                  (date_screened>="2022-06-07 12:00:00" & result<3)),"Y","N"),
           fail_total=if_else(study_status_cat == "Screen failure/incomplete","Y","N"))%>%
    mutate(chol_fail=replace_na(chol_fail,"NA"))%>%
    mutate(chol_fail=if_else(study_status_cat!="Screen failure/incomplete", "N", chol_fail))%>%
    summarise(chol_fail=length(chol_fail[chol_fail=="Y"])/length(chol_fail),
              fail_total=length(fail_total[fail_total=="Y"])/length(fail_total))%>%
    View()

### only participants with data -----


participant_status%>%
  left_join(screening_labs%>%select(pt_id, result))%>%
  filter(study_status!=10)%>%
  select(pt_id, study_status_cat, date_screened, date_randomized, result)%>%
  mutate(chol_fail=if_else(study_status_cat == "Screen failure/incomplete" & ((date_screened<"2021-07-13 12:00:00" & result<4)|
                                                                                (date_screened<"2022-06-07 12:00:00" &  date_screened>="2021-07-13 12:00:00" & result<3.5)|
                                                                                (date_screened>="2022-06-07 12:00:00" & result<3)),"Y","N"),
         fail_total=if_else(study_status_cat == "Screen failure/incomplete","Y","N"))%>%
  mutate(chol_fail=replace_na(chol_fail,"NA"))%>%
  mutate(chol_fail=if_else(study_status_cat!="Screen failure/incomplete", "N", chol_fail))%>%
  
  left_join(participant_details%>%select(pt_id=participant_id, centre_id))%>%
  
  right_join(site_invite_dates%>%
               select(centre_id, date_uploaded))%>%
  
  left_join(mailings_individual%>%
              select(pt_id,invite_date=date)%>%
              mutate(invite_date=as.Date(invite_date, format="%Y-%m-%d")))%>%
  
  
  filter(!is.na(invite_date),
         !is.na(date_uploaded))%>%
  mutate(invite_pre_screened=if_else(invite_date<date_uploaded, "Invite not pre-screened", "Invite pre-screened"))%>%
  
  filter(pt_id %in% trust_labs$pt_id)%>%
  
  
  group_by(invite_pre_screened)%>%
  # summarise(n=)
  summarise(failed_chol=length(date_screened[chol_fail=="Y"]),
            failed_total=length(date_screened[fail_total=="Y"]),
            total=length(date_screened))%>%
  mutate(prop_chol=round(failed_chol/total*100,1),
         SE_chol=round(sqrt(prop_chol/100*(1-prop_chol/100)/total)*100,1),
         prop_total=round(failed_total/total*100,1),
         SE_total=round(sqrt(prop_total/100*(1-prop_total/100)/total)*100,1),
         prop_chol_over_total=round(failed_chol/failed_total*100,1),
         SE_chol_over_total=round(sqrt(prop_chol_over_total/100*(1-prop_chol_over_total/100)/total)*100,1))%>%
  
  View()

### before central pre-screening vs after (4.0 cut-off) -----


participant_status%>%
  left_join(screening_labs%>%select(pt_id, result))%>%
  filter(study_status!=10)%>%
  select(pt_id, study_status_cat, date_screened, date_randomized, result)%>%
  mutate(chol_fail=if_else(study_status_cat == "Screen failure/incomplete" & ((date_screened<"2021-07-13 12:00:00" & result<4)|
                                                                                (date_screened<"2022-06-07 12:00:00" &  date_screened>="2021-07-13 12:00:00" & result<3.5)|
                                                                                (date_screened>="2022-06-07 12:00:00" & result<3)),"Y","N"),
         fail_total=if_else(study_status_cat == "Screen failure/incomplete","Y","N"))%>%
  mutate(chol_fail=replace_na(chol_fail,"NA"))%>%
  mutate(chol_fail=if_else(study_status_cat!="Screen failure/incomplete", "N", chol_fail))%>%
  
  left_join(participant_details%>%select(pt_id=participant_id, centre_id))%>%
  
  right_join(site_invite_dates%>%
               select(centre_id, date_uploaded))%>%
  
  left_join(mailings_individual%>%
              select(pt_id,invite_date=date)%>%
              mutate(invite_date=as.Date(invite_date, format="%Y-%m-%d")))%>%
  
  
  filter(!is.na(invite_date),
         !is.na(date_uploaded))%>%
  mutate(invite_pre_screened=if_else(invite_date<date_uploaded, "Invite not pre-screened", "Invite pre-screened"))%>%
  
  filter(pt_id %in% trust_labs$pt_id |
           invite_pre_screened=="Invite not pre-screened"
           )%>%
  
  
  group_by(invite_pre_screened)%>%
  # summarise(n=)
  summarise(failed_chol=length(date_screened[chol_fail=="Y"]),
            failed_total=length(date_screened[fail_total=="Y"]),
            total=length(date_screened))%>%
  mutate(prop_chol=round(failed_chol/total*100,1),
         SE_chol=round(sqrt(prop_chol/100*(1-prop_chol/100)/total)*100,1),
         prop_total=round(failed_total/total*100,1),
         SE_total=round(sqrt(prop_total/100*(1-prop_total/100)/total)*100,1),
         prop_chol_over_total=round(failed_chol/failed_total*100,1),
         SE_chol_over_total=round(sqrt(prop_chol_over_total/100*(1-prop_chol_over_total/100)/total)*100,1))%>%
  
  View()


### before central pre-screening vs after for participants with available data (by cutoff) -----


participant_status%>%
  left_join(screening_labs%>%select(pt_id, result))%>%
  filter(study_status!=10)%>%
  select(pt_id, study_status_cat, date_screened, date_randomized, result)%>%
  mutate(chol_fail=if_else(study_status_cat == "Screen failure/incomplete" & ((date_screened<"2021-07-13 12:00:00" & result<4)|
                                                                                (date_screened<"2022-06-07 12:00:00" &  date_screened>="2021-07-13 12:00:00" & result<3.5)|
                                                                                (date_screened>="2022-06-07 12:00:00" & result<3)),"Y","N"),
         fail_total=if_else(study_status_cat == "Screen failure/incomplete","Y","N"))%>%
  mutate(chol_fail=replace_na(chol_fail,"NA"))%>%
  mutate(chol_fail=if_else(study_status_cat!="Screen failure/incomplete", "N", chol_fail))%>%
  
  left_join(participant_details%>%select(pt_id=participant_id, centre_id))%>%
  
  right_join(site_invite_dates%>%
               select(centre_id, date_uploaded))%>%
  
  left_join(mailings_individual%>%
              select(pt_id,invite_date=date)%>%
              mutate(invite_date=as.Date(invite_date, format="%Y-%m-%d")))%>%
  
  
  filter(!is.na(invite_date),
         !is.na(date_uploaded))%>%
  mutate(invite_pre_screened=if_else(invite_date<date_uploaded, "Invite not pre-screened", "Invite pre-screened"))%>%
  
  filter(pt_id %in% trust_labs$pt_id)%>%
  
  mutate(screening_cut_off=if_else(invite_date>="2019-12-02", "3.5", "4.0"))%>%
  group_by(invite_pre_screened, screening_cut_off)%>%
  # summarise(n=)
  summarise(failed_chol=length(date_screened[chol_fail=="Y"]),
            failed_total=length(date_screened[fail_total=="Y"]),
            total=length(date_screened))%>%
  mutate(prop_chol=round(failed_chol/total*100,1),
         SE_chol=round(sqrt(prop_chol/100*(1-prop_chol/100)/total)*100,1),
         prop_total=round(failed_total/total*100,1),
         SE_total=round(sqrt(prop_total/100*(1-prop_total/100)/total)*100,1),
         prop_chol_over_total=round(failed_chol/failed_total*100,1),
         SE_chol_over_total=round(sqrt(prop_chol_over_total/100*(1-prop_chol_over_total/100)/total)*100,1))%>%
  
  View()


### before central pre-screening vs after for all participants (by cutoff) -----


participant_status%>%
  left_join(screening_labs%>%select(pt_id, result))%>%
  filter(study_status!=10)%>%
  select(pt_id, study_status_cat, date_screened, date_randomized, result)%>%
  mutate(chol_fail=if_else(study_status_cat == "Screen failure/incomplete" & ((date_screened<"2021-07-13 12:00:00" & result<4)|
                                                                                (date_screened<"2022-06-07 12:00:00" &  date_screened>="2021-07-13 12:00:00" & result<3.5)|
                                                                                (date_screened>="2022-06-07 12:00:00" & result<3)),"Y","N"),
         fail_total=if_else(study_status_cat == "Screen failure/incomplete","Y","N"))%>%
  mutate(chol_fail=replace_na(chol_fail,"NA"))%>%
  mutate(chol_fail=if_else(study_status_cat!="Screen failure/incomplete", "N", chol_fail))%>%View()
  
  left_join(participant_details%>%select(pt_id=participant_id, centre_id))%>%
  
  right_join(site_invite_dates%>%
               select(centre_id, date_uploaded))%>%
  
  left_join(mailings_individual%>%
              select(pt_id,invite_date=date)%>%
              mutate(invite_date=as.Date(invite_date, format="%Y-%m-%d")))%>%
  
  
  filter(!is.na(invite_date),
         !is.na(date_uploaded))%>%
  mutate(invite_pre_screened=if_else(invite_date<date_uploaded, "Invite not pre-screened", "Invite pre-screened"))%>%
  
  # filter(pt_id %in% trust_labs$pt_id)%>%
  
  mutate(screening_cut_off=if_else(invite_date>="2019-12-02", "3.5", "4.0"))%>%
  group_by(invite_pre_screened, screening_cut_off)%>%
  # summarise(n=)
  summarise(failed_chol=length(date_screened[chol_fail=="Y"]),
            failed_total=length(date_screened[fail_total=="Y"]),
            total=length(date_screened))%>%
  mutate(prop_chol=round(failed_chol/total*100,1),
         SE_chol=round(sqrt(prop_chol/100*(1-prop_chol/100)/total)*100,1),
         prop_total=round(failed_total/total*100,1),
         SE_total=round(sqrt(prop_total/100*(1-prop_total/100)/total)*100,1),
         prop_chol_over_total=round(failed_chol/failed_total*100,1),
         SE_chol_over_total=round(sqrt(prop_chol_over_total/100*(1-prop_chol_over_total/100)/total)*100,1))%>%
  
  View()
  
  
### by screening cut-off (all participants) ------
  

participant_status%>%
  left_join(screening_labs%>%select(pt_id, result))%>%
  filter(study_status!=10)%>%
  select(pt_id, study_status_cat, date_screened, date_randomized, result)%>%
  mutate(chol_fail=if_else(study_status_cat == "Screen failure/incomplete" & ((date_screened<"2021-07-13 12:00:00" & result<4)|
                                                                                (date_screened<"2022-06-07 12:00:00" &  date_screened>="2021-07-13 12:00:00" & result<3.5)|
                                                                                (date_screened>="2022-06-07 12:00:00" & result<3)),"Y","N"),
         fail_total=if_else(study_status_cat == "Screen failure/incomplete","Y","N"))%>%
  mutate(screen_cut_off = case_when(date_screened<"2021-07-13 12:00:00" ~"4.0",
                                    date_screened<"2022-06-07 12:00:00" &  date_screened>="2021-07-13 12:00:00" ~"3.5",
                                    date_screened>="2022-06-07 12:00:00" ~ "3.0"))%>%
  mutate(chol_fail=replace_na(chol_fail,"NA"))%>%
  mutate(chol_fail=if_else(study_status_cat!="Screen failure/incomplete", "N", chol_fail))%>%
  
  left_join(participant_details%>%select(pt_id=participant_id, centre_id))%>%
  
  right_join(site_invite_dates%>%
               select(centre_id, date_uploaded))%>%
  
  left_join(mailings_individual%>%
              select(pt_id,invite_date=date)%>%
              mutate(invite_date=as.Date(invite_date, format="%Y-%m-%d")))%>%
  
  
  filter(!is.na(invite_date),
         !is.na(date_uploaded))%>%
  mutate(invite_pre_screened=if_else(invite_date<date_uploaded, "Invite not pre-screened", "Invite pre-screened"))%>%
  
  # filter(pt_id %in% trust_labs$pt_id)%>%
  
  mutate(pre_screening_cut_off=if_else(invite_date>="2019-12-02", "3.5", "4.0"))%>%

  group_by(invite_pre_screened, pre_screening_cut_off, screen_cut_off)%>%
  # summarise(n=)
  summarise(failed_chol=length(date_screened[chol_fail=="Y"]),
            failed_total=length(date_screened[fail_total=="Y"]),
            total=length(date_screened))%>%
  mutate(prop_chol=round(failed_chol/total*100,1),
         SE_chol=round(sqrt(prop_chol/100*(1-prop_chol/100)/total)*100,1),
         prop_total=round(failed_total/total*100,1),
         SE_total=round(sqrt(prop_total/100*(1-prop_total/100)/total)*100,1),
         prop_chol_over_total=round(failed_chol/failed_total*100,1),
         SE_chol_over_total=round(sqrt(prop_chol_over_total/100*(1-prop_chol_over_total/100)/total)*100,1))%>%
  
  View()  
  
  
### by screening cut-off (participants with available data) ------  
  
  
  
  participant_status%>%
    left_join(screening_labs%>%select(pt_id, result))%>%
    filter(study_status!=10)%>%
    select(pt_id, study_status_cat, date_screened, date_randomized, result)%>%
    mutate(chol_fail=if_else(study_status_cat == "Screen failure/incomplete" & ((date_screened<"2021-07-13 12:00:00" & result<4)|
                                                                                  (date_screened<"2022-06-07 12:00:00" &  date_screened>="2021-07-13 12:00:00" & result<3.5)|
                                                                                  (date_screened>="2022-06-07 12:00:00" & result<3)),"Y","N"),
           fail_total=if_else(study_status_cat == "Screen failure/incomplete","Y","N"))%>%
    mutate(screen_cut_off = case_when(date_screened<"2021-07-13 12:00:00" ~"4.0",
                                      date_screened<"2022-06-07 12:00:00" &  date_screened>="2021-07-13 12:00:00" ~"3.5",
                                      date_screened>="2022-06-07 12:00:00" ~ "3.0"))%>%
    mutate(chol_fail=replace_na(chol_fail,"NA"))%>%
    mutate(chol_fail=if_else(study_status_cat!="Screen failure/incomplete", "N", chol_fail))%>%
    
    left_join(participant_details%>%select(pt_id=participant_id, centre_id))%>%
    
    right_join(site_invite_dates%>%
                 select(centre_id, date_uploaded))%>%
    
left_join(mailings_individual%>%
                select(pt_id,invite_date=date)%>%
                mutate(invite_date=as.Date(invite_date, format="%Y-%m-%d")))%>%
    
    
    filter(!is.na(invite_date),
           !is.na(date_uploaded))%>%
    mutate(invite_pre_screened=if_else(invite_date<date_uploaded, "Invite not pre-screened", "Invite pre-screened"))%>%
    
    filter(pt_id %in% trust_labs$pt_id)%>%
    
    mutate(pre_screening_cut_off=if_else(invite_date>="2019-12-02", "3.5", "4.0"))%>%
    
    group_by(invite_pre_screened, pre_screening_cut_off, screen_cut_off)%>%
    # summarise(n=)
    summarise(failed_chol=length(date_screened[chol_fail=="Y"]),
              failed_total=length(date_screened[fail_total=="Y"]),
              total=length(date_screened))%>%
    mutate(prop_chol=round(failed_chol/total*100,1),
           SE_chol=round(sqrt(prop_chol/100*(1-prop_chol/100)/total)*100,1),
           prop_total=round(failed_total/total*100,1),
           SE_total=round(sqrt(prop_total/100*(1-prop_total/100)/total)*100,1),
           prop_chol_over_total=round(failed_chol/failed_total*100,1),
           SE_chol_over_total=round(sqrt(prop_chol_over_total/100*(1-prop_chol_over_total/100)/total)*100,1))%>%
    
    View()  
  

## prop of sites who ran out of data ----

### by data reception outcome -----
invite_data%>%
  select(-invite_date)%>%
    distinct(pt_id, .keep_all=T)%>%
    # filter(centre_id %in% 
    #          (feasibility%>%
    #          filter(outcome == "Data uploaded")%>%
    #          select(centre_id)%>%
    #          .[[1]]))%>%
  mutate(centre_id=as.character(centre_id))%>%
  left_join(feasibility%>%
              select(centre_id,
                     outcome))%>% 
  # filter(outcome!="Not contacted")%>%
  # filter(!is.na(outcome))%>%
  mutate(data_reception_epoch=if_else(date_valid_from<="2020-12-31",
                              "2019-2020",
                             "2021-2023"))%>%
  filter(centre_id!=0)%>%
  mutate(outcome=if_else(str_detect(outcome, "Not feasible|Not willing"), "Not feasible/\nwilling", outcome))%>%
  mutate(outcome=if_else(outcome  %in% c("Data uploaded", "Issues with data"), "Data uploaded/\nissues with data", outcome))%>%
  mutate(outcome=if_else(outcome=="Ongoing", "Abandoned",outcome))%>%
  mutate(outcome=if_else(outcome=="Ran out of data", "Ran out of data\n(2019-2020)",outcome))%>%
  mutate(outcome=replace_na(outcome, "Not contacted"))%>%
  mutate(outcome=factor(outcome, levels=c("Data uploaded/\nissues with data",
                                          "Not feasible/\nwilling",
                                          "Abandoned",
                                          "Ran out of data\n(2019-2020)",
                                          "Not contacted")))%>%
  group_by(outcome)%>%
  summarise(invited_2019_2020=length(invited[invited=="Y" & data_reception_epoch == "2019-2020"]),
            total_2019_2020=length(invited[data_reception_epoch == "2019-2020"]),
            invited=length(invited[invited=="Y"]),
            total=n())%>%
  pivot_longer(-outcome, names_to = "metric", values_to="value")%>%
  mutate(epoch=factor(if_else(str_detect(metric, "2019"),"Invite data received 2019-2020", "All invite data received (2019-2023)"),
                      levels=c("Invite data received 2019-2020",
                               "All invite data received (2019-2023)")))%>%
  mutate(metric = if_else(str_detect(metric,"invited"), "invited", "total"))%>%
  pivot_wider(id_cols=c(outcome, epoch), names_from = "metric", values_from="value")%>%
  mutate(prop=round(invited/total*100,1))%>%
            
  ggplot(aes(outcome, prop, fill=outcome))+
  geom_col()+
  facet_wrap(~epoch)+
  geom_text(aes(label=paste0(prop, "%")), vjust=-1)+
  scale_y_continuous(limits=c(0,100))+
  labs(y="Proportion of individuals invited from pool at each site",
       x="Centralised cholesterol pre-screening outcome")+
  theme(legend.position="none")
  
  
### pre-screened vs not -----
  
  
  invite_data%>%
    select(-invite_date)%>%
    distinct(pt_id, .keep_all=T)%>%
    # filter(centre_id %in% 
    #          (feasibility%>%
    #          filter(outcome == "Data uploaded")%>%
    #          select(centre_id)%>%
    #          .[[1]]))%>%
    mutate(centre_id=as.character(centre_id))%>%
    left_join(feasibility%>%
                select(centre_id,
                       outcome))%>% 
    # filter(outcome!="Not contacted")%>%
    # filter(!is.na(outcome))%>%
    mutate(data_reception_epoch=if_else(date_valid_from<="2020-12-31",
                                        "2019-2020",
                                        "2021-2023"))%>%
    filter(centre_id!=0)%>%
    mutate(outcome=if_else(outcome=="Data uploaded", "Yes","No"))%>%
    mutate(outcome=replace_na(outcome,"No"))%>%
    group_by(outcome)%>%
    summarise(invited_2019_2020=length(invited[invited=="Y" & data_reception_epoch == "2019-2020"]),
              total_2019_2020=length(invited[data_reception_epoch == "2019-2020"]),
              invited=length(invited[invited=="Y"]),
              total=n())%>%
    pivot_longer(-outcome, names_to = "metric", values_to="value")%>%
    mutate(epoch=factor(if_else(str_detect(metric, "2019"),"Invite data received 2019-2020", "All invite data received (2019-2023)"),
                        levels=c("Invite data received 2019-2020",
                                 "All invite data received (2019-2023)")))%>%
    mutate(metric = if_else(str_detect(metric,"invited"), "invited", "total"))%>%
    pivot_wider(id_cols=c(outcome, epoch), names_from = "metric", values_from="value")%>%
    mutate(prop=round(invited/total*100,1))%>%
    mutate(outcome=factor(outcome, levels=c("Yes", "No")))%>%
    
    ggplot(aes(outcome, prop, fill=outcome))+
    geom_col()+
    facet_wrap(~epoch)+
    geom_text(aes(label=paste0(prop, "%")), vjust=-1)+
    scale_y_continuous(limits=c(0,100))+
    labs(y="Proportion of individuals invited from pool at each site",
         x="Successful pre-screening")+
    theme(legend.position="none")
  
  
  
  
## number of invites avoided -------
  
  

  invite_data%>%
    mutate(centre_id=as.character(centre_id))%>%
    left_join(site_invite_dates%>%
                select(centre_id, date_uploaded))%>%
    filter(!is.na(date_uploaded))%>%
    ungroup()%>%
    summarise(invites=n_distinct(pt_id[!is.na(invite_date)]),
              potential_invitees=n_distinct(pt_id))%>%
    View()
  
  
  invite_data%>%
    mutate(centre_id=as.character(centre_id))%>%
    left_join(site_invite_dates%>%
                select(centre_id, date_uploaded))%>%
    filter(!is.na(date_uploaded))%>%
    ungroup()
  
    
invite_data%>%
  select(pt_id, centre_id,invite_date, date_valid_from)%>%
  left_join(
    trust_labs%>%
    mutate(pt_id=as.character(pt_id)))%>%
  mutate(centre_id=as.character(centre_id))%>%
  left_join(site_invite_dates%>%
                select(centre_id, date_uploaded))%>%
  filter(!is.na(date_uploaded))%>%

  distinct(pt_id, chol_value, chol_date, centre_id, invite_date, date_uploaded, date_valid_from)%>%
  
    # mutate(invite_pre_screened=if_else(invite_date<date_uploaded, "Invite not pre-screened", "Invite pre-screened"))%>%

  
    
    mutate(pre_screening_cut_off=if_else(
      date_uploaded >=as.Date("2019-12-02", format="%Y-%m-%d") | 
            (!is.na(invite_date) & invite_date>=as.Date("2019-12-02", format="%Y-%m-%d")), 
      3.5, 4.0)
      )%>%
    mutate(cholesterol_time_before_upload = difftime(date_uploaded,chol_date,units="days")/30,
           cholesterol_time_before_invite = difftime(invite_date,chol_date,units="days")/30)%>%
    
    group_by(pt_id)%>%
    slice_max(chol_date)%>%
    slice_min(date_valid_from)%>%
    slice_min(invite_date)%>%
    ungroup()%>%
    
  
    mutate(invite_type = case_when(
      date_valid_from < date_uploaded & chol_value < pre_screening_cut_off & is.na(invite_date)~ "Invite pre-screened, avoided" ,
      
      invite_date < date_uploaded & chol_value < pre_screening_cut_off & !is.na(invite_date)~ "Invite not pre-screned, could have been avoided" ,
      
      invite_date > date_uploaded & chol_value >= pre_screening_cut_off & !is.na(invite_date) | 
        (pre_screening_cut_off ==3.5 & (cholesterol_time_before_upload>18 | cholesterol_time_before_invite >18) & !is.na(invite_date)) |
           (!is.na(invite_date) & chol_value>pre_screening_cut_off & pre_screening_cut_off ==3.5 & cholesterol_time_before_upload <18) | 
              (pre_screening_cut_off ==4 & !is.na(invite_date) &!is.na(chol_value))~ "Invite pre-screened, passed",
      
      invite_date < date_uploaded & chol_value >= pre_screening_cut_off & !is.na(invite_date)~ "Invite not pre-screened, would pass",
      chol_value>= pre_screening_cut_off & is.na(invite_date) & cholesterol_time_before_upload <=18 | 
        (is.na(invite_date) & cholesterol_time_before_upload >18)~ "Not invited, would pass",
      
      chol_value < pre_screening_cut_off & !is.na(invite_date) & cholesterol_time_before_upload <=18 & cholesterol_time_before_invite <=18 ~ "Invite pre-screened, failed cut-off but invited regardless",
      is.na(chol_value) & is.na(invite_date)~"Not invited, cholesterol result unavailable",
      is.na(chol_value) & !is.na(invite_date) ~ "Invited, cholesterol result unavailable"))%>%
    count(invite_type)%>%
    mutate(prop_of_invites = round(n/199897*100,1),
           prop_of_potential_invites = round(n/261031*100,1),
           prop_of_potential_invites_with_data=round(n/139096*100,1))%>%
    
    View()


## proportions entering run-in (based on categories above)


invite_data%>%
  select(pt_id, centre_id,invite_date, date_valid_from)%>%
  left_join(
    trust_labs%>%
      mutate(pt_id=as.character(pt_id)))%>%
  mutate(centre_id=as.character(centre_id))%>%
  left_join(site_invite_dates%>%
              select(centre_id, date_uploaded))%>%
  filter(!is.na(date_uploaded))%>%
  
  distinct(pt_id, chol_value, chol_date, centre_id, invite_date, date_uploaded, date_valid_from)%>%
  
  # mutate(invite_pre_screened=if_else(invite_date<date_uploaded, "Invite not pre-screened", "Invite pre-screened"))%>%
  
  
  
  mutate(pre_screening_cut_off=if_else(
    date_uploaded >=as.Date("2019-12-02", format="%Y-%m-%d") | 
      (!is.na(invite_date) & invite_date>=as.Date("2019-12-02", format="%Y-%m-%d")), 
    3.5, 4.0)
  )%>%
  mutate(cholesterol_time_before_upload = difftime(date_uploaded,chol_date,units="days")/30,
         cholesterol_time_before_invite = difftime(invite_date,chol_date,units="days")/30)%>%
  
  group_by(pt_id)%>%
  slice_max(chol_date)%>%
  slice_min(date_valid_from)%>%
  slice_min(invite_date)%>%
  ungroup()%>%
  
  
  mutate(invite_type = case_when(
    date_valid_from < date_uploaded & chol_value < pre_screening_cut_off & is.na(invite_date)~ "Invite pre-screened, avoided" ,
    
    invite_date < date_uploaded & chol_value < pre_screening_cut_off & !is.na(invite_date)~ "Invite not pre-screned, could have been avoided" ,
    
    invite_date > date_uploaded & chol_value >= pre_screening_cut_off & !is.na(invite_date) | 
      (pre_screening_cut_off ==3.5 & (cholesterol_time_before_upload>18 | cholesterol_time_before_invite >18) & !is.na(invite_date)) |
      (!is.na(invite_date) & chol_value>pre_screening_cut_off & pre_screening_cut_off ==3.5 & cholesterol_time_before_upload <18) | 
      (pre_screening_cut_off ==4 & !is.na(invite_date) &!is.na(chol_value))~ "Invite pre-screened, passed",
    
    invite_date < date_uploaded & chol_value >= pre_screening_cut_off & !is.na(invite_date)~ "Invite not pre-screened, would pass",
    chol_value>= pre_screening_cut_off & is.na(invite_date) & cholesterol_time_before_upload <=18 | 
      (is.na(invite_date) & cholesterol_time_before_upload >18)~ "Not invited, would pass",
    
    chol_value < pre_screening_cut_off & !is.na(invite_date) & cholesterol_time_before_upload <=18 & cholesterol_time_before_invite <=18 ~ "Invite pre-screened, failed cut-off but invited regardless",
    is.na(chol_value) & is.na(invite_date)~"Not invited, cholesterol result unavailable",
    is.na(chol_value) & !is.na(invite_date) ~ "Invited, cholesterol result unavailable"))%>%
  
  left_join(participant_status%>%
              select(pt_id,study_status_cat))%>%

  mutate(study_status_cat=replace_na(study_status_cat, "No reply"))%>%
  # count(invite_type,study_status_cat)%>%
  
  # filter to people entering run-in
  mutate(entered_run_in=if_else(!study_status_cat %in% c("No reply","Registered","Screen failure/incomplete"),"Yes","No"),
         screened=if_else(study_status_cat %in% c("Consent withdrawal","Randomised","Screen failure/incomplete","Withdrawal/stopped during run-in"),"Yes","No"))%>%
  group_by(invite_type)%>%
  summarise(total_entered_run_in=length(entered_run_in[entered_run_in=="Yes"]),
            total_screened=length(screened[screened=="Yes"]),
            total=length(entered_run_in),
            prop_vs_total=round(total_entered_run_in/total*100,1),
            prop_vs_screened=round(total_entered_run_in/total_screened*100,1))%>%View()

    

## characteristics of people entering run-in (based on pre-screening status)----


medical_history%>%
  select(pt_id,mhterm, mhoccur, mhpresp)%>%
  mutate(pt_id=as.character(pt_id))%>%
  left_join(participant_status%>%
              select(pt_id,study_status_cat))%>%
  
  # filter to people entering run-in
  filter(!study_status_cat %in% c("Registered","Screen failure/incomplete"))%>%
  left_join(participant_details%>%
            select(pt_id=participant_id, centre_id))%>%
  
  # filter to people in sites with cholesterol data
  mutate(centre_with_data = if_else(
    centre_id %in%
      (feasibility%>%
         filter(outcome=="Data uploaded")%>%
         select(centre_id)%>%
         .[[1]]),
    "Y","N"))%>%
    filter(centre_with_data=="Y")%>%
  
  # join invite dates
  left_join(invite_data%>%
              distinct(pt_id, invite_date)%>%
              group_by(pt_id)%>%
              slice_min(invite_date)%>%
              ungroup())%>%
  left_join(site_invite_dates%>%
              select(centre_id, date_uploaded))%>%
  
  # compute flag for centralised pre-screening status
  mutate(`Central pre-screening` = case_when(invite_date<date_uploaded~"None",
                                             invite_date>date_uploaded & invite_date<"2019-12-02" ~"4.0 cutoff",
                                             invite_date>date_uploaded & invite_date>="2019-12-02" ~"3.5 cutoff"))%>%
  mutate(`Central pre-screening` = factor(`Central pre-screening`, levels=c("None", "4.0 cutoff","3.5 cutoff")))%>%
  filter(!is.na(`Central pre-screening`))%>%

  # join screening POC levels
  left_join(individual_chol_values_all%>%
              filter(Source=="POC")%>%
              group_by(pt_id)%>%
              slice_min(date)%>%
              slice_max(result)%>%
              distinct(pt_id, result)%>%
              select(pt_id, `POC cholesterol`=result))%>%
  
 
  
  # compute medical history at screening
  mutate(mhoccur=if_else(mhpresp=="N","Missing",mhoccur))%>%
  mutate(pt_id=as.character(pt_id))%>%
  filter(mhterm %in% c("Diabetes",
                       "Type 2 diabetes",
                       "Type 1 diabetes",
                       "CABG",
                       "PCI",
                       "Myocardial infarction",
                       # "Stroke",
                       "Haemorrhagic stroke",
                       "Ischaemic stroke",
                       "Unknown aetiology stroke",
                       "Lower limb arterial revascularisation",
                       "Aortic aneurysm repair (surgery or stent)"))%>%
  pivot_wider(c(pt_id,`Central pre-screening`, `POC cholesterol`), names_from = "mhterm", values_from="mhoccur")%>%
  mutate(Diabetes=if_else(is.na(Diabetes) & (`Type 2 diabetes` == "Y" | `Type 1 diabetes` == "Y"), "Y", Diabetes))%>%
  select(-c(`Type 1 diabetes`,`Type 2 diabetes`))%>%
  
  
  # join age and sex
  left_join(participant_details%>%
              select(pt_id=participant_id,
                     age,
                     sex))%>%
  mutate(across(-c(age, `POC cholesterol`), ~replace_na(.,"N")))%>%
  
  
  # join statin intensity at screening
  
  left_join(
    screening_statins)%>%
  
  select(Age=age, 
         Sex=sex,
         `POC cholesterol`,
         `High intensity statin` = statin_intensity,
         Diabetes,
         `Myocardial infarction`,
         CABG,
         PCI,
         `Ischaemic stroke`,
         `Haemorrhagic stroke`,
         `Unknown aetiology stroke`,
         `Lower limb arterial revascularisation`,
         `Aortic aneurysm repair (surgery or stent)`,
         `Central pre-screening`)%>%
  
 
 
  # build table
  tbl_summary(by="Central pre-screening",
              missing="no",
              statistic = list(all_continuous() ~ "{mean} ({sd})"),
              value=list(Sex~"F",
                         Diabetes~"Y",
                         CABG~"Y",
                         PCI~"Y",
                         `Myocardial infarction`~"Y",
                         `Haemorrhagic stroke`~"Y",
                         `Ischaemic stroke`~"Y",
                         `Unknown aetiology stroke`~"Y",
                         `Lower limb arterial revascularisation`~"Y",
                         `Aortic aneurysm repair (surgery or stent)`~"Y",
                         `High intensity statin`~"High"
                         ))%>%
  modify_spanning_header(all_stat_cols() ~ "**Centralised cholesterol pre-screening**") %>%
  as_flex_table()%>%
  save_as_docx(path="Outputs/Tables/baseline_characteristics_entered_run_in_before_after_pre_screening.docx",
               pr_section = sect_properties)





  

  
  
