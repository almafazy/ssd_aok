#Get started----
library(tidyverse)
library(butteR)
library(koboloadeR)
library(lubridate)
library(sf)
library(rtree)
library(purrr)
library(fs)
library(tidyverse)
library(stringr)



month_of_assessment<-"2020-02-01"
admin_gdb <- "inputs/gis_data/boundaries"
logs_dir <- "inputs/2020_02/cleaning_logs"
latest_settlement_data <- "inputs/2020_02/SSD_Settlements_V38.csv"
raw_data <- "inputs/2020_02/AoK_Feb2020.csv"
tableau_filter <-"Tableau/Table_data/2020_02/SSD_Filter.csv"


source("scripts/functions/aok_aggregation_functions.R")
source("scripts/functions/aok_cleaning_functions.R")
source("scripts/functions/aok_aggregate_by_county_wrapped.R")



#Load data ------------------------------

# LOAD AOK RAW DATA
raw_aok <-butteR::read_all_csvs_in_folder(input_csv_folder = "inputs/2020_02/raw_data")
raw_aok <-bind_rows(raw_aok)


aok_raw <- raw_aok


#aok_raw <-read.csv(raw_data, stringsAsFactors = F,na.strings = c("n/a","", ""))

#making sure all our uuid in the dataset are unique

aok_raw <- distinct(aok_raw,X_uuid, .keep_all= TRUE)

##aok_clean <-read.csv("outputs/REACH_SSD_AoKclean3.csv", stringsAsFactors = F,na.strings = c("n/a","", ""))

#Add latest settlement data and and then create its shapefile version of wgs84 projection
master_settlement<-read.csv(latest_settlement_data, stringsAsFactors = FALSE)
master_settlement <- distinct(master_settlement,NAMECOUNTY, .keep_all= TRUE)

colnames(master_settlement)<-paste0("mast.",colnames(master_settlement))
master_settlement_sf<- st_as_sf(master_settlement,coords=c("mast.X","mast.Y"), crs=4326)





#add in the csv cleaning logs and add base name for feedback purposes

cleaning_logz <- fs::dir_ls(logs_dir,regexp = "\\.csv$")
cleaning_logz  %>% map_dfr(cleaning_logz)
cleaning_log <-cleaning_logz %>% map_dfr(read_csv,.id = "source")
cleaning_log <- cleaning_log %>% mutate(source = str_remove_all(source, "inputs/2020_02/cleaning_logs/REACH_SSD_AoK_")) %>%
                mutate(source = str_remove_all(source, "_Cleaning_Log_Feb2020.csv"))


names(cleaning_log)[names(cleaning_log) == "source"] <- "A.base"


#Most prevalent error types per base
logs_base <- cleaning_log %>% select(A.base,Sectors,indicator) %>% filter(cleaning_log$change_type !="no_action") %>%
            group_by(A.base,Sectors,indicator) %>% summarise(total_count = n()) %>% arrange(A.base,desc(total_count)) %>%
            mutate(rank = dense_rank(desc(total_count))) %>% arrange(A.base,Sectors, rank,indicator)


cleaning_log <- cleaning_log %>%
  filter(!str_detect(indicator,"settlement"))


#we had 2 sets of uuid columns. i.e. different variations was used in saving the uuid

cleaning_log$uuid <- ifelse(is.na(cleaning_log$uuid), cleaning_log$ï..uuid, cleaning_log$uuid)


#Write out the finalised compiled

write.csv(
  logs_base,
  file = "outputs/logs_base_summary.csv",
  na = "NA",
  row.names = FALSE)


write.csv(
  cleaning_log,
  file = "outputs/cleaning_log.csv",
  na = "NA",
  row.names = FALSE)




#Zacks compilation code. wanted to pull in the file name and didn't know how to modify his
#cleaning_logs<-butteR::read_all_csvs_in_folder(input_csv_folder = "inputs/2020_02/cleaning_logs")
#cleaning_log<-bind_rows(cleaning_logs)



# Reflect cleaning log------


cleaning_check <- butteR::check_cleaning_log(df = aok_raw,
                            df_uuid = "X_uuid",
                            cl = cleaning_log,
                            cl_change_type_col ="change_type",
                            cl_change_col = "indicator",
                            cl_uuid = "uuid",
                            cl_new_val = "new_value")


aok_clean <- butteR::implement_cleaning_log(df = aok_raw,
                                df_uuid = "X_uuid",
                                cl = cleaning_log,
                                cl_change_type_col ="change_type",
                                cl_change_col = "indicator",
                                cl_uuid = "uuid",
                                cl_new_val = "new_value")



#Making sure no duplicates in settlement list




#Check settlement match------

#aok_clean <- aok_clean %>% mutate(mast.NAMECOUNTY = paste0(D.info_settlement, D.info_county))
#aok_clean <- aok_clean %>% filter(!is.na(D.info_county))
#aok_clean <- left_join(aok_clean, master_settlement, by = "mast.NAMECOUNTY")


#aok_clean_summarise <- aok_clean %>%  group_by(D.info_county,mast.NAMECOUNTY) %>% summarise(total_count = n())

#unmatched_records <-aok_clean %>% filter(is.na(mast.COUNTYJOIN)) %>% select(X_uuid,D.info_state,D.info_county,D.info_settlement,D.info_settlement_other,D.info_settlement_final,mast.NAMECOUNTY)








#KI level clean data
write.csv(
  aok_raw,
  file = "outputs/AOK_clean_data_20200201.csv",
  na = "NA",
  row.names = FALSE)







# write.csv(aok_raw, "aok_clean_test.csv")
# ?implement_cleaning_log

# <- ISSUES TO AOS

# IMPLEMENT CLEANING LOG




# CREATE AOK_CLEAN
# STEP 2 NEW SETTLEMENTS --------------------------------------------------
# PERHAPS THE MOST DIFFICULT STEP
# OUTPUTS: NEW ITEMSET, NEW MASTER SETTLEMENT FILE, CLEANING LOG

new_settlements<-butteR::read_all_csvs_in_folder(input_csv_folder = "inputs/2020_02/new_settlements")
new_sett<-bind_rows(new_settlements)
new_sett<-new_sett %>% filter(action=="Map")

adm2<- st_read(admin_gdb,"ssd_admbnda_adm2_imwg_nbs_20180401")
adm2<-st_transform(adm2,crs=4326)
new_sett_sf<-st_as_sf(new_sett,coords=c("long","lat"), crs=4326)





ggplot()+geom_sf(data=adm2)+
  geom_sf_label(data= new_sett_sf,aes(label =D.info_settlement_other))+
  geom_sf(data=new_sett_sf)



# SPATIAL JOIN
new_sett_sf<-new_sett_sf %>% st_join( adm2 %>% dplyr::select(adm2=admin2RefN))

new_sett_sf<-new_sett_sf %>%
  mutate(
    new.enum_sett_county=paste0(D.info_settlement_other,D.info_county) %>% tolower_rm_special(),
    new.adm2_sett_county=paste0(D.info_settlement_other,adm2) %>% tolower_rm_special()
  )

master_settlement_sf<-master_settlement_sf %>%
  mutate(
    mast.settlement_county_sanitized= mast.NAMECOUNTY %>% tolower_rm_special()
  )


# CHECK IF NEW SETTLEMENTS HAVE BEEN FIXED IN CL --------------------------

aok_clean1<-aok_clean

# aok_clean1[aok_clean1$X_uuid=="b4d97108-3f34-415d-9346-f22d2aa719ea","D.info_settlement_other"]<-NA
# aok_clean1[aok_clean1$X_uuid=="b4d97108-3f34-415d-9346-f22d2aa719ea","D.info_settlement"]<-"Bajur"

#names(new_sett_sf)[names(new_sett_sf) == "ï..uuid"] <- "X_uuid"

#mapped_other <- left_join(new_sett_sf ,aok_clean1, by = "X_uuid")
#remove_from_new_sett <- mapped_other %>% filter(is.na(D.info_settlement))  %>% pull(X_uuid)


#new_sett_sf<- new_sett_sf %>% filter(!X_uuid %in% remove_from_new_sett)

names(new_sett_sf)[names(new_sett_sf) == "ï..uuid"] <- "X_uuid"

# NEW SETTLEMENT DATA WHICH MATCHES MASTER SETTLEMENTS EXACTLY ------------

exact_matches1<-new_sett_sf %>%
  mutate(matched_where= case_when(new.enum_sett_county %in% master_settlement_sf$mast.settlement_county_sanitized~"enum", #CHECK WITH ENUMS INPUT
                                  new.adm2_sett_county %in% master_settlement_sf$mast.settlement_county_sanitized~"shapefile_only" #CHECK WITH SHAEFILE COUNTY
  )) %>%
  filter(!is.na(matched_where)) #ONLY RETURN EXACT MATCHES

# WRITE EXACT MATCHES TO CLEANING LOG TO THEN IMPLEMENT ON DATA.
aok_exact_matches_cl<-exact_matches_to_cl(exact_match_data = exact_matches1,user = "Jack",uuid_col  = "X_uuid")

#NOW IMPLEMENT THIS CLEANING LOG!
aok_clean2<-butteR::implement_cleaning_log(df = aok_clean1,
                                           df_uuid = "X_uuid",
                                           cl = aok_exact_matches_cl,
                                           cl_change_type_col = "change_type",
                                           cl_change_col = "indicator",
                                           cl_uuid = "uuid",
                                           cl_new_val = "new_value")

#EXTRACT NEW SETTLEMENTS WHICH DO NO MATCH
new_sett_sf_unmatched<- new_sett_sf %>% filter(!X_uuid %in% exact_matches1$X_uuid)

#REMOVE MATCHED SETTLEMENTS FROM MASTER
master_settlement_sf_not_matched<-master_settlement_sf %>%
  filter(mast.settlement_county_sanitized %in% new_sett_sf_unmatched$new.enum_sett_county==FALSE)

# MATCH NEW SETTLEMENT TO CLOSEST SETTLEMENT IN MASTER --------------------
new_with_closest_old<-butteR::closest_distance_rtree(new_sett_sf_unmatched %>%
                                                       st_as_sf(coords=c("X","Y"), crs=4326) ,master_settlement_sf_not_matched)
### ----- can't run this bit as can't get Rtree on my version of R

#CLEAN UP DATASET
new_with_closest_old_vars<-new_with_closest_old %>%
  mutate(new.D.info_settlement_other= D.info_settlement_other %>% gsub("-","_",.)) %>%
  select(X_uuid,
         new.A.base=A.base,
         new.county_enum=D.info_county,
         new.county_adm2= adm2,
         new.sett_county_enum=new.enum_sett_county,
         new.sett_county_adm2= new.adm2_sett_county,
         new.D.info_settlement_other=D.info_settlement_other,
         mast.settlement=mast.NAMEJOIN,
         mast.settlement_county_sanitized,
         dist_m)



# ADD A FEW USEFUL COLUMNS - THIS COULD BE WRITTEN TO A CSV AND WOULD BE THE BEST OUTPUT TO BE REVIEWED
settlements_best_guess<-new_with_closest_old_vars %>%
  mutate(gte_50=ifelse(dist_m<500, " < 500 m",">= 500 m"),
         string_proxy=stringdist::stringdist(a =new.sett_county_enum,
                                             b= mast.settlement_county_sanitized,
                                             method= "dl", useBytes = TRUE)
  ) %>%
  arrange(dist_m,desc(string_proxy))


# HOWEVER, TO KEEP EVERYTHING IN THE R ENVIRONMENT- HERE IS AN INTERACTIVE FUNCTION TO MODIFY THE SETTLEMENT BEST GUESS DF IN PLACE
# OUTUT WILL BE A CLEANING LOG (IF THERE ARE CHANGES TO BE MADE)
new_settlement_evaluation<-evaluate_unmatched_settlements(user= "jack",new_settlement_table = settlements_best_guess, uuid_col = "X_uuid")

new_settlement_evaluation$checked_setlements
new_settlement_evaluation$cleaning_log

# debugonce(implement_cleaning_log)
aok_clean3<-butteR::implement_cleaning_log(df = aok_clean2,df_uuid = "X_uuid",
                                           cl =new_settlement_evaluation$cleaning_log ,
                                           cl_change_type_col = "change_type",
                                           cl_change_col = "suggested_indicator",
                                           cl_uuid = "uuid",
                                           cl_new_val = "suggested_new_value")





# ADD NEW SETTLEMENTS TO NEW ITEMSET  -------------------------------------

#put into itemset format
new_sets_to_add_itemset<-new_settlement_evaluation$checked_setlements %>%
  filter(action==2) %>%
  mutate(
    list_name="settlements",
    label= new.D.info_settlement_other %>% gsub("_","-", .)
  ) %>%
  select(list_name,name=new.D.info_settlement_other, label,admin_2=new.county_adm2)

# read in previous itemset
itemset<-read.csv("inputs/tool/REACH_SSD_AoK_V38_Febuary2020/itemsets.csv", strip.white = T, stringsAsFactors = T,na.strings = c(" ",""))
itemset_not_other<-itemset %>% filter(name!="other")
itemset_other<- itemset %>% filter(name=="other")


itemset_binded<-bind_rows(list(new_sets_to_add_itemset,itemset_not_other)) %>% arrange(admin_2)
itemset_full_binded<- bind_rows(list(itemset_binded,itemset_other))

#write to csv for next aok round
write.csv(itemset_full_binded, "AoK_itemset_Feb2020.csv")

# NEXT WE ADD THE NEW SETTLEMENTS TO THE SHAPEFILE ------------------------

# add to master file ------------------------------------------------------
# new_sett<-read.csv("inputs/new_settlements/20200207_New_settlement_Jan2020_ZA.csv", stringsAsFactors = FALSE)
# new_sett %>% head()
master_settlement<-read.csv("inputs/48_December_2019/SSD_Settlements_V38.csv", stringsAsFactors = FALSE)
# master_settlement$DATA_SOURC
new_sett_sf
new_setts_add_to_master<-new_settlement_evaluation$checked_setlements %>%
  filter(action==2) %>%
  mutate(
    NAME= new.D.info_settlement_other %>% gsub("_","-", .),
    NAMEJOIN= mast.settlement,
    NAMECOUNTY=paste0(NAMEJOIN,new.county_adm2),
    COUNTYJOIN= new.county_adm2 ,
    DATE= month_of_assessment %>% ymd(),
    DATA_SOURC="AOK",
    IMG_VERIFD= 0
  ) %>% #get coordinates from field data back in
  left_join(new_sett_sf %>%
              st_drop_geometry_keep_coords(), by="uuid") %>%
  filter(!is.na(X)) %>%
  select(NAME,NAMEJOIN,NAMECOUNTY,COUNTYJOIN,DATE,DATA_SOURC,IMG_VERIFD,X,Y)

master_new<-bind_rows(list(new_setts_add_to_master,master_settlement %>% mutate(DATE=dmy(DATE))))

#write to csv v 39
write.csv(master_new, "AoK_mastersettlementlist_Feb2020.csv")



###########################################
#ok assume we have imlpemented all cleaning
############################################
aok_clean3 <-aok_raw

#filter out thse that don't match with settlement data

aok_clean3<- aok_clean3 %>% filter(!is.na(mast.COUNTYJOIN)) %>% select(start:X_validation_status)



#convert all the multuple select rows as numeric
#column headers of multiple select columns

aok_clean3[aok_clean3==0]<- "no"
aok_clean3[aok_clean3==1]<- "yes"



iso_date<- Sys.Date() %>%  str_replace_all("-","_")
#maybe change the way assessment month is represented
aggregated_file_name<- paste0("outputs/", iso_date,"_reach_ssd_aok_data_analysis_basic_FEB2020_Data.csv")

#next start with the remove grouper stuff.
# prev_round<-read.csv("inputs/2020_02/2020_02_13_reach_ssd_aok_clean_data_compiled.csv", stringsAsFactors = FALSE, na.strings=c("", " ", NA, "NA"))
prev_round<-read.csv("inputs/2020_02/REACH_SSD_AoK_LongTermSettlementData.csv", stringsAsFactors = FALSE, na.strings=c("", " ", NA, "NA"))
# debugonce(aggregate_aok_by_county)

# debugonce(aggregate_aok_by_county)






aok_clean_by_county<-aggregate_aok_by_county(clean_aok_data = aok_clean3,aok_previous = prev_round, current_month = "2020-02-01")


names(aok_raw)[names(aok_raw) == "mast.NAMECOUNTY"] <- "D.settlecounty"

matc_data <- inner_join(aok_clean_by_county,master_settlement, by = "D.settlecounty" )




aok_clean_by_county[aok_clean_by_county=="NA"]<- "blanked"

write.csv(aok_clean_by_county,
          aggregated_file_name,
          na = "blanked",
          row.names = FALSE)



# Make longterm output also

aok_longterm <- bind_rows(aok_clean_by_county, prev_round)


aok_longterm <- aok_longterm  %>%  select(month:U.market_now_barriers_at.none)


write.csv(
  aok_longterm,
  file = "outputs/2020_02/REACH_SSD_AoK_LongTermData_Settlement_Feb2019-Feb2020.csv",
  na = "NA",
  row.names = FALSE
)


#Tableau files--------

#Long term dataset
write.csv(
  aok_longterm,
  file = "Tableau/Table_data/2020_02/REACH_SSD_AoK_LongTermSettlementData.csv",
  na = "NA",
  row.names = FALSE
)


#Filter file

#Previous month filter
filter_file <-read.csv(tableau_filter, stringsAsFactors = FALSE)

settlement_count <- as.data.frame(table(master_settlement$mast.COUNTYJOIN))
asssessed_count <- as.data.frame(table(aok_clean_by_county$D.info_county))
filter_current_month <- left_join(settlement_count,asssessed_count, by = "Var1")

colnames(filter_current_month) <- c("county_join", "settlement_count", "assessment_count")
filter_current_month[is.na(filter_current_month)] <- 0
filter_current_month$month <- month_of_assessment
filtercolumns_added <- filter_file %>%  select(county_join,county_label,state_join,state_label,month) %>% group_by(county_join,county_label,state_join,state_label) %>% summarise_all(funs(sum(!is.na(.))))
filter_ <- left_join(filtercolumns_added,filter_current_month, by = "county_join")
filter_ <- filter_ %>%  select(month.y,"county_join", "county_label", "state_join", "state_label","settlement_count", "assessment_count") %>%
  arrange(state_join,county_join)
names(filter_)[names(filter_) == "month.y"] <- "month"





updated_filter <- bind_rows(filter_,filter_file) %>% select(month:assessment_count)

#filter dataset
write.csv(
  updated_filter,
  file = "Tableau/Table_data/2020_02/SSD_Filter.csv",
  na = "NA",
  row.names = FALSE
)

#Settlement data to be used



setttlement_tableau <-  master_settlement_sf %>%
  rename_at(vars(starts_with("mast.")), funs(str_remove(., "mast."))) %>%
  select(everything(), -X.1)

setttlement_tableau <- sf::st_as_sf(setttlement_tableau, coords=geometry, crs= 4326)

setttlement_tableau <- st_transform(setttlement_tableau, 32636)



sf::st_write(setttlement_tableau,"Tableau/Spatial_data/SSD_Settlements.shp", update=TRUE, driver = "ESRI Shapefile")

sf::st_write (setttlement_tableau,"Tableau/Spatial_data/SSD_Settlements.shp", over)


## Hexagonal Aggregation -------------------------------
#READ IN HEX GRID
hex_grid <- st_read(dsn = "inputs/GIS",layer ="Grids_info") %>%
  mutate( id_grid = as.numeric(rownames(.)))

master_sett_new<-master_new %>%
  mutate(id_sett = as.numeric(rownames(.))) %>%
  st_as_sf(coords=c("X","Y"), crs=4326) %>%
  st_transform(crs=st_crs(hex_grid)) %>%
  select(NAME:COUNTYJOIN)

aok_clean3<-aok_clean3 %>%
  mutate(date=month_of_assessment %>% ymd(),
         month=month(date),
         year=year(date),
         #if we use D.info_settlement_final the others wont match until clean
         D.settlecounty=paste0(D.info_settlement,D.info_county)) %>%
  # therefore use D.info_settlement and filter others for tutorial
  filter(D.info_settlement!="other")

sett_w_grid <- st_join(master_sett_new, hex_grid)
assessed_w_grid <-inner_join(sett_w_grid, aok_clean3, by = c("NAMECOUNTY"="D.settlecounty") )


#Aggregate the data to the hexagon grid-level through the following steps:
#  1. Calculate # KIs per settlement (D.ki_coverage)
#  2. Calculate the # of settlements/grid,  and # KIs/grid
#  3. Filter out grids with less than 2 KIs or Settlements (would be good to have citation for this rule).


grid_summary<-assessed_w_grid %>%
  group_by(NAMECOUNTY,State_id) %>%
  summarise(D.ki_coverage=n()) %>%
  group_by(State_id) %>%
  summarise(settlement_num=n() ,ki_num=sum(D.ki_coverage) )

#Filter Grids with less than 2 KIs
grid_summary_thresholded <- grid_summary %>% filter(ki_num > 1, settlement_num > 1)


#Next we will create composite indicators to analyze at the grid level. This may need to be edited to add or #remove composite indicators later.


#create new composites
assessed_w_grid_w_composite<-assessed_w_grid %>%
  mutate(
    idp_sites= ifelse(J.j2.idp_location=="informal_sites",1,0),
    IDP_present= ifelse(F.idp_now=="yes",1,0),
    IDP_time_arrive=  ifelse(F.f2.idp_time_arrive %in% c("1_month","3_month"),1,0),
    IDP_majority=  ifelse( F.f2.idp_perc %in% c("half","more_half"),1,0),
    food_inadequate= ifelse(G.food_now == "no", 1,0),
    less_one_meal = ifelse(G.meals_number %in% c("one", "Less_than_1"),1,0),
    hunger_severe_worse = ifelse(S.shock_hunger %in% c("hunger_severe", "hunger_worst"),1,0),
    wildfood_sick_alltime = ifelse(G.food_wild_emergency=="yes"|G.food_wild_proportion=="all",1,0),
    skipping_days = ifelse(G.food_coping_comsumption.skip_days == "yes",1,0),
    flooded_shelter = ifelse(J.shelter_flooding == "yes",1,0),
    fsl_composite = (food_inadequate +less_one_meal+hunger_severe_worse+wildfood_sick_alltime+skipping_days)/5
  )

#extract new columns added (should be only composite). You can add new composites above and this will still work
vars_to_avg<-names(assessed_w_grid_w_composite)[!names(assessed_w_grid_w_composite)%in%names(assessed_w_grid)]

analyzed_by_grid<-assessed_w_grid_w_composite %>%
  group_by(id_grid, State_id,month,year,date,D.info_state, D.info_county)%>%
  summarise_at(vars(vars_to_avg),mean, na.rm=T)




#Once analyzed you can write the aggreagted data to a csv or left_join it to the original hex data and write #it out as a polygon straight for mapping.


#Filter Grids with less than 2 KIs

analyzed_by_grid_thresholded<-analyzed_by_grid %>%
  filter(State_id %in% grid_summary_thresholded$State_id)

# write.csv(
# analyzed_by_grid_thresholded,
# file = paste0(month_of_assessment %>% str_replace_all("-","_"),"_AoK_hex_aggregations.csv"),
# na = "NA",
# row.names = FALSE)


hex_grid_polygon_with_aggregated_data<-hex_grid %>% left_join(analyzed_by_grid_thresholded %>% st_drop_geometry())

# or write it out to a polgon file for mapping
#using st_write function









