-   [SETUP](#setup)
    -   [packages](#packages)
-   [INPUTS](#inputs)
    -   [New Settlement Data](#new-settlement-data)
    -   [Cleaning Logs](#cleaning-logs)
-   [DEALING WITH NEW SETTLEMENT
    DATA](#dealing-with-new-settlement-data)
    -   [Reformatting Data/ Minor
        Manipulations](#reformatting-data-minor-manipulations)
    -   [Checking/Cross Referencing Settlement Data, Cleaning Logs and
        Assessment
        Data](#checkingcross-referencing-settlement-data-cleaning-logs-and-assessment-data)
    -   [Exact Matches](#exact-matches)
    -   [Finding The Closest Point](#finding-the-closest-point)
    -   [Evaluating the closest point](#evaluating-the-closest-point)
    -   [NEW SETTLEMENT OUTPUTS](#new-settlement-outputs)
-   [AGGREATION/ANALYSIS](#aggreationanalysis)
    -   [County Level Aggregation](#county-level-aggregation)
    -   [Hexagonal Aggregation](#hexagonal-aggregation)

<!-- # SOUTH SUDAN AOK DATA PROCESSING TUTORIAL -->

This project was developed in January 2020 to automate South Sudan AOK
data collation and analysis. This document serves as a tutorial for the
the next Analyst. As this Github will no longer be maintaine after March
6 2020 it is recommended that this github be forked by the responsible
GIS/Data Unit Manager and the fork be maintained and updated.

SETUP
=====

packages
--------

You will need the packages below. This analysis is dependent on the
butteR package which can be downloaded from github (link below).

``` r
# install.packages("devtools")
devtools::install_github("zackarno/butteR")
```

``` r
library(tidyverse)
library(butteR)
library(koboloadeR)
library(lubridate)
library(sf)
source("scripts/functions/aok_aggregation_functions.R")
source("scripts/functions/aok_cleaning_functions.R")
```

INPUTS
======

New Settlement Data
-------------------

``` r
month_of_assessment<-"2020-02-01"

admin_gdb<- "../../gis_data/gis_base/boundaries/county_shapefile"
master_settlement<-read.csv("inputs/48_December_2019/SSD_Settlements_V37.csv", stringsAsFactors = FALSE)
colnames(master_settlement)<-paste0("mast.",colnames(master_settlement))
master_settlement_sf<- st_as_sf(master_settlement,coords=c("mast.X","mast.Y"), crs=4326)


new_settlements<-butteR::read_all_csvs_in_folder(input_csv_folder = "inputs/2020_02/new_settlements")
new_sett<-bind_rows(new_settlements)
new_sett<-new_sett %>% filter(action=="Map")

adm2<- st_read(admin_gdb,"ssd_admbnda_adm2_imwg_nbs_20180401" )
```

    ## Reading layer `ssd_admbnda_adm2_imwg_nbs_20180401' from data source `C:\02_REACH_SSD\gis_data\gis_base\boundaries\county_shapefile' using driver `ESRI Shapefile'
    ## Simple feature collection with 78 features and 17 fields
    ## geometry type:  POLYGON
    ## dimension:      XY
    ## bbox:           xmin: -477970.6 ymin: 385787.9 xmax: 827553.4 ymax: 1352704
    ## epsg (SRID):    32636
    ## proj4string:    +proj=utm +zone=36 +datum=WGS84 +units=m +no_defs

``` r
adm2<-st_transform(adm2,crs=4326)
new_sett_sf<-st_as_sf(new_sett,coords=c("long","lat"), crs=4326)

# LOAD RAW DATA -----------------------------------------------------------
# aok_raw<-download_aok_data(keys_file = "scripts/functions/keys.R")
# write.csv(aok_raw,"inputs/2020_02/2020_02_FEB_AOK_RAW_20200301.csv")
aok_raw<-read.csv("inputs/2020_02/2020_02_FEB_AOK_RAW_20200301.csv", stringsAsFactors = F,na.strings = c("n/a","", ""))
```

Cleaning Logs
-------------

Once cleaning logs are revieved this chunk can be filled. First step is
to compile all of the logs into one. Next we can check and implement
them using the two butteR tools commented out below.

``` r
# STEP 1 COMPILE CLEANING LOGS --------------------------------------------

# cleaning_logs<-butteR::read_all_csvs_in_folder(input_csv_folder = "inputs/2020_02/cleaning_logs")
# cleaning_log<-bind_rows(cleaning_logs)

# CHECK CLEANING LOG
# butteR::check_cleaning_log()
# butteR::implement_cleaning_log()

# OUTPUT ISSUES TO AOS

# IMPLEMENT CLEANING LOG
```

DEALING WITH NEW SETTLEMENT DATA
================================

Reformatting Data/ Minor Manipulations
--------------------------------------

``` r
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
```

Checking/Cross Referencing Settlement Data, Cleaning Logs and Assessment Data
-----------------------------------------------------------------------------

Compare new settlements to data after initial round of data cleaning
implementation too make sure that AO has not already dealt wih the
settlement in the cleaning log. Technically, if the settlement is
addressed in the cleaning log they should have put “cleaning\_log” under
the action column in the new settlement data set.

If the settlement in the new settlement sheet has been addressed in the
cleaning log, remove it from the new settlement sheet.

``` r
# CHECK IF NEW SETTLEMENTS HAVE BEEN FIXED IN CL --------------------------

aok_clean1<-aok_raw

# aok_clean1[aok_clean1$X_uuid=="b4d97108-3f34-415d-9346-f22d2aa719ea","D.info_settlement_other"]<-NA
# aok_clean1[aok_clean1$X_uuid=="b4d97108-3f34-415d-9346-f22d2aa719ea","D.info_settlement"]<-"Bajur"


remove_from_new_sett<-aok_clean1 %>%
  filter(X_uuid %in% new_sett_sf$uuid  & is.na(D.info_settlement_other))%>%
  select(X_uuid,D.info_settlement) %>% pull(X_uuid)

new_sett_sf<- new_sett_sf %>% filter(!uuid %in% remove_from_new_sett)
```

Exact Matches
-------------

It is possible that the enumerator entered “other” for D.new\_settlement
and then wrote a settlement that already existed. These cases are easy
to find. If this situation occurs, it is an error during data
collection/cleaning and should therefore be addressed in a cleaning log
for documentation purposes. The extract\_matches\_to\_cl to transform
this information into a cleaning log, which can then be implemented with
the butteR::implement\_cleaning\_log function.

``` r
# NEW SETTLEMENT DATA WHICH MATCHES MASTER SETTLEMENTS EXACTLY ------------

exact_matches1<-new_sett_sf %>%
  mutate(matched_where= case_when(new.enum_sett_county %in% master_settlement$mast.settlement_county_sanitized~"enum", #CHECK WITH ENUMS INPUT
                                  new.adm2_sett_county %in% master_settlement$mast.settlement_county_sanitized~"shapefile_only" #CHECK WITH SHAEFILE COUNTY
  )) %>%
  filter(!is.na(matched_where)) #ONLY RETURN EXACT MATCHES

# WRITE EXACT MATCHES TO CLEANING LOG TO THEN IMPLEMENT ON DATA.
aok_exact_matches_cl<-exact_matches_to_cl(exact_match_data = exact_matches1,user = "Jack")

#NOW IMPLEMENT THIS CLEANING LOG!
aok_clean2<-butteR::implement_cleaning_log(df = aok_clean1,df_uuid = "X_uuid",
                                           cl = aok_exact_matches_cl,
                                           cl_change_type_col = "change_type",
                                           cl_change_col = "indicator",
                                           cl_uuid = "uuid",
                                           cl_new_val = "new_value")
```

    ## character(0)
    ## [1] "NOT IN DATASET"
    ## [1] "no change_response in log"
    ## [1] "no surveys to remove in log"

Finding The Closest Point
-------------------------

We will next use the butteR::closest\_distance\_rtree tool to find the
closest point in the master settlement list to each of the remaining new
settlements. The following code just runs this tool, cleans up the
output and also performs fuzzy string distance measurement which may be
helpful.

``` r
#EXTRACT NEW SETTLEMENTS WHICH DO NO MATCH
new_sett_sf_unmatched<- new_sett_sf %>% filter(!uuid %in% exact_matches1$uuid)

#REMOVE MATCHED SETTLEMENTS FROM MASTER
master_settlement_sf_not_matched<-master_settlement_sf %>%
  filter(mast.settlement_county_sanitized %in% new_sett_sf_unmatched$new.enum_sett_county==FALSE)

# MATCH NEW SETTLEMENT TO CLOSEST SETTLEMENT IN MASTER --------------------

new_with_closest_old<-butteR::closest_distance_rtree(new_sett_sf_unmatched %>%
                                                       st_as_sf(coords=c("X","Y"), crs=4326) ,master_settlement_sf_not_matched)
#CLEAN UP DATASET
new_with_closest_old_vars<-new_with_closest_old %>%
  mutate(new.D.info_settlement_other= D.info_settlement_other %>% gsub("-","_",.)) %>%
  select(uuid,
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
```

Evaluating the closest point
----------------------------

The object, “settlements\_best\_guess,” is the best output to review and
determine if the found closest settlement in the master list should take
the place of the new settlement or not. The user has two options:

1.  The less preferred option: can write this output to a csv and
    manually assess the the table. Add a column called “action”. If the
    the master settlement should replace the “new settlement” put 1,
    otherwise 2. This file will then need to be read back into the
    script. Additionally, cleaning log entries will need to be added for
    any record in which a 1 was perscribed to action.

2.  The preferred option: The code below can be used to interactively
    assess the matched settlements in the R environment. When running
    the code in an R script a menu will prompt you through the
    interaction. Each line in the settlements\_best\_guess will be
    printed with the prompt: “1. fix with master, or 2. master is not
    correct.” The user must press 1 or two based on there understanding
    of the names and distances provided.

After cycling through each record, the resulting object is a list of two
data frames. The first data frame is the same list used as the input
with the additional column “action” where action=1 means fix dataset
with new settlement and 2 means this is a new settlement. The second
data frame is the auto-generated cleaning log for all records where the
user decided to fix the settlement with a settlement from the master
list.

``` r
# HOWEVER, TO KEEP EVERYTHING IN THE R ENVIRONMENT- HERE IS AN INTERACTIVE FUNCTION TO MODIFY THE SETTLEMENT BEST GUESS DF IN PLACE
# OUTUT WILL BE A CLEANING LOG (IF THERE ARE CHANGES TO BE MADE)
new_settlement_evaluation<-evaluate_unmatched_settlements(user= "zack",new_settlement_table = settlements_best_guess)

new_settlement_evaluation$checked_setlements
new_settlement_evaluation$cleaning_log

# butteR::implement_cleaning_log()
```

NEW SETTLEMENT OUTPUTS
----------------------

### Generate New Itemset

Interactive functions cannot be used in a knitted document. Therefore,
to fully understand this functionality the user will have to use this
code in the R-Script.

For the sake of showing how to produce the new itemset for the odk tool
and producing the new master settlement list I will create the action
column in R and name it appropriately

``` r
#this would normally be unnecessary as the objects would be created from the interactive function
#############################################################################
new_settlement_evaluation<-list()
new_settlement_evaluation$checked_setlements<-settlements_best_guess
new_settlement_evaluation$checked_setlements$action<-c(1,1,2,2,2)
#############################################################################

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
```

### Generate New Master Settlement List

Only settlements determined to be actually new settlements should be
added to the master settlement list. The following code reads the master
settlement in again and adds all of the checked new settlements that are
determined to be new.

``` r
# NEXT WE ADD THE NEW SETTLEMENTS TO THE MASTER LIST
master_settlement<-read.csv("inputs/48_December_2019/SSD_Settlements_V37.csv", stringsAsFactors = FALSE)

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
```

AGGREATION/ANALYSIS
===================

County Level Aggregation
------------------------

Once the cleaning log(s) have been implemented and the new settlements
dealt with it is time to aggregate/analyze the data. It is important to
note that any cleaning logs generated from the new settlement process
must be implemented and binded to the original compiled cleaning log for
documentation purposes.

The aggregation relies on functions built many years ago. It is
recommended that this process be streamlined in the future with a new
script. However, given time constraints we can continue to use the old
process since it does work.

``` r
# insert aggregation script here or source it
```

Once aggregation is completed it should be written to csv. This csv can
be binded to the long term data using the long term data aggreation
script. The long term data is the input for the Tableau workbook.

``` r
# insert code here
```

Hexagonal Aggregation
---------------------

The monthly data should then be aggregated to the hexagonal grid for
Factsheet maps. The grid has already been created and can simply be
loaded.

``` r
# insert code here or source script
```
