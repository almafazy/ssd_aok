exact_matches_to_cl<-function(exact_match_data,user="Jack",uuid_col){
  if("sf" %in% class(exact_match_data)){
    exact_match_data<-exact_match_data %>% st_drop_geometry()
  }
  cleaning_log1<-exact_match_data %>%
    mutate(uuid= !!sym(uuid_col),
           spotted=user,
           change_type="change_response",
           Sectors="Area_of_Knowledge",
           indicator="D.info_settlement",
           current_value= "other",
           new_value=D.info_settlement_other,
           issue="User chose other when correct name was available") %>%
    select(uuid, spotted:issue)

  cleaning_log2<-exact_match_data %>%
    mutate(uuid= !!sym(uuid_col),
           spotted=user,
           change_type="change_response",
           Sectors="Area_of_Knowledge",
           indicator="D.info_settlement_other",
           current_value= D.info_settlement_other,
           new_value=NA,
           issue="User chose other when correct name was available") %>%
    select(uuid, spotted:issue)

  if(any(exact_match_data[["matched_where"]]=="shapefile_only")){
    wrong_county_data<-exact_match_data %>% filter(matched_where=="shapefile_only")
    cleaning_log3<-wrong_county_data %>%
      mutate(uuid= !!sym(uuid_col),
             spotted=user,
             change_type="change_response",
             Sectors="Area_of_Knowledge",
             indicator="D.info_county",
             current_value= D.info_county,
             new_value=adm2,
             issue="Enumerator filled incorrect county") %>%
      select(uuid, spotted:issue)}

  cl_list<-list(get0("cleaning_log1"),
                get0("cleaning_log2"),
                get0("cleaning_log3"))
  cleaning_log<-bind_rows(cl_list) %>% arrange(uuid)
  return(cleaning_log)}



evaluate_unmatched_settlements<-function(user,new_settlement_table, uuid_col){
  output<-list()
  new_settlement_table$action<-NA

  for ( i in 1: nrow(new_settlement_table)){
    print(new_settlement_table[i,])
    choice <- menu(c("fix with master", "master is not correct"))
    new_settlement_table[i,][["action"]]<- choice
    # do other things

  }
  cleaning_log1<-new_settlement_table %>%
    filter(action==1) %>%mutate(
      uuid=!!sym(uuid_col),
      spotted=user,
      change_type="change_response",
      Sectors="Area_of_Knowledge",
      indicator="",
      current_value= "",
      new_value="",
      issue="",
      suggested_indicator= "D.info_settlement",
      suggested_issue="User chose other when name correct name was available",
      suggested_new_value=mast.settlement) %>%
    select(uuid:suggested_new_value) #need to add uuid into selection on real data
  cleaning_log2<-new_settlement_table %>%
    filter(action==1) %>%mutate(
      uuid=!!sym(uuid_col),
      spotted=user,
      change_type="change_response",
      Sectors="Area_of_Knowledge",
      indicator="",
      current_value= "",
      new_value="",
      issue="",
      suggested_indicator= "D.info_settlement_other",
      suggested_issue="User chose other when name correct name was available",
      suggested_new_value=NA) %>%
    select(uuid:suggested_new_value) #need to add uuid into selection on real data


  cleaning_log_combined<-bind_rows(list(get0("cleaning_log1"), get0("cleaning_log2")))
  output$checked_setlements<-new_settlement_table
  output$cleaning_log<-cleaning_log_combined

  return(output)
}

aok_to_grid<-function(aok_data, settlement_data, grid_data){
  aok_data %>%
    group_by() %>%
    summarise()
  aok_settlement_joined<-aok_data %>% left_join(settlement_data)


}


# returns string w/o leading or trailing whitespace
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
