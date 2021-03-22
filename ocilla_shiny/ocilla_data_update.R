#Script for loading/updating data from Fulcrum

library(sf)
library(tidyverse)

parcel_points_dataall <-st_read("https://web.fulcrumapp.com/shares/bab6426d5db0cae5.geojson",stringsAsFactors=FALSE) %>%
  st_set_geometry(NULL) %>%
  mutate(x=as.numeric(longitude),y=as.numeric(latitude))

parcel_points_completed<-parcel_points_dataall %>%
  filter(status=="Completed")

#Add in metadata
prop_type_dict<-read_csv("ocilla_shiny/data/prop_type_dict.csv")

parcel_points_data<-parcel_points_completed %>%
  left_join(prop_type_dict) %>%
  mutate(date=format(Sys.Date(),format="%Y_%m_%d"),
         cond_factor=factor(condition,levels=c("well maintained","sound","minor repairs needed",
                                               "moderate rehabilitation needed",
                                               "substantial rehabilitation needed","dilapidated")))
#General property info
parcel_points_data<-parcel_points_data %>%
  mutate(str1=if_else(general_prop=="occupied","Occupied",""),
         str2=if_else(general_prop=="unoccupied","Unoccupied",""),
         str3=if_else(general_prop=="for sale","For sale",""),
         str4=if_else(general_prop=="unknown","Status unknown","")) %>%
  unite(col="genprop",str1:str4,sep=" ",remove=TRUE)

#Foundation info
parcel_points_data<-parcel_points_data %>%
  mutate(found_good=if_else(foundation=="found_good","TRUE","FALSE"),
         found_partreplace=if_else(foundation=="found_partreplace","TRUE","FALSE"),
         found_compreplace=if_else(foundation=="found_compreplace","TRUE","FALSE"),
         found_cracked=if_else(foundation=="found_cracked","TRUE","FALSE"))

parcel_points_data<-parcel_points_data %>%
  mutate(str0="<br><ul>",
         str1=if_else(found_good=="TRUE","<li>Good",""),
         str2=if_else(found_partreplace=="TRUE","<li>Partial replacement needed",""),
         str3=if_else(found_compreplace=="TRUE","<li>Complete replacement needed",""),
         str4=if_else(found_cracked=="TRUE","<li>Cracked",""),
         str99="</ul>") %>%
  unite(col="found_text",str0:str99,sep=" ",remove=TRUE)

#Exterior info
parcel_points_data<-parcel_points_data %>%
  group_by(fulcrum_id)%>%
  mutate(str0="<br><ul>",
         str1=if_else("ext_good" %in% exterior,"<li>Good",""),
         str2=if_else(str_detect(exterior,"ext_repaint"),"<li>Repainting needed",""),
         str3=if_else(str_detect(exterior,"ext_cracked"),"<li>Cracked/minor dry rot",""),
         str4=if_else(str_detect(exterior,"ext_needs_replace"),"<li>Needs replacement",""),
         str5=if_else(str_detect(exterior,"ext_chimney"),"<li>Chimney needs repair",""),
         str6=if_else(str_detect(exterior,"ext_nosiding"),"<li>Missing/no siding",""),
         str7=if_else(str_detect(exterior,"ext_notvis"),"<li>Not visible",""),
         str99="</ul>") %>%
  unite(col="exterior_text",str0:str99,sep=" ",remove=TRUE)

#Windows/doors info
parcel_points_data<-parcel_points_data %>%
  group_by(fulcrum_id)%>%
  mutate(str0="<br><ul>",
         str1=if_else(str_detect(windows_doors,"window_good"),"<li>Good",""),
         str2=if_else(str_detect(windows_doors,"window_repaint"),"<li>Repainting needed",""),
         str3=if_else(str_detect(windows_doors,"window_crackedpanes"),"<li>Cracked window panes",""),
         str4=if_else(str_detect(windows_doors,"window_minreplace"),"<li>1-3 windows need replacement",""),
         str5=if_else(str_detect(windows_doors,"window_majreplace"),"<li>More than 3 windows need replacement",""),
         str99="</ul>") %>%
  unite(col="windows",str0:str99,sep=" ",remove=TRUE)

#Stairs_rails info
parcel_points_data<-parcel_points_data %>%
  group_by(fulcrum_id) %>%
  mutate(str0="<br><ul>",
         str1=if_else(str_detect(stairs_rails,"stairs_good"),"<li>Good",""),
         str2=if_else(str_detect(stairs_rails,"stairs_cracked"),"<li>Cracked/minor repairs needed",""),
         str3=if_else(str_detect(stairs_rails,"stairs_majorrepair"),"<li>Major repair needed",""),
         str4=if_else(str_detect(stairs_rails,"stairs_repaint"),"<li>Repainting needed",""),
         str99="</ul>") %>%
  unite(col="stairs_text",str0:str99,sep=" ",remove=TRUE)

#Roofing info
parcel_points_data<-parcel_points_data %>%
  group_by(fulcrum_id) %>%
  mutate(str0="<br><ul>",
         str1=if_else(str_detect(roofing,"roof_good"),"<li>Good",""),
         str2=if_else(str_detect(roofing,"roof_gutters"),"<li>Gutters need repair",""),
         str3=if_else(str_detect(roofing,"roof_shingles"),"<li>Cracked/peeling shingles",""),
         str4=if_else(str_detect(roofing,"roof_reroof_part"),"<li>Partial re-roofing needed",""),
         str5=if_else(str_detect(roofing,"roof_reroof_tot"),"<li>Total re-roofing needed",""),
         str6=if_else(str_detect(roofing,"roof_newstructure"),"<li>New roofing structure needed",""),
         str99="</ul>") %>%
  unite(col="roof_text",str0:str99,sep=" ",remove=TRUE)

#Lot assessment
parcel_points_data<-parcel_points_data %>%
  group_by(fulcrum_id) %>%
  mutate(str0="",
         str1=if_else(str_detect(lot,"lot_satis"),"<br>Satisfactory",""),
         str2=if_else(str_detect(lot,"lot_weeds"),"<br>Lawn overgrown/weeds",""),
         str3=if_else(str_detect(lot,"lot_missingcover"),"<br>Missing ground cover/grass",""),
         str4=if_else(str_detect(lot,"lot_trees"),"<br>Dead/hazardous trees",""),
         str5=if_else(str_detect(lot,"lot_inop_vehicle"),"<br>Inoperable vehicle in yard",""),
         str6=if_else(str_detect(lot,"lot_junk"),"<br>Major cleanup/junk in yard",""),
         str6=if_else(str_detect(lot,"lot_porchstorage"),"<br>Porch used as storage",""),
         str6=if_else(str_detect(lot,"lot_graffiti"),"<br>Graffiti on house/property",""),
         str99="") %>%
  unite(col="lot_text",str0:str99,sep=" ",remove=TRUE)

#Create textbox
proper=function(x) paste0(toupper(substr(x, 1, 1)), tolower(substring(x, 2)))

parcel_points_data1<-parcel_points_data %>%
  replace_na(list(photo1 = "blank", photo2 = "blank", photo3="blank",
                  photo4="blank",comments="blank")) %>%
  mutate(str01=paste("<strong>Parcel ID: </strong>",parcel_no),
         str02=paste("<br><strong>Parcel address: </strong>", prop_add),
         str03=paste("<br><strong>Property type: </strong>",prop_type_name),
         str04=paste("<br><strong>Property status: </strong>",general_prop),
         str05=paste("<br><strong>Property conditions: </strong>",proper(condition)),
         str06="<br><br><strong><u>Structural conditions</u></strong>",
         str07=paste("<br><b><i>Foundation: </b></i>",found_text),
         str08=paste("<b><i>Exterior: </b></i>",exterior_text),
         str09=paste("<b><i>Windows/doors: </b></i>",windows),
         str10=paste("<b><i>Stairs/rails: </b></i>",stairs_text),
         str11=paste("<b><i>Roof: </b></i>",roof_text),
         #str12="<strong><u>Lot assessment</u></strong>",
         str13=paste("<b><i>Lot assessment: </b></i>",lot_text),
         str14=if_else(comments=="blank","",paste("<strong><u><br><br>Other comments 
                                                 </strong></u><br>",comments)),
         str90=if_else(is.na(photo1_url)==FALSE,paste("<br><br><strong>Photos</strong><br><a href='",
                                             photo1_url,"' target='_blank'>Picture 1</a>"),""),
         str91=if_else(is.na(photo2_url)==FALSE,paste("<br><a href='",
                                             photo2_url,"' target='_blank'>Picture 2</a>"),""),
         str92=if_else(is.na(photo3_url)==FALSE,paste("<br><a href='",
                                             photo3_url,"' target='_blank'>Picture 3</a>"),""),
         str93=if_else(is.na(photo4_url)==FALSE,paste("<br><a href='",
                                             photo4_url,"' target='_blank'>Picture 4</a>"),"")) %>%
  unite(col="textbox1",str01:str05,sep="",remove=TRUE) %>%
  unite(col="textbox2",str07:str14,sep="",remove=FALSE) %>%
  unite(col="textbox3",str90:str93,sep="",remove=TRUE)

#####################
# ID duplicates and add jitter
#####################

#Identify duplicates
parcel_count<-parcel_points_data1 %>% 
  ungroup() %>%
  dplyr::count(parcel_no)

parcel_points_data2<-parcel_points_data1 %>%
  left_join(parcel_count)

#Separate duplicates and add jitter
parcel_points_data_dup<-parcel_points_data2 %>%
  filter(n>1) 

xrand<-runif(nrow(parcel_points_data_dup),-0.0002,0.0002)
yrand<-runif(nrow(parcel_points_data_dup),-0.0002,0.0002)

parcel_points_data_dup<-parcel_points_data_dup %>%
  bind_cols(data.frame(xrand),data.frame(yrand)) %>%
  mutate(x=x+xrand,
         y=y+yrand) %>%
  select(-xrand,-yrand)

parcel_points_data3<-parcel_points_data2 %>%
  filter(n==1) %>%
  bind_rows(parcel_points_data_dup)

parcel_points<-parcel_points_data3
#parcel_points<-st_as_sf(parcel_points_data3,coords=c("X","Y"),crs=4326,remove=FALSE)

#####################
# Create sortable parcel list with issues
#####################

#Spread out the collapsed variables and create a long list
parcel_points_issue<-parcel_points_data3 %>%
  select(fulcrum_id,parcel_no,prop_add,prop_type_name,genprop,condition,comments,
         x,y,
         photo1,photo2,photo3,photo4,
         foundation, exterior, windows_doors, stairs_rails,roofing,lot) %>%
  gather(foundation:lot,key="var",value="value") %>%
  separate(value,c("a","b","c","d","e"),",") %>%
  gather(a:e,key="var1",value="value") %>%
  dplyr::select(-var1) %>%
  filter(is.na(value)==FALSE)

#################
#Read in issue categories and define color scheme
################

var_alias<-read_csv("ocilla_shiny/data/var_alias.csv")

#####################
# Create issue table
#####################

parcel_issues<-parcel_points_issue %>%
  #Create dummy variable for issue
  mutate(dummy=1) %>%
  #select(-var) %>%
  pivot_wider(names_from=value,values_from=dummy,values_fill=0) %>%
  gather(found_notvis:ext_notvis,key="var",value="value") %>%
  #Create issue count and pct 
  dplyr::group_by(var) %>%
  dplyr::summarise(total=n(),
                   issue_count=sum(value),
                   issue_pct=(round(issue_count/total*100,2))) %>%
  #Add alias names
  left_join(var_alias) %>%
  filter(is.na(var_alias)==FALSE)

parcel_issues<-parcel_issues[order(-parcel_issues$issue_pct),]


#Write tables
write_csv(parcel_points_issue,"ocilla_shiny/data/parcel_points_issue.csv")
write_csv(parcel_points,"ocilla_shiny/data/parcel_points.csv")
write_csv(parcel_issues,"ocilla_shiny/data/parcel_issues.csv")
#write_csv(parcel_points_issue_long,"data/parcel_points_issue_long.csv")
