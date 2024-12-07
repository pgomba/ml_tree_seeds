library(tidyverse)
library(gt)
library(gtExtras)

dir.create(file.path(getwd(),"Outputs"))
save_path<-file.path(getwd(),"Outputs")
data_path<-file.path(getwd(),"data")

########################
## 01. Summary tables ##
########################


# Summary of used manual X-Ray parameters. ---------------------

Xray_parameters<-data.frame(Species=c("Alnus glutinosa", "Betula pendula","Betula pubescens","Pinus sylvestris","Sorbus aucuparia"),
                            Exposure=c("5.8 sec. at 23Kv","6.6 sec. at 20Kv","6.6 sec. at 20Kv","6.2 sec. at 26Kv","5.8 sec. at 23Kv"),
                            `Contrast thresholds`=c("4506 to 6295","4151 to 3233","4151 to 3233","5951 to 9657","4506 to 6295"), check.names = FALSE)%>%
  gt()%>%
  tab_header(title = "Table S1a. Summary of used manual X-Ray parameters.",
             subtitle = "Faxitron Specs and software version here")%>%
  opt_align_table_header(align = "center")%>%
  cols_align(
    align = c("center"),
    columns = everything()
  )%>%
  tab_footnote(
    "All images were originally saved in .tiff format",
    locations = NULL,
    placement = "auto")

gtsave(Xray_parameters,path = save_path,"T_S1a_X-Ray parameters.html")

# Summary of manual parameters used in standard photography. ---------------------

camera_parameters<-data.frame(Species=c("Alnus glutinosa", "Betula pendula","Betula pubescens","Pinus sylvestris","Sorbus aucuparia"),
                            Settings=c("1/50, f9, ISO200","1/50, f9, ISO200","1/50, f9, ISO200","1/50, f9, ISO250","1/50, f9, ISO200"),
                            `Copystand base light intensity`=c("3.5","3.5","3.5","Off","3.5"),
                            check.names = FALSE)%>%
  gt()%>%
  tab_header(title = "Table S1b. Summary of manual parameters used in standard photography.",
             subtitle = "Constant paremeters -> Distance from camera to grid: 78 cm , Copystand top light: On")%>%
  opt_align_table_header(align = "center")%>%
  cols_align(
    align = c("center"),
    columns = everything())%>%
  tab_footnote(
      "Prior to image analysis, original NEF photographs were converted to .tiff",
      locations = NULL,
      placement = "auto")

gtsave(camera_parameters,path = save_path,"T_S1b_camera_parameters.html")



# # Table S2. Summary of used germination conditions ----------------------


germination_conditions<-data.frame(Species=c("Alnus glutinosa", "Betula pendula","Betula pubescens","Pinus sylvestris","Sorbus aucuparia"),
                                    Stratification=c("5˚C – 41 days","5˚C – 39 days","5˚C – 41 days","No","No"),
                                   `Germination Temp.`=c("25˚C","25˚C","25˚C","20˚C","5˚C"),
                                   check.names = FALSE)%>%
  gt()%>%
  tab_header(title = "Table S2. Summary of germination conditions")%>%
  opt_align_table_header(align = "center")%>%
  cols_align(
    align = c("center"),
    columns = everything())%>%

  tab_footnote(
      footnote = "Sources: Davies et al., 2020 and MSB’s Seed Bank Database")%>%
  tab_style(
    style = list(cell_text(style = "italic")
    ),
    locations = cells_body(columns = Species,
      
    )
  )
germination_conditions

gtsave(germination_conditions,path = save_path,"Table_S2_Summary_germination_conditions.html")

# # Table S3. Summary of used XGB parameters ----------------------


cnn_parameters<-data.frame(Hyperparameters=c("learning_rate","n_estimators","subsample","min_child_weight","gamma",
                                             "colsample_bytree","max_depth"),
                                   Values=c("0.01, 0.05, 0.1, 0.3","100, 500, 1000","0.3, 0.5, 0.9 ,1","1, 5, 9, 11",
                                            "0, 1, 4", "0.6, 0.8, 1", "0.6, 0.8, 1"))%>%
  gt()%>%
  tab_header(title = "Table S3. XGBoost grid search hyperparameters used in the internal loop of the nested cross validation and the final model")%>%
  opt_align_table_header(align = "center")%>%
  cols_align(
    align = c("center"),
    columns = everything())

gtsave(cnn_parameters,path = save_path,"T_S3_XGBoost_gs_parameters.html")

# Summary seed provenance -------------------------------------------------

ml_paper_seeds <- read_csv(paste0(data_path,"/ml_paper_seeds_all.csv"))

summary_table_p1<-ml_paper_seeds%>%
  group_by(Species)%>%
  summarise(n_seeds=n())

summary_table_p2<-ml_paper_seeds%>%
  distinct(Species,Tree_N)%>%
  group_by(Species)%>%
  summarise(n_trees=n())

species<-c("Alnus_glutinosa","Betula_pendula","Betula_pubescens","Pinus_sylvestris","Sorbus_aucuparia")

for (i in species) {
  
  tree_vector<-ml_paper_seeds%>%
    filter(Species==i)%>%
    select(Tree_N)%>%
    .[[1]]
  
  tree_vector<-(tree_vector-min(tree_vector)-1) #to start always on tree 1
  
  assign(paste0("tree_vector_", i), tree_vector)
  
}

summary_table<-left_join(summary_table_p1,summary_table_p2)%>%
  mutate(Species=gsub("_"," ",Species))%>%
  rename(`Number of Seeds`=n_seeds,
         `Number of Trees`=n_trees)%>%
  filter(Species!="all species")

tree_proportion<-list(tree_vector_Alnus_glutinosa,tree_vector_Betula_pendula,tree_vector_Betula_pubescens,tree_vector_Pinus_sylvestris,tree_vector_Sorbus_aucuparia)

summary_table$`Seed contribution by maternal line`<-tree_proportion

summary_table$Provenance <-c("Bedfordshire","Suffolk","Hampshire","Highlands","South Yorkshire")

summary_table<-summary_table%>%
  rename(`Seed N.`=`Number of Seeds`,
         `Maternal lines`=`Number of Trees`)%>%
  relocate(Species, Provenance,`Seed N.`,`Maternal lines`)

table1<-summary_table %>%
  gt() %>%
  cols_align(
    align = c("center"),
    columns = everything()
  )%>%
  tab_style(
    style = list(cell_text(style = "italic")
    ),
    locations = cells_body(columns = Species,
                           
    ))%>%
  gt_plt_dist(`Seed contribution by maternal line`, type = "histogram",bw = 1,fill_color="darkgreen")
table1

gtsave(table1,path = save_path,"Table1_Collections_info.html")
