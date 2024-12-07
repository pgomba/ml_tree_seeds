library(tidyverse)

germination <- read_csv("data/ml_paper_seeds_all.csv")%>%
  filter(Set=="Hold out")%>%
  mutate(Germination=ifelse(grepl("^[0-9]+$", Germination),"G",Germination),
         Germination=ifelse(grepl("DURING STRAT.", Germination),"G",Germination),
         Germination=toupper(Germination))%>%
  select(Species, Tree_N, Germination, Bin_germ)

desired_order <- c("Alnus_glutinosa", "Betula_pendula", "Betula_pubescens",
                   "Pinus_sylvestris","Sorbus_aucuparia","all_species") # Specify the desired order
germination$Species <- factor(germination$Species, levels = desired_order) # Convert species to a factor with this order

# Rearrange rows by the factor order
germination <- germination[order(germination$Species), ]
  

# XGB predictions
predictions_names_all_features <- read_csv("Outputs/XGBoost/predictions_names_all_features.csv")%>%
  rename(XGB_all=Prediction)
predictions_names_colour_features <- read_csv("Outputs/XGBoost/predictions_names_colour_features.csv")%>%
  rename(XGB_colour=Prediction)
predictions_names_xray_features <- read_csv("Outputs/XGBoost/predictions_names_xray_features.csv")%>%
  rename(XGB_xray=Prediction)

# CNN predictions

species<-c("Alnus_glutinosa","Betula_pendula","Betula_pubescens","Pinus_sylvestris","Sorbus_aucuparia","all_species")
cnn_predict<-data.frame()

for (i in species) {
  
  if (i!="all_species") {
    
    cnn_colour<-read_csv(paste0("Outputs/CNNs/bin_pred_",i,"_colour.csv"))%>% 
      select(file_path, prediction)%>%
      mutate(seed_n=as.numeric(str_extract(file_path, "\\d+(?=.png)")))%>%
      arrange(seed_n)%>%
      select(prediction)%>%
      rename(cnn_colour_pred=1)
    
    
    cnn_xray<-read_csv(paste0("Outputs/CNNs/bin_pred_",i,"_xray.csv"))%>% 
      select(file_path, prediction)%>%
      mutate(seed_n=as.numeric(str_extract(file_path, "\\d+(?=.png)")))%>%
      arrange(seed_n)%>%
      select(prediction)%>%
      rename(cnn_xray_pred=1)
    
  }else{
    
    cnn_colour<-read_csv(paste0("Outputs/CNNs/bin_pred_",i,"_colour.csv"))%>% 
      select(file_path, prediction)%>%
      mutate(sp=str_extract(file_path, "(?<=\\\\)[A-Za-z_]+(?=_)"),
             seed_n=as.numeric(str_extract(file_path, "\\d+(?=.png)")))%>%
      arrange(sp,seed_n)%>%
      select(prediction)%>%
      rename(cnn_colour_pred=1)
    
    
    cnn_xray<-read_csv(paste0("Outputs/CNNs/bin_pred_",i,"_xray.csv"))%>% 
      select(file_path, prediction,real_class)%>%
      mutate(sp=str_extract(file_path, "(?<=\\\\)[A-Za-z_]+(?=_)"),
             seed_n=as.numeric(str_extract(file_path, "\\d+(?=.png)")))%>%
      arrange(sp,seed_n)%>%
      select(prediction)%>%
      rename(cnn_xray_pred=1)
    
  }
  
  
  temp_df<-cbind(cnn_colour,cnn_xray)
  temp_df$Species<-i
  
  cnn_predict<-bind_rows(cnn_predict,temp_df)%>%
    select(-Species)
}


maternal_predict<-cbind(germination,predictions_names_all_features,predictions_names_colour_features,predictions_names_xray_features,cnn_predict)



write.csv(maternal_predict,"outputs/maternal_predict.csv",row.names = F)




  