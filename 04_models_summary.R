library(tidyverse)
library(extrafont)
library(hrbrthemes)
library(ggtext)


theme_set(theme_ipsum_rc(base_family = "Roboto", base_size = 16,plot_title_size = 16)+
            theme(plot.background = element_rect(colour = "white", fill = "white"),
                  panel.grid.minor = element_blank(),
                  plot.title.position = "plot",
                  plot.caption = element_text(size = 13),
                  axis.title.x = element_text(size=16,hjust = 1),
                  axis.title.y = element_text(size=16,hjust = 1), 
                  plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "cm"),
                  panel.background = element_rect(colour="white"))
)


## Open and merge evaluation outputs

files_evaluation<- list.files(path = "outputs/CNNs", pattern = "evaluation",full.names = T)

cnn_evaluation_compilation<-data.frame()

for (i in files_evaluation) {
  
  type<-gsub("evaluation_","",i)%>%
    gsub(".*_","",.)%>%
    gsub(".csv","",.)
  
  species<-gsub("outputs/CNNs/evaluation_","",i)%>%
    gsub("_colour.csv","",.)%>%
    gsub("_xray.csv","",.)
  
  data<-read.csv(i)
  
  data$species<-species
  data$type<-type
  
  cnn_evaluation_compilation<-bind_rows(cnn_evaluation_compilation,data)
  
}

cnn_evaluation_compilation<-cnn_evaluation_compilation%>%
  relocate(species, type)%>%
  mutate(species=ifelse(species!="all_species",str_to_title(species),species),
         Specificity=true_negatives/(true_negatives+false_positives))%>%
  select(-loss)%>%
  rename(Accuracy=binary_accuracy,
         Precision=precision,
         AUC=auc,
         Recall=recall,
         Species=species)

cnn_evaluation_compilation$model<-"CNN"

## clean xgboost results for merge

xgb_boost_eval<-read.csv("outputs/XGBoost/evaluation_results.csv")%>%
  select(Species,model,Accuracy,Precision,AUC, Recall,f1,cm_TN_tl,cm_FP_tr,cm_FN_bl,cm_TP_br)%>%
  mutate(type=gsub("names_","",model))%>%
  mutate(type=gsub("_.*","",type),)%>%
  mutate(model="XGB")%>%
  mutate(Specificity=cm_TN_tl/(cm_TN_tl+cm_FP_tr))%>%
  rename(true_negatives=cm_TN_tl,
         true_positives=cm_TP_br,
         false_negatives=cm_FN_bl,
         false_positives=cm_FP_tr)

## Combine

all_models_eval<-bind_rows(cnn_evaluation_compilation,xgb_boost_eval)

write.csv(all_models_eval,file.path(dirname(dirname(getwd())),"outputs/05_CNN/all_models_eval.csv"),row.names = F)
          

### graph test #Complete heatmap


graph_data<-all_models_eval%>%
  select(-true_negatives,-true_positives,-false_positives,-false_negatives)%>%
  pivot_longer(cols = Accuracy:Specificity,names_to = "Parameters",values_to = "Values")%>%
  mutate(Species=gsub("_"," ",Species))%>%
  mutate(type = paste("<span style = 'color: ",
                         ifelse(type=="all", "#FF5A5F",
                                ifelse(type=="colour", "#FFB400", "#007A87")),
                         ";'>",
                         type,
                         "</span>", sep = ""))%>%
  mutate(Species=ifelse(Species!="all species",paste0("<i>",Species),Species))

ggplot(graph_data,aes(y=fct_rev(Parameters),x=Species,fill=Values))+
  geom_tile(colour="white",alpha=.6)+
  geom_text(aes(label=round(Values,2)))+
  scale_fill_viridis_c()+
  coord_fixed()+
  facet_grid(fct_rev(model)~fct_inorder(type),switch = "y")+
  theme_ipsum_rc(axis_text_size = 14,axis_title_size = 14)+
  theme(axis.text.x = element_markdown(hjust=1,angle=45),
        plot.background = element_rect(colour="white"),
        strip.text.x = element_markdown(face = "bold"),
        strip.text.y = element_text(hjust=.5),
        legend.position = "inside",
        legend.position.inside = c(.85,.3),
        legend.direction = "horizontal",
        legend.title.position = "top",
        panel.grid.major = element_line(colour="white"),
        axis.ticks.x = element_line(colour="black"))+
  labs(y="Evaluation parameters")




## reduced heatmap

rh<-all_models_eval%>%
  select(Species, model,type, Accuracy,f1,Specificity)%>%
  mutate(new_model=paste(model,type))%>%
  select(-model,-type)%>%
  pivot_longer(cols = Accuracy:Specificity)%>%
  mutate(Species=case_when(Species=="Alnus_glutinosa"~"A. glutinosa",
                           Species=="Betula_pendula"~"B. pendula",
                           Species=="Betula_pubescens"~"B. pubescens",
                           Species=="Pinus_sylvestris"~"P. sylvestris",
                           Species=="Sorbus_aucuparia"~"S. aucuparia",
                           Species=="all_species"~"all species"))%>%
  mutate(Species=ifelse(Species!="all species",paste0("<i>",Species),Species))%>%
  mutate(new_model=case_when(new_model=="XGB xray"~"**XGB** | <span style='color:#007A87;'>X-ray</span>",
                             new_model=="XGB colour"~"**XGB** | <span style='color:#FFB400;'>colour</span>",
                             new_model=="XGB all"~"**XGB** | <span style='color:#FF5A5F;'>all</span>",
                             new_model=="CNN xray"~"**CNN** | <span style='color:#007A87;'>X-ray</span>",
                             new_model=="CNN colour"~"**CNN** | <span style='color:#FFB400;'>colour</span>"))%>%
  mutate(new_model=as.factor(new_model))

rh$new_model <- factor(rh$new_model, levels = c("**XGB** | <span style='color:#FFB400;'>colour</span>",
                                                "**XGB** | <span style='color:#007A87;'>X-ray</span>",
                                                "**XGB** | <span style='color:#FF5A5F;'>all</span>",
                                                "**CNN** | <span style='color:#FFB400;'>colour</span>",
                                                "**CNN** | <span style='color:#007A87;'>X-ray</span>"))

ggplot(rh,aes(x=Species,y=fct_rev(new_model),fill=value))+
  geom_tile(colour="white",alpha=.7)+
  geom_text(aes(label=round(value,2)),size=4)+
  scale_fill_viridis_c(limits = c(0, 1), breaks = c(0, .5, 1), option = "D")+
  scale_x_discrete(position = "top") +
  facet_wrap(~name,strip.position = "bottom")+
  coord_fixed()+
  theme(axis.text.x.top   = element_markdown(size=16,angle = 45,hjust = 0,vjust = 0),
        legend.title = element_text(vjust=1),
        legend.position = "right",
        plot.background = element_rect(colour="white"),
        axis.text.y=element_markdown(size=14),
        strip.text.x = element_text(size=16))+
  labs(y="",fill="",x="")
  

ggsave("outputs/Fig3_summary_eval_models_simpl.png",,dpi="retina",width = 10,height = 3.8)





















  
