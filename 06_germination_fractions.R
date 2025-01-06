library(tidyverse)
library(extrafont)
library(hrbrthemes)
library(ggtext)
library(patchwork)

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

species<-c("Alnus_glutinosa","Betula_pendula","Betula_pubescens","Pinus_sylvestris","Sorbus_aucuparia","all species")


data_path<-file.path(getwd(),"data")
save_path<- file.path(getwd(),"Outputs")

ml_paper_seeds <- read_csv(paste0(data_path,"/ml_paper_seeds_all.csv"))


dataset<-ml_paper_seeds%>%
    mutate(Germination=ifelse(grepl("^[0-9]+$", Germination),"G",Germination),
           Germination=ifelse(grepl("DURING STRAT.", Germination),"G",Germination),
           Germination=ifelse(grepl("ABNO", Germination),"G",Germination),
           Germination=ifelse(grepl("x", Germination),"G",Germination),
           Germination=toupper(Germination))%>%
    filter(grepl("G|M|V|E",Germination))%>%
  filter(Species!="all_species")

total_seeds_species<-dataset%>%
  summarise(.by = Species,total_n=n())

dataset_graph<-dataset%>%
  summarise(.by = c(Species,Germination),n=n())%>%
  left_join(.,total_seeds_species)%>%
  mutate(prop=n/total_n,
         Species=gsub("_"," ",Species),
         Species = paste0(Species,"\nn = ",total_n))


dataset_graph$Germination <- factor(dataset_graph$Germination, levels = c("G", "V", "M", "E"), ordered = TRUE )

ggplot(dataset_graph,aes(y= fct_reorder(Species,desc(Species)),x=prop,fill=fct_rev(Germination)))+
  geom_col(colour="black",size=1)+
  scale_fill_manual(values = c("#ff7f7f","#ffe302","#71a6d2","#93c572"))+
  labs(y="",x="Fraction")+
  theme(axis.text.y = element_text(face="italic"),
        plot.background = element_rect(colour = "white"))+
  guides(fill = guide_legend(reverse=T))+
  labs(fill="Status")

ggsave(path=save_path,"Figure2_germination_fractions.png",dpi = "retina",height = 4.73,width = 9.98)

### Fractions per maternal line

dataset$Tree_N <- ifelse(dataset$Species == "Betula_pendula", dataset$Tree_N - 14, dataset$Tree_N)

total_seeds_species<-dataset%>%
  summarise(.by = c(Species,Tree_N),total_n=n())

dataset_graph<-dataset%>%
  summarise(.by = c(Species,Tree_N, Germination),n=n())%>%
  left_join(.,total_seeds_species)%>%
  mutate(prop=n/total_n,
         Species=gsub("_"," ",Species))

dataset_graph$Tree_N <- as.factor(dataset_graph$Tree_N)
dataset_graph$Tree_N <- factor(dataset_graph$Tree_N, levels = rev(sort(unique(dataset_graph$Tree_N))))

ggplot(dataset_graph,aes(y=Tree_N,x=prop,fill=Germination))+
  facet_grid(Species ~ . , scale = "free_y") +
  geom_col(colour="black")+
  theme_ipsum_rc()+
  labs(y="Maternal Lines",x="Fraction")+

  theme(strip.text.y = element_text(face = "italic"),
        legend.position = "bottom")


### Fractions per model

ml_paper_pred<-read_csv("outputs/maternal_predict.csv") 

predset<-ml_paper_pred %>%
  mutate(Germination=ifelse(grepl("^[0-9]+$", Germination),"G",Germination),
         Germination=ifelse(grepl("DURING STRAT.", Germination),"G",Germination),
         Germination=ifelse(grepl("ABNO", Germination),"G",Germination),
         Germination=ifelse(grepl("x", Germination),"G",Germination),
         Germination=toupper(Germination))%>%
  filter(grepl("G|M|V|E",Germination))%>%
  #filter(Species=="all_species")%>%
  mutate(Germination=case_when(Germination=="G"~"Germinated",
                               Germination=="V"~"Viable",
                               Germination=="M"~"Mouldy",
                               Germination=="E"~"Empty"))



predset$Tree_N <- ifelse(predset$Species == "Betula_pendula", predset$Tree_N - 14, predset$Tree_N)

predset_long <- predset %>%
  group_by(Species, Tree_N, Germination) %>% 
  pivot_longer(cols = c(Bin_germ, XGB_all,XGB_colour, XGB_xray,cnn_colour_pred,cnn_xray_pred)) %>%
  ungroup()

### 

total_unsort<-predset_long%>%
  filter(name == "Bin_germ") %>%
  summarise(.by = c(Species, name),total_n=n()) %>%
  ungroup()

unsort<-predset_long%>%
  filter(name == "Bin_germ") %>%
  mutate(value = 1) %>%
  summarise(.by = c(Species,Germination,name, value), n=n())%>%
  left_join(.,total_unsort)%>%
  mutate(prop=n/total_n,
         Species=gsub("_"," ",Species))

total_seeds_species<-predset_long%>%
  filter(name != "Bin_germ") %>%
  summarise(.by = c(Species, name),total_n=n()) %>%
  ungroup()

predset_graph<-predset_long%>%
  filter(name != "Bin_germ") %>%
  summarise(.by = c(Species,Germination,name, value), n=n())%>%
  left_join(.,total_seeds_species)%>%
  mutate(prop=n/total_n,
         Species=gsub("_"," ",Species))

predset_graph <-rbind(unsort, predset_graph)

predset_graph <- predset_graph%>%
  mutate(Species=case_when(Species=="Alnus glutinosa"~"A. glutinosa",
                           Species=="Betula pubescens"~"B. pubescens",
                           Species=="Betula pendula"~"B. pendula",
                           Species=="Sorbus aucuparia"~"S. aucuparia",
                           Species=="Pinus sylvestris"~"P. sylvestris",
                           Species=="all species"~"all species"),
         value=case_when(value=="0"~"Discard",
                         value=="1"~"Accept"),
         name=case_when(name=="XGB_xray"~"**XGB** | <span style='color:#007A87;'>X-ray</span>",
                        name=="XGB_colour"~"**XGB** | <span style='color:#FFB400;'>colour</span>",
                        name=="XGB_all"~"**XGB** | <span style='color:#FF5A5F;'>all</span>",
                        name=="cnn_xray_pred"~"**CNN** | <span style='color:#007A87;'>X-ray</span>",
                        name=="cnn_colour_pred"~"**CNN** | <span style='color:#FFB400;'>colour</span>",
                        name=="Bin_germ"~"Unsorted")
  )

predset_graph$name <- factor(predset_graph$name, levels = c("**XGB** | <span style='color:#FF5A5F;'>all</span>",
                                                            "**XGB** | <span style='color:#007A87;'>X-ray</span>",
                                                            "**XGB** | <span style='color:#FFB400;'>colour</span>",
                                                            "**CNN** | <span style='color:#007A87;'>X-ray</span>",
                                                            "**CNN** | <span style='color:#FFB400;'>colour</span>",
                                                            "Unsorted"))
predset_graph$Germination<-factor(predset_graph$Germination,levels = c("Empty","Mouldy","Viable","Germinated"))

######

left_graph_set<-predset_graph%>%
  filter(value=="Accept")

left_graph_set_final_germination<-left_graph_set%>%
  group_by(Species,name)%>%
  summarise(total_specific=sum(n))

left_graph_set<-left_join(left_graph_set,left_graph_set_final_germination,by=c("Species","name"))%>%
  mutate(prop2=n/total_specific)

left_graph_set$name <- factor(left_graph_set$name, levels = c("**CNN** | <span style='color:#007A87;'>X-ray</span>",
                                                            "**CNN** | <span style='color:#FFB400;'>colour</span>",
                                                            "**XGB** | <span style='color:#FF5A5F;'>all</span>",
                                                            "**XGB** | <span style='color:#007A87;'>X-ray</span>",
                                                            "**XGB** | <span style='color:#FFB400;'>colour</span>",
                                                            "Unsorted"))

left_graph_set$Species <- factor(left_graph_set$Species, levels = c("A. glutinosa",
                                                              "B. pendula",
                                                              "B. pubescens",
                                                              "P. sylvestris",
                                                              "S. aucuparia",
                                                              "all species"))


a<-ggplot(left_graph_set,aes(y=name,x=prop, fill=Germination))+
  scale_fill_manual(values = c("#ff7f7f","#ffe302","#71a6d2","#93c572"))+
  scale_y_discrete(position = "right")+
  facet_grid(Species ~ . , scale = "free_y",switch = "y") +
  geom_col(colour="black")+
  geom_text(data=left_graph_set%>%filter(Germination=="Germinated"),aes(label=paste0(round(prop2,2)*100,"%")),x=0,hjust=-.2,colour="black",fontface="bold")+
  labs(y="",x="Fraction",fill="Status")+
  theme(strip.text.y = element_markdown(face = "italic",size = 16),
        legend.position = "bottom",
        axis.text.y = element_blank(),
        plot.margin = margin(0,0,0,0),
        panel.border = element_blank())+
  guides(fill = guide_legend(reverse=TRUE))+
  labs(subtitle = "Accept")
  a

right_graph_set<-predset_graph%>%filter(value=="Discard")%>%
  add_row(Species="S. aucuparia",Germination="Germinated",name="**XGB** | <span style='color:#007A87;'>X-ray</span>",prop=0)%>%
  add_row(Species="S. aucuparia",Germination="Germinated",name="Unsorted",prop=0)%>%
  add_row(Species="A. glutinosa",Germination="Germinated",name="Unsorted",prop=0)%>%
  add_row(Species="B. pendula",Germination="Germinated",name="Unsorted",prop=0)%>%
  add_row(Species="B. pubescens",Germination="Germinated",name="Unsorted",prop=0)%>%
  add_row(Species="P. sylvestris",Germination="Empty",name="Unsorted",prop=0)%>%
  add_row(Species="B. pendula",Germination="Germinated",name="**CNN** | <span style='color:#FFB400;'>colour</span>",prop=0)%>%
  add_row(Species="S. aucuparia",Germination="Germinated",name="**CNN** | <span style='color:#007A87;'>X-ray</span>",prop=0)%>%
  add_row(Species="S. aucuparia",Germination="Germinated",name="**CNN** | <span style='color:#FFB400;'>colour</span>",prop=0)%>%
  add_row(Species="all species", Germination="Germinated",name="Unsorted",prop=0)

right_graph_set$Germination<-factor(right_graph_set$Germination,levels = c("Empty","Mouldy","Viable","Germinated"))

right_graph_set$name <- factor(right_graph_set$name, levels = c("**CNN** | <span style='color:#007A87;'>X-ray</span>",
                                                              "**CNN** | <span style='color:#FFB400;'>colour</span>",
                                                              "**XGB** | <span style='color:#FF5A5F;'>all</span>",
                                                              "**XGB** | <span style='color:#007A87;'>X-ray</span>",
                                                              "**XGB** | <span style='color:#FFB400;'>colour</span>",
                                                              "Unsorted"))

right_graph_set$Species <- factor(right_graph_set$Species, levels = c("A. glutinosa",
                                                                 "B. pendula",
                                                                 "B. pubescens",
                                                                 "P. sylvestris",
                                                                 "S. aucuparia",
                                                                 "all species"))


b<-ggplot(right_graph_set,aes(y=name,x=prop, fill=Germination))+
  scale_fill_manual(values = c("#ff7f7f","#ffe302","#71a6d2","#93c572"))+
  scale_y_discrete(position = "left")+
  scale_x_continuous(limits = c(0,1))+
  facet_grid(Species ~ . , scale = "free_y",switch = "y") +
  geom_col(data=right_graph_set, colour="black")+
  labs(y="",x="Fraction",fill="Status")+
  theme(strip.text.y = element_blank(),
        legend.position = "bottom",
        panel.border = element_blank(),
        axis.text.y=element_markdown(hjust = 0),
        plot.background=element_rect("white",colour = NA),
        plot.margin = margin(0,10,0,0))+
  guides(fill = guide_legend(reverse=TRUE))+
  labs(subtitle = "Discard")
b
a+b+ plot_layout(guides = "collect")& theme(legend.position = "bottom")

ggsave(path=save_path,"Fig4_models_vs_germination_fractions.png",dpi = "retina",height = 13,width = 8.3)
