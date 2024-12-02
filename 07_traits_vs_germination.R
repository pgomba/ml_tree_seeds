library(tidyverse)
library(ggh4x)
library(extrafont)
library(hrbrthemes)

data_path<-file.path(getwd(),"data")
save_path<- file.path(getwd(),"Outputs")

###########################################
# 02. Pairwise comparison &               #
# Morphological traits vs germination     #
###########################################

##Load data

ml_paper_seeds <- read_csv(paste0(data_path,"/ml_paper_seeds_all.csv"))

# Vector with species for looping purposes

species<-c("Alnus_glutinosa","Betula_pendula","Betula_pubescens","Pinus_sylvestris","Sorbus_aucuparia")

# Vector with traits to be tested for normality

traits<-c("Mean_grey","Area","Perim.","Feret","MinFeret", "Circ.","AR","Round","Solidity", "Mean_L","Mean_a", "Mean_b", "Mean_grey","Mean_core_grey",
          "Contrast","Dissimilarity","Homogeneity","Energy","Correlation","ASM")

# Normality check
normality_table<-data.frame()

for (i in species) {
  
  data<-ml_paper_seeds%>%
    filter(Species==i)
  
  germ<-data%>%
    filter(Bin_germ==1)
  no_germ<-data%>%
    filter(Bin_germ==0)
  
  for (j in traits) {
    
    germ2<-shapiro.test(germ[[j]])
    no_germ2<-shapiro.test(no_germ[[j]])
    
    temp_df<-data.frame(species=i,trait=j,germination=germ2$p.value,no_germination=no_germ2$p.value)
    
    normality_table<-bind_rows(normality_table,temp_df)
    
  }
  
}

normality_table<-normality_table%>%
  mutate(germin=ifelse(germination<0.05,"not-normal","normal"),
         no_germin=ifelse(no_germination<0.05,"not-normal","normal"))%>%
  mutate(test_to_perform=ifelse(germin=="not-normal","Mann-Whitney","t-test"),
         test_to_perform=ifelse(no_germin=="not-normal","Mann-Whitney",test_to_perform))


####################################################
## Pairwise comparison                            ##
####################################################


pairwise_comparison<-data.frame()


for (j in species) {
  
  for (i in traits) {
 
    data_selection<-ml_paper_seeds%>%
      filter(Species==j)%>%
      select(all_of(i),Bin_germ)
    
    
    germinated<-data_selection%>%
      filter(Bin_germ==1)%>%
      .[[1]]
    
    no_germinated<-data_selection%>%
      filter(Bin_germ==0)%>%
      .[[1]]

    test<-wilcox.test(germinated,no_germinated,p.adjust.method = "holm",paired = F,alternative = "two.sided",conf.level = .99)
    
    p_value<-test$p.value
    
    temp_df<-data.frame(Species=j,Trait=i,p_value)
    pairwise_comparison<-bind_rows(pairwise_comparison,temp_df)
    
    
  }
}

## Add label "*" when significant (p<0.05) difference
pairwise_comparison<-pairwise_comparison%>%
  mutate(label=ifelse(p_value<0.05,"*",""),
         Species=gsub("_"," ",Species))


####################################################
## Build Pairwise Figure                          ##
####################################################


traits_for_graph<-ml_paper_seeds%>%
  filter(Species!="all_species")%>%
  select(Species,Bin_germ,8:26)%>%
  pivot_longer(cols=3:21,names_to = "Variable",values_to = "Value")%>%
  mutate(Species=gsub("_"," ",Species))

traits_for_graph2<-traits_for_graph%>%
  group_by(Species,Variable)%>%
  summarise(garg=mean(Value),
            garg2=max(Value))

traits_for_graph<-left_join(traits_for_graph,pairwise_comparison,by=c("Species"="Species","Variable"="Trait"))%>%
  left_join(.,traits_for_graph2,by=c("Species"="Species","Variable"="Variable"))



ggplot(traits_for_graph,aes(x=Value,fill=as.factor(Bin_germ)))+
  geom_density(colour="white",linewidth=.5,alpha=.6)+
  geom_text(aes(label=label,x=garg,y=0),size=10)+
  scale_fill_manual(values=c("#D41159","#1A85FF"))+
  ggh4x::facet_grid2(Variable~Species, scales = "free", independent = "all",switch = "y")+
  theme_ipsum_rc()+
  theme(plot.background = element_rect(colour="white"),
        axis.text.x = element_blank(),
        axis.text.y=element_blank(),
        legend.position = "bottom",
        strip.text.x = element_text(size=20,face="italic"),
        strip.text.y=element_text(size=16),
        axis.title.y = element_text(size=20),
  )+
  labs(fill="Germination",y="Density distribution",x="")

ggsave(path=save_path,"FigS1A_traits_vs_germination.png",dpi = "retina",height = 33.1,width = 23.4) #A1






