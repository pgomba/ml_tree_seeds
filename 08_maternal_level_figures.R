library(ggplot2)
library(tidyr)
library(ggfx)
library(patchwork)

df = read.csv("outputs/maternal_predict.csv") 
df$original_Tree_N <- df$Tree_N
df$Tree_N <- ifelse(df$Species == "Betula_pendula", df$Tree_N - 14, df$Tree_N)

data = read.csv("data/ml_paper_seeds_all.csv")
data$original_Tree_N <- data$Tree_N
data$Tree_N <- ifelse(data$Species == "Betula_pendula", data$Tree_N - 14, data$Tree_N)



## COLOUR TRAITS BY MATERNAL LINE FIGURE
traits = data %>%
  group_by(Tree_N, Bin_germ) %>% 
  pivot_longer(cols = c(Area, Perim., Feret, MinFeret, Circ., AR, Round, Solidity, Mean_L, Mean_a, Mean_b,Mean_grey,Mean_core_grey,
                        Contrast,Dissimilarity,Homogeneity,Energy,Correlation,ASM,)) 

mean_values <- traits %>%
  group_by(Species, name, Bin_germ) %>%
  summarize(mean_value = mean(value), .groups = 'drop')

par(mar=c(3,3,3,3))
par(mfrow=c(1,1))

plots <- list()

traits <- filter(traits, Species != "all_species") 
traits$Species <-as.factor(traits$Species)
traits$Species <- factor(traits$Species, levels = sort(levels(traits$Species)))

total_species <- length(unique(traits$Species))
species_list <- unique(traits$Species)

for (i in seq_along(species_list)) {
  
  s <- species_list[i]
  
  dp <- filter(traits, Species == s) 
  dp$Species <- gsub("_", " ", dp$Species)
  
  dp2 <- filter(mean_values, Species == s) 
  dp2$Species <- gsub("_", " ", dp2$Species)
  
 plot = ggplot() +
  facet_grid(name ~ Species, scales="free") +
  geom_jitter(data = dp, aes(x = factor(Tree_N), y = value, colour= factor(Bin_germ)), alpha = 0.3, size = 0.5) + 
  with_shadow(
    geom_hline(data = dp2, aes(yintercept = mean_value, colour = factor(Bin_germ)), linetype = "dashed", linewidth = 0.3),
    x_offset = 0, y_offset = 0, sigma = 0.05, color = "grey30"
  ) +
  labs(x = "Maternal line", y = "", colour = "Germination Outcome") +
  scale_fill_viridis_d(name = "Germination outcome", direction = -1, option = "H", begin = 0.25, end = 0.85) +
  theme_minimal(base_size = 7) +
  theme(legend.position = if (i == 4) "top" else "none",
        strip.text.y = if (i == total_species) element_text() else element_blank(), 
        strip.text.x = element_text(face = "italic"), 
        #axis.text.x = element_blank(),
        axis.title.x = if (i == 4) element_text() else element_blank()
        )
 plots[[s]] <- plot
 
}

(plot = plots[[1]] | plots[[2]] | plots[[3]] | plots[[4]] | plots[[5]])

ggsave("outputs/maternal_line_traits.pdf", plot, width = 27, height = 31, units = "cm")



## MODEL METRICS BY MATERNAL LINE FIGURE

# get number of seeds and germination percentage in the train set


germ = data %>%
  filter(Set == "train") %>% #Species == "Alnus_glutinosa" & 
  group_by(Species, Tree_N) %>% # added Species
  summarise(germination_rate = mean(Bin_germ),
            fraction_of_seeds = length(Tree_N)) %>%
  mutate(fraction_of_seeds = 100 * fraction_of_seeds/ sum(fraction_of_seeds)) %>%
  ungroup()

df_long <- df %>%
  rename(XGB_Colour=XGB_colour,XGB_Xray=XGB_xray,CNN_Colour=cnn_colour_pred,CNN_Xray=cnn_xray_pred)%>%
  pivot_longer(cols = c(XGB_all, XGB_Colour, XGB_Xray, CNN_Colour, CNN_Xray),
               names_to = "type",values_to = "pred") %>%
  mutate(germ=Bin_germ)%>% #Any germ column does the work
  
  mutate(type = factor(type, levels = c("XGB_all", "XGB_Colour", "XGB_Xray", "CNN_Colour", "CNN_Xray")))%>%
  select(Species, Tree_N,type,pred,germ)

df_long = 
df_long %>%
  mutate(
    fn = ifelse(germ == 1 & pred == 0, 1, 0),
    tn = ifelse(germ == 0 & pred == 0, 1, 0),
    tp = ifelse(germ == 1 & pred == 1, 1, 0),
    fp = ifelse(germ == 0 & pred == 1, 1, 0))


# Aggregate data by tree
df_agg <- df_long %>%
  group_by_at(vars(Species, Tree_N, type)) %>% #added Species
  summarise(
    #fraction_of_seeds = length(Tree_N), # including n of the train set
    germ = mean(germ),
    pred = mean(pred),
    fp = mean(fp),
    fn = mean(fn),
    tp = mean(tp),
    tn = mean(tn),
  ) %>%
  ungroup() %>%
  right_join(., germ, by = c("Species", "Tree_N")) %>%
  filter(!type == "all") %>% 
  mutate(
    accuracy= (tp+tn)/(tp+fn+fp+tn),
    specificity = tn/(fp+tn), 
    precision = tp/(tp+fp),
    recall = tp/(tp+fn),
    f1 = 2 * (precision * recall) / (precision + recall)
  ) 



df_meas = df_agg %>%
  pivot_longer(cols = c(germination_rate, fraction_of_seeds, accuracy, specificity, precision, recall, f1))

df_meas$name <- gsub("_", " ", df_meas$name)

df_meas <- df_meas %>%
  mutate(name = factor(name, levels = c("germination rate", "specificity", "recall", "precision" , "f1", "accuracy", "fraction of seeds")))



#df_meas$Tree_N <- factor(df_meas$Tree_N, levels = tree_order)


#   plot <- ggplot() +
#   facet_grid(Species ~ type) +
#   geom_text(data = df_meas, aes(x = factor(Tree_N), y = factor(name), label = round(value, 2)), vjust = 2.5, size = 2.5) +
#   geom_point(data = df_meas, aes(x = factor(Tree_N), y = factor(name), size = value, colour = value)) +
#   labs(x = "Tree ID", y = "") +
#   scale_colour_viridis_c(name = "Measure", direction = 1, option = "D", end = 0.9) +
#   theme_minimal()+
#   theme(legend.position = "none")


plots <- list()

total_species <- length(unique(df_meas$Species))
species_list <- unique(df_meas$Species)

for (i in seq_along(species_list)) {
  
  s <- species_list[i]
  
  dp <- filter(df_meas, Species == s) 
  
  dp$Species <- gsub("_", " ", dp$Species)

  dp$value[is.nan(dp$value)] <- NA
  
  plot <- ggplot() +
    geom_text(data = dp, aes(x = factor(Tree_N), y = factor(name), label = ifelse(name == "fraction of seeds", paste0(round(value,0),"%"), round(value, 2)), vjust = ifelse(name == "fraction of seeds", 0.5, 2.6)), size = 1.5) +
    geom_point(data = subset(dp, dp$name != "fraction of seeds"), aes(x = factor(Tree_N), y = factor(name), size = value, colour = value)) +
    scale_size(range = c(0.1, 3.3)) +
    facet_grid(Species ~ type, labeller = labeller(Species = label_value)) +
    labs(x = ifelse(i == total_species, "Maternal lines", ""), y = "") +
    scale_colour_viridis_c(name = "Measure", direction = 1, option = "D", end = 0.9) +
    theme_minimal(base_size=8) +
    theme(legend.position = "none",
          plot.background = element_rect(fill = "white", color = NA),
          panel.background = element_rect(fill = "white", color = NA),
          #legend.position = if (i == total_species) "bottom" else "none",
          strip.text.y = element_text(face = "italic", size = 9),
          strip.text.x = if (i == 1) element_text(face = "plain", size = 10) else element_blank(), 
          axis.title.x = if (i == total_species) element_text() else element_blank(),
          panel.grid.major = element_blank())
          #axis.text.x = element_blank())

  
  plots[[s]] <- plot
}

(plot = plots[[1]]/plots[[2]]/plots[[3]]/plots[[4]]/plots[[5]])

#ggsave("outputs/maternal_metrics.png", plot, width = 11.69, height = 8.27, units = "in")


## Reduced graph


df_meas = df_agg %>%
  pivot_longer(cols = c(germination_rate, fraction_of_seeds, accuracy, specificity, precision, recall, f1))

df_meas$name <- gsub("_", " ", df_meas$name)

df_meas$type <- ifelse(df_meas$name == "fraction of seeds" | df_meas$name == "germination rate", "NA", as.character(df_meas$type))

df_meas = df_meas[!duplicated(df_meas), ]

df_meas <- df_meas %>%
  mutate(name = factor(name, levels = c("specificity", "recall", "precision" , "f1", "accuracy", "fraction of seeds", "germination rate")))

df_meas$type <- gsub("NA", " ", df_meas$type)

df_meas <- df_meas %>%
  mutate(type = factor(type, levels = c(" ", "CNN_Xray", "CNN_Colour","XGB_all", "XGB_Xray",     "XGB_Colour")))

plots <- list()

total_species <- length(unique(df_meas$Species))
species_list <- unique(df_meas$Species)

for (i in seq_along(species_list)) {
  
  s <- species_list[i]
  
  dp <- filter(df_meas, Species == s)
  
  dp <- filter(dp, name == "accuracy" | name == "f1" | name == "specificity" | name == "fraction of seeds" | name =="germination rate")  
  
  dp$Species <- gsub("_", " ", dp$Species)
  
  dp$value[is.nan(dp$value)] <- NA
  
  plot <- ggplot() +
    geom_text(data = dp, aes(x = factor(Tree_N), y = factor(name), label = ifelse(name == "fraction of seeds", paste0(round(value,0),"%"), round(value, 2)), vjust = ifelse(name == "fraction of seeds", 0.5, 2.6)), size = 1.5) +
    geom_point(data = subset(dp, dp$name != "fraction of seeds"), aes(x = factor(Tree_N), y = factor(name), size = value, colour = value)) +
    scale_size(range = c(0.1, 3.3)) +
    facet_grid(type ~ Species, labeller = labeller(Species = label_value), scale = "free_y") +
    labs(x = ifelse(i == total_species, "Maternal lines", ""), y = "") +
    scale_colour_viridis_c(name = "Measure", direction = 1, option = "D", end = 0.9) +
    theme_minimal(base_size=8) +
    theme(legend.position = "none",
          plot.background = element_rect(fill = "white", color = NA),
          panel.background = element_rect(fill = "white", color = NA),
          #legend.position = if (i == total_species) "bottom" else "none",
          strip.text.x = element_text(face = "italic", size = 9),
          strip.text.y.right = if (i == total_species) element_text(face = "plain", size = 7) else element_blank(), 
          axis.text.y.left = if (i == 1) element_text() else element_blank(),
          panel.grid.major = element_blank())
  #axis.text.x = element_blank())
  
  
  plots[[s]] <- plot
}

(plot = plots[[1]]|plots[[2]]|plots[[3]]|plots[[4]]|plots[[5]])

ggsave("outputs/maternal_metrics_reduced.png", plot, width = 13, height = 4.5, units = "in")

