library(tidyverse)
library(tidyr)
library(ggfx)
library(patchwork)
options(scipen = 999)

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
  labs(x = "Mother tree", y = "", colour = "Germination Outcome") +
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

ggsave("outputs/mother_tree_traits.pdf", plot, width = 27, height = 31, units = "cm")



## MODEL METRICS BY MOTHER TREE FIGURE

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
  mutate(Seed_N = row_number()) %>%
  pivot_longer(cols = c(XGB_all, XGB_Colour, XGB_Xray, CNN_Colour, CNN_Xray),
               names_to = "type",values_to = "pred") %>%
  mutate(germ=Bin_germ,
         type = factor(type, levels = c("XGB_all", "XGB_Colour", "XGB_Xray", "CNN_Colour", "CNN_Xray")))%>%
  select(Species, Seed_N, Tree_N,type,pred,germ)

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
    accuracy = (tp+tn)/(tp+fn+fp+tn),
    specificity = tn/(fp+tn), 
    precision = tp/(tp+fp),
    recall = tp/(tp+fn),
    f1 = 2 * (precision * recall) / (precision + recall)
  ) 


# ============================================================

# Correlation between training representation and per-tree performance

# ============================================================

df_cor <- df_agg %>%
  filter(!is.na(f1), !is.nan(f1), Species != "all_species", ) %>%
  group_by(Species, type) %>%
  summarise(
    f1_r_spearman = cor(fraction_of_seeds, f1, method = "spearman", use = "complete.obs"),
    f1_p_spearman = cor.test(fraction_of_seeds, f1, method = "spearman")$p.value,
    f1_r_pearson  = cor(fraction_of_seeds, f1, method = "pearson",  use = "complete.obs"),
    f1_p_pearson  = cor.test(fraction_of_seeds, f1, method = "pearson")$p.value,
    
    accuracy_r_spearman = cor(fraction_of_seeds, accuracy, method = "spearman", use = "complete.obs"),
    accuracy_p_spearman = cor.test(fraction_of_seeds, accuracy, method = "spearman")$p.value,
    accuracy_r_pearson  = cor(fraction_of_seeds, accuracy, method = "pearson",  use = "complete.obs"),
    accuracy_p_pearson  = cor.test(fraction_of_seeds, accuracy, method = "pearson")$p.value,
    
    specificity_r_spearman = cor(fraction_of_seeds, specificity, method = "spearman", use = "complete.obs"),
    specificity_p_spearman = cor.test(fraction_of_seeds, specificity, method = "spearman")$p.value,
    specificity_r_pearson  = cor(fraction_of_seeds, specificity, method = "pearson",  use = "complete.obs"),
    specificity_p_pearson  = cor.test(fraction_of_seeds, specificity, method = "pearson")$p.value,
    
    n = n(),
    .groups = "drop"
  )

write.csv(df_cor, "Outputs/Correlation_training-representation_vs_tree_performance.csv", row.names = FALSE)

# ============================================================

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
    labs(x = ifelse(i == total_species, "Mother tree", ""), y = "") +
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

#ggsave("outputs/mother_tree_metrics.png", plot, width = 11.69, height = 8.27, units = "in")


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
    labs(x = ifelse(i == total_species, "Mother tree", ""), y = "") +
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

ggsave("outputs/mother_tree_metrics_reduced.png", plot, width = 13, height = 4.5, units = "in")



# ============================================================

# GLMM ANALYSIS

# ============================================================

library(lme4)
library(performance)   
library(DHARMa)       
library(broom.mixed) 
library(rlang)
library(tidytext)

# ============================================================

# OBJECTIVE 1: Between-tree variance in germination probability

# ============================================================

names(data)
df_seeds <- data %>%
  filter(Species != "all_species") %>%
  mutate(Tree_ID = as.factor(paste0(Species, "_", Tree_N)),
         Species = as.factor(Species)
  ) %>%
  rename(germ = Bin_germ) %>%
  select(Species, Tree_ID, germ)

# Random intercept GLMM: tree nested within species

# Species as a fixed effect and maternal-tree identity as a random intercept

m_germ <- glmer(
  germ ~ Species + (1 | Tree_ID),
  data   = df_seeds,
  family = binomial,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

summary(m_germ)

VarCorr(m_germ)   # raw variance components

# ICC = proportion of latent variance attributable to tree identity
icc(m_germ) # Adjusted ICC: Proportion of variance attributable to the 
# random effects after accounting for the fixed effects in the model.

(r2m <- r2(m_germ)) # marginal R2 (fixed) and conditional R2 (fixed + random)
round(r2m$R2_conditional - r2m$R2_marginal, 2) # random effects contribution on top of species alone

# Does tree identity significantly explain germination variance?
m_germ_notree <- glm(germ ~ Species, data = df_seeds, family = binomial)

anova(m_germ, m_germ_notree, test = "Chisq")   # note: boundary test, p-val conservative

# DHARMa diagnostics
sim_germ <- simulateResiduals(m_germ, plot = TRUE)
testDispersion(sim_germ)

# Save output
lrt      <- anova(m_germ, m_germ_notree, test = "Chisq")
vc       <- as.data.frame(VarCorr(m_germ))
icc_vals <- icc(m_germ)
r2_vals  <- r2(m_germ)
sim      <- simulateResiduals(m_germ, plot = FALSE)
disp     <- testDispersion(sim, plot = FALSE)

germ_results <- tibble(
  n_obs             = nrow(df_seeds),
  n_trees           = length(unique(df_seeds$Tree_ID)),
  var_tree          = round(vc$vcov[1], 4),
  sd_tree           = round(vc$sdcor[1], 4),
  icc_adjusted      = round(icc_vals$ICC_adjusted,   3),
  icc_unadjusted    = round(icc_vals$ICC_unadjusted, 3),
  R2_marginal       = round(r2_vals$R2_marginal,     3),
  R2_conditional    = round(r2_vals$R2_conditional,  3),
  R2_mother_tree = round(r2_vals$R2_conditional - r2_vals$R2_marginal, 3),
  LRT_chi2          = round(lrt$Chisq[2], 3),
  LRT_p             = signif(lrt$`Pr(>Chisq)`[2], 3),
  DHARMa_disp       = round(disp$statistic, 3),
  DHARMa_p          = round(disp$p.value, 3)
)

write.csv(germ_results, "Outputs/GLMM_germination_mother_tree.csv", row.names = F)

# ============================================================

# OBJECTIVE 2: Does classifier performance depend on mother tree?

# ============================================================

# Add correct/incorrect outcome; use full df_long (each row = seed × classifier)
df_perf_all_types <- df_long %>%
  filter(Species != "all_species", 
         Species != "Sorbus_aucuparia" # Excluded from analysis because too few 
         # non-germinating seeds were available, resulting in insufficient 
         # training data for the non-viable class and 0% specificity across all 
         # classifiers.
         ) %>% 
  mutate(
    Tree_ID = as.factor(paste0(Species, "_", Tree_N)),
    Species = as.factor(Species),
    correct = as.integer(germ == pred),
    germ = factor(germ, levels = c(0, 1))
  )


# Quantify whether performance varied among maternal trees for each classifier 
# independently. Within each classifier-specific model, each seed contributed 
# only one observation, so no seed-level repeated-measures term was required.

classifier_types <- levels(df_perf_all_types$type)

random_intercept_df <- data.frame()

results_list <- list()

for (ct in classifier_types) {
  df_perf <- df_perf_all_types %>%
    filter(type == ct)
  
  m_A <- glm(correct ~ Species + germ, data = df_perf, family = binomial)
  m_B <- glmer(
    correct ~ Species + germ + (1 | Tree_ID),
    data    = df_perf,
    family  = binomial,
    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
  )
  
  if (is.null(m_B)) next
  
  lrt      <- anova(m_B, m_A, test = "Chisq")
  vc       <- as.data.frame(VarCorr(m_B))
  icc_vals <- icc(m_B)
  r2_vals  <- r2(m_B)
  sim      <- simulateResiduals(m_B, plot = FALSE)
  disp     <- testDispersion(sim, plot = FALSE)
  
  results_list[[ct]] <- tibble(
    classifier        = ct,
    n_obs             = nrow(df_perf),
    n_trees           = length(unique(df_perf$Tree_ID)),
    var_tree          = round(vc$vcov[1], 4),
    sd_tree           = round(vc$sdcor[1], 4),
    icc_adjusted      = round(icc_vals$ICC_adjusted,   3),
    icc_unadjusted    = round(icc_vals$ICC_unadjusted, 3),
    R2_marginal       = round(r2_vals$R2_marginal,     3),
    R2_conditional    = round(r2_vals$R2_conditional,  3),
    R2_mother_tree = round(r2_vals$R2_conditional - r2_vals$R2_marginal, 3),
    LRT_chi2          = round(lrt$Chisq[2], 3),
    LRT_p             = signif(lrt$`Pr(>Chisq)`[2], 3),
    DHARMa_disp       = round(disp$statistic, 3),
    DHARMa_p          = round(disp$p.value, 3)
  )
  
  re_df <- ranef(m_B, condVar = TRUE)$Tree_ID %>%
    as_tibble(rownames = "Tree_ID") %>%
    rename(intercept = '(Intercept)') %>%
    mutate(
      se = sqrt(attr(ranef(m_B, condVar = TRUE)$Tree_ID, "postVar")[1, 1, ]),
      lower = intercept - 1.96 * se,
      upper = intercept + 1.96 * se,
      Species = sub("_[0-9]+$", "", Tree_ID),
      Species = sub("_", " ", Species),
      type = ct,
      Tree_ID_ord = reorder_within(Tree_ID, intercept, type)
    ) %>%
    arrange(intercept)
  
  random_intercept_df <- rbind(random_intercept_df, re_df)
  
  cat("Done:", ct, "\n")
}
results_table <- bind_rows(results_list)
print(results_table)
write.csv(results_table, "Outputs/GLMM_model_performance_mother_tree.csv", row.names = F)


p1 = ggplot(random_intercept_df, 
       aes(x = Tree_ID_ord, 
           y = intercept, colour = Species)) +
  facet_wrap(vars(type), nrow = 5, scales = "free") +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_pointrange(aes(ymin = lower, ymax = upper), size = 0.3) +
  labs(x = "Mother tree", y = "Random intercept (log-odds of correct classification)",
       title = "Tree-level deviations in model performance") +
  scale_x_reordered()+
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_blank(),
        legend.position = "right")
p1
ggsave("outputs/GLMM_Tree-level_deviations_model_performance.png", p1, width = 8, height = 8, units = "in")

(random_intercept_df[,-8])
random_intercept_df %>%
  filter(upper < 0 | lower > 0) %>%
  select(-Tree_ID_ord)

