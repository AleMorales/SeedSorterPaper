
# Load libraries and settings ---------------------------------------------
library(SeedSorter)
library(dplyr)
library(caret) # required for the S3 methods of predict

# Create classification tasks combining all data from each genotype -------

# Tasks for training algorithms

trainingTasks  = vector("list", 5)
names(trainingTasks) = c("An1", "Bay0", "Bur0", "Col0", "LP26")

for(name in names(trainingTasks)) {
  A = getTrainingData(main_file = paste0(name, "_A.txt"), 
                         profile_file = paste0(name, "_A_prf.fst"),
                         datadir = "Input/Training/", clean = TRUE)
  B = getTrainingData(main_file = paste0(name, "_B.txt"), 
                         profile_file = paste0(name, "_B_prf.fst"),
                         datadir = "Input/Training/", clean = TRUE)
  C = getTrainingData(main_file = paste0(name, "_C.txt"), 
                         profile_file = paste0(name, "_C_prf.fst"),
                         main_waste_file = "waste.txt", profile_waste_file = "waste_prf.fst",
                         datadir = "Input/Training/", clean = TRUE)
  trainingTasks[[name]] = createTrainingTask(rbind(A, B, C), id = name) 
}


# Tasks for making predictions 
predFilenames = vector("list", 5)
names(predFilenames) = c("An1", "Bay0", "Bur0", "Col0", "LP26")

predFilenames[['An1']]  = c("130_2_107_01","130_2_107_02","156_2_109_01","156_2_109_02",
                            "165_2_104_01","322_3_275_01","323_3_279_01","324_3_280_01",
                            "325_3_276_01","326_3_272_01","948_3_274_01","967_3_277_01")

predFilenames[['Bay0']] = c("28_1_96_02","28_1_96_04", "125_2_132_01","151_2_139_01",
                            "157_2_138_01",
                            "367_2_131_01","903_3_253_01","952_3_257_01","xx_1_99_02")

predFilenames[['Bur0']] = c("101_1_92_01","124_2_148_01","365_2_147_01","485_3_337_01",
                            "490_3_336_01",
                            "962_3_332_01","973_3_334_01","xx_1_90_01","xx_1_93_1",
                            "xx_1-89_01")

predFilenames[['Col0']] = c("968_3_283_01","959_3_288_01","499_3_286_01","492_3_285_01",
                            "516_1_107_02","516_1_107_01","126_2_113_01","121_2_116_01",
                            "118_2_115_01")

predFilenames[['LP26']] = c("348_2_127_01","85_1_74_01","69_1_77_01")

predData = vector("list", 5)
names(predData) = c("An1", "Bay0", "Bur0", "Col0", "LP26")
prettyNames = c(An1 = "An-1", Bay0 = "Bay-0", Bur0 = "Bur-0", Col0 = "Col-0", LP26 = "Lp2-6")

for(name in names(predData)) {
  predData[[name]] = vector("list", length(predFilenames[[name]]))
}

for(name in names(predData)) {
  for(i in seq_along(predFilenames[[name]])) {
    print( paste0(predFilenames[[name]][i],"_extra_ch0_prf.fst"))
    predData[[name]][[i]] =  getPredictionData(main_file = paste0(predFilenames[[name]][i],"_extra.txt"),
                                               profile_file = paste0(predFilenames[[name]][i],"_extra_ch0_prf.fst"),
                                               datadir = paste0("Input/Application/",prettyNames[[name]],"/"))
  }
}


# Train the different algorithms on all data from each genotype -----------
models = vector("list", 5)
names(models) = c("An1", "Bay0", "Bur0", "Col0", "LP26")
algorithms = c("extinction", "lda", "qda", "knn", "logistic", "naiveBayes",
               "randomforest", "svm", "xgboost", "kmeans")
parallel = c(extinction = FALSE, lda = FALSE, qda = FALSE, knn = TRUE, logistic = TRUE,
             naiveBayes = FALSE, randomforest = TRUE, svm = TRUE, xgboost = TRUE, kmeans = FALSE)

algorithms = algorithms
for(name in names(models)) {
  print(name)
  models[[name]] = vector("list", length(algorithms))
  names(models[[name]]) = algorithms
  balance = table(getTaskData(trainingTasks[[name]])$Class)
  ratio = max(balance)/min(balance)
  for(algo in algorithms) {
    print(algo)
    models[[name]][[algo]] = tuneAlgorithm(algorithm = algo, trainingTasks[[name]], osw.rate = ratio,
                                           parallel = parallel[algo], nthreads = 64)
  }
}

saveRDS(models, file = "Intermediate/modelsCoarse.rds")

# Make predictions --------------------------------------------------------

models = readRDS(file = "Intermediate/modelsCoarse.rds")

predCoarse  = vector("list", 5)
names(predCoarse) = c("An1", "Bay0", "Bur0", "Col0", "LP26")

predCoarseStats  = vector("list", 5)
names(predCoarseStats) = c("An1", "Bay0", "Bur0", "Col0", "LP26")

for(name in names(predCoarse)) {
  for(i in seq_along(predFilenames[[name]])) {
    predData[[name]][[i]] = mutate(predData[[name]][[i]],

                                   extinction_pred = classifySeeds(models[[name]][["extinction"]],
                                                                   predData[[name]][[i]][,1:7])$data$response,

                                   lda_pred = classifySeeds(models[[name]][["lda"]],
                                                                   predData[[name]][[i]][,1:7])$data$response,

                                   qda_pred = classifySeeds(models[[name]][["qda"]],
                                                            predData[[name]][[i]][,1:7])$data$response,

                                   knn_pred = classifySeeds(models[[name]][["knn"]],
                                                            predData[[name]][[i]][,1:7])$data$response,

                                   logistic_pred = classifySeeds(models[[name]][["logistic"]],
                                                            predData[[name]][[i]][,1:7])$data$response,

                                   naiveBayes_pred = classifySeeds(models[[name]][["naiveBayes"]],
                                                                 predData[[name]][[i]][,1:7])$data$response,

                                   randomforest_pred = classifySeeds(models[[name]][["randomforest"]],
                                                                   predData[[name]][[i]][,1:7])$data$response,

                                   svm_pred = classifySeeds(models[[name]][["svm"]],
                                                                     predData[[name]][[i]][,1:7])$data$response,

                                   xgboost_pred = classifySeeds(models[[name]][["xgboost"]],
                                                            predData[[name]][[i]][,1:7])$data$response,

                                   kmeans_pred = classifySeeds(models[[name]][["kmeans"]],
                                                                predData[[name]][[i]][,1:7])$data$response,

                                   Replicate = i)
  }
  predCoarse[[name]] = do.call("rbind", predData[[name]])
  if(length(predCoarse[[name]])) {

    extinction_pred = filter(predCoarse[[name]], extinction_pred == 'S') %>% group_by(Replicate) %>%
      summarise(muSize = median(Size), sdSize = sd(Size), nSeeds = n(), method = "extinction")

    lda_pred = filter(predCoarse[[name]], lda_pred == 'S') %>% group_by(Replicate) %>%
      summarise(muSize = median(Size), sdSize = sd(Size), nSeeds = n(), method = "lda")

    qda_pred = filter(predCoarse[[name]], qda_pred == 'S') %>% group_by(Replicate) %>%
      summarise(muSize = median(Size), sdSize = sd(Size), nSeeds = n(), method = "qda")

    knn_pred = filter(predCoarse[[name]], knn_pred == 'S') %>% group_by(Replicate) %>%
      summarise(muSize = median(Size), sdSize = sd(Size), nSeeds = n(), method = "knn")

    logistic_pred = filter(predCoarse[[name]], logistic_pred == 'S') %>% group_by(Replicate) %>%
      summarise(muSize = median(Size), sdSize = sd(Size), nSeeds = n(), method = "logistic")

    naiveBayes_pred = filter(predCoarse[[name]], naiveBayes_pred == 'S') %>% group_by(Replicate) %>%
      summarise(muSize = median(Size), sdSize = sd(Size), nSeeds = n(), method = "naiveBayes")

    randomforest_pred = filter(predCoarse[[name]], randomforest_pred == 'S') %>% group_by(Replicate) %>%
      summarise(muSize = median(Size), sdSize = sd(Size), nSeeds = n(), method = "randomforest")

    svm_pred = filter(predCoarse[[name]], svm_pred == 'S') %>% group_by(Replicate) %>%
      summarise(muSize = median(Size), sdSize = sd(Size), nSeeds = n(), method = "svm")

    xgboost_pred = filter(predCoarse[[name]], xgboost_pred == 'S') %>% group_by(Replicate) %>%
      summarise(muSize = median(Size), sdSize = sd(Size), nSeeds = n(), method = "xgboost")

    kmeans_pred = filter(predCoarse[[name]], kmeans_pred == 'S') %>% group_by(Replicate) %>%
      summarise(muSize = median(Size), sdSize = sd(Size), nSeeds = n(), method = "kmeans")

  }

    predCoarseStats[[name]] = rbind(extinction_pred, lda_pred, qda_pred,
                                    knn_pred, logistic_pred, naiveBayes_pred,
                                    randomforest_pred, svm_pred,
                                    xgboost_pred, kmeans_pred)
}


saveRDS(object = predCoarseStats, file = "Intermediate/predCoarse.rds")



# Add seed weight ---------------------------------------------------------

library(readxl)
library(dplyr)

# Replicates where seed sorting is incomplete are removed

predCoarseStats = readRDS(file = "Intermediate/predCoarse.rds")

predCoarseStats[["An1"]] = mutate(predCoarseStats[["An1"]], 
                                  Experiment = c(2,2,2,2,2,3,3,3,3,3,3,3)[Replicate],
                                  ID = c(107,107,109,109,104,275,279,280,
                                         276,272,274,277)[Replicate],
                                  Rep = c(1,2,1,2,1,1,1,1,1,1,1,1)[Replicate]) %>%
  filter(!(ID %in% 107))

predCoarseStats[["Bay0"]]= mutate(predCoarseStats[["Bay0"]], 
                                  ID = c(96,96,132,139,138,131,253,257,99)[Replicate],
                                  Experiment = c(1,1,2,2,2,2,3,3,1)[Replicate],
                                  Rep = c(1,2,1,1,1,1,1,1,1)[Replicate]) %>%
  filter(!(ID %in% c(96,99)))

predCoarseStats[["Bur0"]] = mutate(predCoarseStats[["Bur0"]], 
                                   ID = c(92,148,147,337,336,332,334,90,93,89)[Replicate],
                                   Experiment = c(1,2,2,3,3,3,3,1,1,1)[Replicate],
                                   Rep = rep(1,10)[Replicate]) %>%
  filter(!(ID %in% c(93)))

predCoarseStats[["Col0"]] = mutate(predCoarseStats[["Col0"]],
                                   ID = c(283,288,286,285,107,107,113,116,115)[Replicate],
                                   Experiment = c(3,3,3,3,1,1,2,2,2)[Replicate],
                                   Rep = c(1,1,1,1,1,2,1,1,1)[Replicate])

predCoarseStats[["LP26"]] = mutate(predCoarseStats[["LP26"]],
                                   ID = c(107,74,77)[Replicate],
                                   Experiment = c(2,1,1)[Replicate],
                                   Rep = rep(1,3)[Replicate])


# List of seed samples with weights
SeedWeights = read_excel("../Data/CoarseCleaning/SeedWeights.xlsx", sheet = "RevisedSeedWeight")

# Add weights to each sample and mergen data into a single table
for(genotype in names(predCoarseStats)) {
  predCoarseStats[[genotype]] = left_join(predCoarseStats[[genotype]],
                                          dplyr::select(SeedWeights,Experiment,ID,Rep,Yield)) %>%
                                mutate(Accession = genotype)
}
predCoarseStats = do.call("rbind", predCoarseStats)

# Aggregate results from technical replicates (perhaps would be better to merge the replicates at the level of raw data)
predCoarseStatsAggr = group_by(predCoarseStats, Accession, method, Experiment, ID) %>%
  summarise(muSize_ = sum(muSize*nSeeds)/sum(nSeeds),
            nSeeds_ = sum(nSeeds),
            sdSize_ = sqrt(sum(sdSize^2*nSeeds)/sum(nSeeds)),
            Yield = sum(Yield, na.rm = TRUE)) %>%
  rename(muSize = muSize_, nSeeds = nSeeds_, sdSize = sdSize_)

pretty_acc = c(Col0 = "Col-0", An1 = "An-1", Bur0 = "Bur-0",
               LP26 = "Lp2-6", Bay0 = "Bay-0")

predCoarseStatsAggr$Accession = pretty_acc[predCoarseStatsAggr$Accession]


# Figure 1: Relative differences across algorithms in nSeeds and muSize --------
predCoarseStatsAggr = arrange(predCoarseStatsAggr, method, Accession, Experiment, ID)

calcDiff = function(x) {
  data = cbind(refPred, filter(predCoarseStatsAggr, method == x[1]))
  comp = mutate(data, diff_nSeeds = (nSeeds1 - nSeeds)/nSeeds,
            diff_muSize = (muSize1 - muSize)/muSize) %>% ungroup %>%
        dplyr::select(Accession, Experiment, ID, diff_nSeeds, diff_muSize)
  cbind(comp, Method = x)
}

methods = unique(predCoarseStatsAggr$method)
methods_compare = methods[-which(methods %in% c("kmeans"))] %>% as.list

# reference is the median of all algorithms
refPred = group_by(predCoarseStatsAggr, Accession, Experiment, ID) %>%
  summarise(nSeeds = median(nSeeds), muSize = median(muSize),
            sdSize = median(sdSize), Yield = median(Yield))

library(purrr)
diff = map_dfr(methods_compare, calcDiff) %>% as_tibble




library(cowplot)

pretty_names = c(xgboost = "Extreme Gradient Boosting",
                 svm    = "Support Vector Machine",
                 randomforest = "Random Forest",
                 knn = "K-Nearest Neighbours",
                 qda = "Quadratic Discriminant Analysis",
                 naiveBayes = "Naive Bayes",
                 lda = "Linear Discriminant Analysis",
                 logistic = "Logistic Regresion Elastic Net",
                 extinction = "Extinction threshold")
diff$Method = pretty_names[diff$Method]

# Differences among algorithms in median seed size
lower = ggplot(diff, 
       aes(x = Method, y = abs(diff_muSize*100))) + 
  geom_boxplot() + theme_bw() + coord_flip(ylim = c(0,8)) +
  ylab("Relative difference in median size (%)") + xlab("")

upper = ggplot(filter(diff, abs(diff_muSize) > 0), 
       aes(x = Method, y = abs(diff_muSize*100))) + 
  geom_point() + scale_y_log10() + coord_flip(ylim = c(8,100)) + 
  theme_bw() +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        plot.margin = unit(c(5.5, 5.5, 5.5, 0.0), "pt"))

pSize = plot_grid(lower, upper, nrow = 1, rel_widths = c(1,0.3), align = "h")

# Differences among algorithms in number of seeds
lower = ggplot(diff, aes(x = Method, y = abs(diff_nSeeds*100))) + 
  geom_boxplot() + theme_bw() + coord_flip(ylim = c(0,35)) +
  ylab("Relative difference in number of seeds (%)") + xlab("")

upper = ggplot(filter(diff, abs(diff_nSeeds) > 0), 
               aes(x = Method, y = abs(diff_nSeeds*100))) + 
  geom_point() + scale_y_log10() + coord_flip(ylim = c(35,500)) + 
  theme_bw() +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        plot.margin = unit(c(5.5, 5.5, 5.5, 0.0), "pt"))


pNSeeds = plot_grid(lower, upper, nrow = 1, rel_widths = c(1,0.3), align = "h")

fig_comparison = gridExtra::grid.arrange(pSize, pNSeeds, nrow = 2)

ggsave("Output/CoarseCleaningComparison.png", plot = fig_comparison,
       device = "png", dpi = 600, width = 18, height = 16, units = "cm")

# Figure 2: Yield vs nSeeds for one algorithm + R2 and slopes (app --------
library(robust)
library(broom)

weight_number = ggplot(refPred, aes(x = nSeeds/1000, y = Yield/1000, col = Accession, 
                    fill = Accession)) +
  geom_point() + geom_smooth(method = "lm", alpha = 0.2, se = F) +
  ylab("Sample weight (g)") + xlab("Number of seeds (x1000)") + 
  scale_color_discrete(guide = guide_legend(title = "", nrow = 2)) +
  scale_fill_discrete(guide = guide_legend(title = "", nrow = 2)) +
  theme_bw() + theme(legend.position = "top")

ggsave("Output/YieldNSeedsComparison.png", plot = weight_number,
       device = "png", dpi = 300, width = 8.5, height = 9.5, units = "cm")

regrs = group_by(refPred, Accession) %>% 
          do(regr = lm(I(Yield/1e3)~I(nSeeds/1000), data= .)) %>%
  tidy(regr, conf.int = TRUE)

slopes = filter(regrs, term == "I(nSeeds/1000)")

slopes

# Figure 3: muSize distribution for each accession for one algorit --------

musize = ggplot(refPred,
       aes(x = Accession, y = muSize, color = Accession)) + geom_boxplot() +
  ylab(expression("Median seed size ("*mu*m*")")) + 
  theme_bw() +
  theme(legend.position = "none")
  # scale_color_discrete(guide = guide_legend(title = "", nrow = 2)) +
  # theme_bw() + 

ggsave("Output/CoarseSeedsSize.png", plot = musize,
       device = "png", dpi = 300, width = 8.5, height = 8.5, units = "cm")


