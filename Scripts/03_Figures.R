
# Load libraries and settings ---------------------------------------------
library(SeedSorter)


# Load benchmarks across tasks and within tasks----------------------------

benchmarks = lapply(list("extinction", "knn", "lda", "logistic", "naiveBayes",
                         "qda", "randomforest", "svm", "xgboost"), 
                    function(x) 
                           readRDS(paste0("Intermediate/BenchmarkAllFiles_",x,".rds")))

benchmark = mergeComparisons(benchmarks)
rm(benchmarks)
saveRDS(benchmark, file = "Intermediate/BenchmarkAllFiles.rds")


benchmarks = lapply(list("extinction", "knn", "lda", "logistic", "naiveBayes",
                         "qda", "randomforest", "svm", "xgboost","kmeans"), 
                    function(x) 
                           readRDS(paste0("Intermediate/BenchmarkAllFilesWithin_",x,".rds")))

benchmarks = lapply(as.list(1:15), function(x) lapply(benchmarks, function(y) y[[x]]))
benchmarks_within = lapply(benchmarks, mergeComparisons)

rm(benchmarks)
saveRDS(benchmarks_within, file = "Intermediate/BenchmarkAllFilesWithin.rds")


# Plot results across tasks -----------------------------------------------

benchmark = readRDS(file = "Intermediate/BenchmarkAllFiles.rds")

# Calculate performances
library(dplyr)
library(ggplot2)
perf = mlr::getBMRPerformances(benchmark, as.df = TRUE)  %>%
  dplyr::mutate(learner.id = as.character(learner.id),
                method = SeedSorter:::pretty_names[learner.id],
                method = ifelse(is.na(method), "Ensemble", method))

indices = generateIndices(15)

perf = mutate(perf,
              trainGenotype = rep(rep(c("An1", "Bay0", "Bur0", "Col0", "LP26"), each = 3)[indices[,1]], 9),
              testGenotype = rep(rep(c("An1", "Bay0", "Bur0", "Col0", "LP26"), each = 3)[indices[,2]], 9),
              trainReplicate = rep(rep(c("A", "B", "C"), times = 5)[indices[,1]], 9),
              testReplicate = rep(rep(c("A", "B", "C"), times = 5)[indices[,2]], 9))

# Calculate median performance at intergenotype sorting
meanInterGenotype = filter(perf, trainGenotype != testGenotype) %>%
  group_by(method) %>%
  summarise(muber = median(ber), 
            iqrber = diff(quantile(ber, c(0.25,0.75))),
            maxber = max(ber),
            sdber = sd(ber),
            Q1 = quantile(ber, 0.25),
            Q3 = quantile(ber, 0.75)) %>%
  arrange(muber)

# Calculate median performance at intragenotype sorting
meanIntraGenotype = filter(perf, trainGenotype == testGenotype) %>%
  group_by(method) %>%
  summarise(muber = median(ber), 
            iqrber = diff(quantile(ber, c(0.25,0.75))),
            maxber = max(ber),
            sdber = sd(ber),
            Q1 = quantile(ber, 0.25),
            Q3 = quantile(ber, 0.75)) %>%
  arrange(muber)

# Sort methods according to median InterGenotype performance
perf$method = factor(perf$method, levels = meanInterGenotype$method)

# Calculate median for each combination of genotypes
perf = group_by(perf, trainGenotype, testGenotype) %>%
  mutate(med = median(ber*100))


# Calculate performance table for every within task benchmark
benchmarks_within = readRDS(file = "Intermediate/BenchmarkAllFilesWithin.rds")
perf_within = lapply(benchmarks_within, function(x) {
                    mlr::getBMRPerformances(x, as.df = TRUE)  %>%
                        dplyr::mutate(learner.id = as.character(learner.id),
                                      method = SeedSorter:::pretty_names[learner.id]) %>%
    select(task.id, method, ber) %>% rename(task = task.id)
  })

perf_within = do.call("rbind", perf_within)


# Average performance within task
meanIntraTask = group_by(perf_within, method) %>%
  summarise(muber = median(ber), 
            iqrber = diff(quantile(ber, c(0.25,0.75))),
            maxber = max(ber),
            sdber = sd(ber),
            Q1 = quantile(ber, 0.25),
            Q3 = quantile(ber, 0.75)) %>%
  arrange(muber)

# Median performance at intragenotype and intergenotype sortinh
meanPerformance = rbind(meanInterGenotype, meanIntraGenotype, meanIntraTask) %>%
  mutate(Type = rep(c("InterGenotype", "IntraGenotype", "IntraPlant"), times = c(9, 9, 10)))

meanPerformance = group_by(meanPerformance, Type) %>%
  mutate(mumuber = median(muber),
         musdber = median(sdber))


#####################
# Plot across tasks #
#####################

perf_medians = group_by(perf, trainGenotype, testGenotype, method) %>%
                      summarise(muber = median(ber)*100,
                                Q1 = quantile(ber, 0.25)*100,
                                Q3 = quantile(ber, 0.75)*100,
                                maxber = max(ber)) %>%
                      group_by(trainGenotype, testGenotype) %>%
                      mutate(med = median(muber))

plotPerformanceTasks = ggplot(data = perf_medians, aes(x = method, y = muber)) +
  geom_point(size = 1) +
  geom_hline(aes(yintercept = med, group = paste0(trainGenotype,testGenotype)),
             col = "red") +
  stat_summary(fun.y=function(y) y, geom="line", aes(group = 1)) + 
  geom_errorbar(aes(ymax = Q3 , ymin = Q1), width = 0.5) +
  coord_flip() +
  xlab("") +
  ylab('Balanced error rate (%)') +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  facet_grid(trainGenotype~testGenotype) +
  scale_y_continuous(trans='log10')


ggsave(filename = "Output/PerformanceComparisonTasks.png", plot = plotPerformanceTasks,
       device = "png", dpi = 300, width = 17, height = 20, units = "cm")



# Compare performances in different types of tasks ------------------------

meanPerformance$method = factor(meanPerformance$method, 
                                levels = c("K-means clustering",meanInterGenotype$method))
  
plotPerformance = ggplot(meanPerformance, aes(x = method, y = muber*100, fill = Type,
                            col = Type)) + 
  geom_point(position = position_dodge(width = 0.9)) +
  geom_point(aes(y = maxber*100), position = position_dodge(width = 0.9), 
             shape = "cross",show.legend = FALSE) +
  geom_errorbar(aes(ymax = Q3*100 , ymin = Q1*100),
                position = position_dodge(width = 0.9), width = 0.5) +
  geom_hline(aes(yintercept = mumuber*100, col = Type), lty = 2) +
  scale_y_continuous(trans='log10') +
  ylab('Balanced error rate (%)') +
  xlab("") +
  theme_bw() +
  labs(fill = "", col = "") + 
  theme(legend.position = c(0.83,0.18),
        legend.background = element_blank()) +
  coord_flip(ylim = c(0.5,75))
  
ggsave(filename = "Output/PerformanceComparison.png", plot = plotPerformance,
       device = "png", dpi = 600, width = 16, height = 9, units = "cm")



# Plot feature values for the different genotypes -------------------------

library(purrr)
library(dplyr)
library(ggplot2)

genotypes = rep(c("An1", "Bay0", "Bur0", "Col0", "LP26"), each = 3)
replicate = rep(c("A", "B", "C"), times = 5)
taskNames = paste0(genotypes, replicate)

tasks = map(taskNames, ~readRDS(paste0("Intermediate/",.x,".rds")))

datasets = map(tasks, ~getTaskData(.x))

for(i in 1:length(datasets)) {
  datasets[[i]] = mutate(datasets[[i]], Genotype = genotypes[i], replicate = replicate[i])
}

alldata = do.call("rbind", datasets)

alldata$Genotype = c(An1 = "An-1", 
                     Bay0 = "Bay-0", 
                     Bur0 = "Bur-0", 
                     Col0 = "Col-0", 
                     LP26 = "Lp2-6")[alldata$Genotype]
# Distribution of seed size in the different genotypes
ParticleSize = ggplot(filter(alldata, Class == "S") %>% 
                    rename(Accession = Genotype),
                  aes(color = Accession, x = Size)) + 
  geom_density() + 
  geom_density(data = filter(alldata, Class == "W", Genotype == "An-1", replicate == "A"),
               color = "black", lty = 2) +
  coord_cartesian(xlim = c(50,700)) + 
  scale_color_discrete(guide = guide_legend(title = "", nrow = 2)) +
  theme_bw() + theme(legend.position = "top") +
  xlab(expression("Particle size"~(mu*m))) + ylab("Probability density")

ggsave(filename = "Output/ParticleSize.png", plot =  ParticleSize,
       device = "png", dpi = 300, width = 8.5, height = 9.5, units = "cm")


FractionLargeNonSeed = (filter(alldata, Class == "W",  Genotype == "An-1", 
                               replicate == "A", Size > 200) %>% nrow())/
                       (filter(alldata, Class == "W", Genotype == "An-1", 
                               replicate == "A") %>% nrow)

# Pairs plot showing the different features and where seeds are lo --------

library(GGally)

getData2 = function(main_file, profile_file, datadir = "",
                   main_waste_file = NULL, profile_waste_file = NULL,
                   calibration = c(38.325, 0.185), clean = FALSE, cleanguess = 200) {
  
  # Retrieve data from the main_file and profile_file
  data = SeedSorter:::readData(file.path(datadir, main_file),
                  file.path(datadir, profile_file)) %>%
    dplyr::mutate(Class = "S")
  
  # Optionally clean the sample
  if(clean) {
    data = SeedSorter:::extractfeatures(data, calibration)
    data = SeedSorter:::cleanTrainingSample(data, guess = cleanguess)
  }
  
  
  # If a waste file is added, append it to the data
  if(!is.null(main_waste_file)) {
    waste = SeedSorter:::readData(file.path(datadir, main_waste_file),
                                  file.path(datadir, profile_waste_file)) %>%
      dplyr::mutate(Class = "W")
    if(clean) waste = SeedSorter:::extractfeatures(waste, calibration)
    data = dplyr::bind_rows(data, waste)
  }
  
  # Perform transformations (feature engineering) if not clean
  if(!clean) data = SeedSorter:::extractfeatures(data, calibration)
  
  # Select features to be used for classification
  dplyr::select(data, Extinction, rGreen, rYellow, rRed, P, Px, C, Size, Class)
}

genotypes = rep(c("An1", "Bay0", "Bur0", "Col0", "LP26"), each = 3)
replicate = rep(c("A", "B", "C"), times = 5)
taskNames = paste0(genotypes, "_", replicate)
datasets = vector("list", 15)
i = 0
for(taskName in taskNames) {
  i = i + 1
  main_file = paste0(taskName, "_01_prof.txt")
  profile_file = paste0(taskName, "_01_prof_prf.fst")
    datasets[[i]] = getData2(main_file = main_file, 
                   profile_file = profile_file, 
                   main_waste_file = "waste_01.txt", 
                   profile_waste_file = "waste_01_ch0_prf.fst",
                   datadir = "Input/", clean = TRUE)
}

for(i in 1:length(datasets)) {
  datasets[[i]] = mutate(datasets[[i]], Genotype = genotypes[i], replicate = replicate[i])
}

alldata = do.call("rbind", datasets)


subset_data = filter(alldata, Genotype == "Col0", replicate == "A") %>%
  select(Extinction, rGreen, rYellow, rRed, P, Px, C, Size, Class)

subset_data = rbind(filter(subset_data, Class == "S"),
                    filter(subset_data, Class == "W")[sample(1:9737, 2000),]) %>%
  rename(rF510 = rGreen, rF543 = rYellow, rF615 = rRed)


my_cor = function (data, mapping, alignPercent = 0.6, method = "pearson", 
                   use = "complete.obs", corAlignPercent = NULL, corMethod = NULL, 
                   corUse = NULL, ...) {
  cor_fn <- function(x, y) {
    cor(x, y, method = method, use = use)
  }
  xData <- eval_data_col(data, mapping$x)
  yData <- eval_data_col(data, mapping$y)
  xVal <- xData
  yVal <- yData
  
  xmin <- min(xVal, na.rm = TRUE)
  xmax <- max(xVal, na.rm = TRUE)
  xrange <- c(xmin - 0.01 * (xmax - xmin), xmax + 0.01 * 
                (xmax - xmin))
  ymin <- min(yVal, na.rm = TRUE)
  ymax <- max(yVal, na.rm = TRUE)
  yrange <- c(ymin - 0.01 * (ymax - ymin), ymax + 0.01 * 
                (ymax - ymin))
  corVal = cor_fn(xVal, yVal)
  p <- ggally_text(label = round(corVal, 2), mapping, size = 2 + 3*abs(corVal),
                   xP = 0.5, col = ifelse(corVal > 0, "darkgreen", "red"),
                   yP = 0.5, xrange = xrange, yrange = yrange, ...) + 
    theme(legend.position = "none")
  p
}

plot_pairs = ggpairs(data = select(subset_data, -Class), 
                     mapping = aes(col = subset_data$Class),
                     lower = list(continuous = wrap("points", alpha = 0.3, shape = 1)),
                     diag = list(continuous = wrap("densityDiag", alpha = 0.5)),
                     upper = list(continuous = wrap(my_cor, method = "spearman"))) +
  theme(axis.text.x.bottom = element_text(angle = 45))

ggsave("Output/PairFeatures.png",
       plot = plot_pairs, height = 17, width = 17, units = "cm")



# Size distribution of dust and seeds -------------------------------------

library(dplyr)
library(purrr)
library(ggplot2)


genotypes = rep(c("An1", "Bay0", "Bur0", "Col0", "LP26"), each = 3)
replicate = rep(c("A", "B", "C"), times = 5)
dataNames = paste0(genotypes, "_", replicate)

getUncleanedData = function(dataName) {
  main_file = paste0(dataName, "_01_prof.txt")
  profile_file = paste0(dataName, "_01_prof_prf.fst")
  data = getData(main_file = main_file, 
                 profile_file = profile_file, 
                 datadir = "Input/", clean = FALSE)
}

datasets = map(dataNames, getUncleanedData)


for(i in 1:length(datasets)) {
  datasets[[i]] = mutate(datasets[[i]], Genotype = genotypes[i], Replicate = replicate[i])
}

alldata = do.call("rbind", datasets)

alldata$Genotype = c(An1 = "An-1", 
                     Bay0 = "Bay-0", 
                     Bur0 = "Bur-0", 
                     Col0 = "Col-0", 
                     LP26 = "Lp2-6")[alldata$Genotype]

ParticleSize = ggplot(filter(alldata, Class == "S", Size < 800) %>% 
                    rename(Accession = Genotype),
                  aes(color = Replicate, x = Size)) + 
  geom_density() + 
  theme_bw() + 
  facet_wrap(~Accession) +
  theme(legend.position = "none") +
  xlab(expression("Seed size"~(mu*m))) + ylab("Probability density")

ggsave("Output/UncleanedSamples.png",
       plot = ParticleSize, height = 10, width = 18, units = "cm")

