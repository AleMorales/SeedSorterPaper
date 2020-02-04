
# Load libraries and settings ---------------------------------------------

library(dplyr)
library(purrr)
library(ggplot2)
library(SeedSorter)


# Load datasets -----------------------------------------------------------


genotypes = rep(c("An1", "Bay0", "Bur0", "Col0", "LP26"), each = 3)
replicate = rep(c("A", "B", "C"), times = 5)

i = 0
cluster = vector("list", 15)
threshold = vector("list", 15)

for(genotype in genotypes) {
  for(replicate in replicate) {
    i = i + 1
    taskName = paste0(genotype, replicate)
    main_file = paste0(genotype, "_", replicate, "_01_prof.txt")
    profile_file = paste0(genotype, "_", replicate, "_01_prof_prf.fst")
    
    cluster[[i]] = getData(main_file = main_file, profile_file = profile_file, 
                           datadir = "Input/", clean = TRUE) %>%
                      mutate(Genotype = genotype, Replicate = replicate)
    
    threshold[[i]] = getData(main_file = main_file, profile_file = profile_file, 
                      datadir = "Input/", clean = FALSE) %>%
                      mutate(Class = ifelse(Size > 200, "S", "W"),
                             Genotype = genotype, Replicate = replicate)
  }
}

cluster = do.call("rbind", cluster)
threshold = do.call("rbind", threshold)

table(cluster$Class, threshold$Class)

mmce = numeric(5) 
dustfract = numeric(5)
i = 0
for(genotype in c("An1", "Bay0", "Bur0", "Col0", "LP26")) {
  i = i + 1
  mmce[[i]] = measureMMCE(filter(cluster, Genotype == genotype)$Class, 
                          filter(threshold, Genotype == genotype)$Class)
  temp = table(filter(cluster, Genotype == genotype)$Class)
  dustfract[[i]] = temp[2]/sum(temp)
}


nseeds = sum(cluster$Class == "S")/15

distrSeeds = quantile(filter(cluster, Class == "S")$Size, probs = c(0.025,0.9755))
avgSeed = median(filter(cluster, Class == "S")$Size)
avgSeedGenotype = group_by(cluster, Genotype) %>% summarise(muSize = median(Size))
avgSeedGenotype


# Waste material ----------------------------------------------------------

genotypes = rep(c("An1", "Bay0", "Bur0", "Col0", "LP26"), each = 3)
replicate = rep(c("A", "B", "C"), times = 5)
taskNames = paste0(genotypes, replicate)

tasks = map(taskNames, ~readRDS(paste0("Intermediate/",.x,".rds")))

datasets = map(tasks, ~getTaskData(.x))

for(i in 1:length(datasets)) {
  datasets[[i]] = mutate(datasets[[i]], Genotype = genotypes[i], replicate = replicate[i])
}

alldata = do.call("rbind", datasets)


wasteParticles = group_by(alldata, Genotype, replicate) %>%
  filter(Class == 'W') %>% summarise(muSize = median(Size), 
                                     lowerSize = quantile(Size, 0.025),
                                     upperSize = quantile(Size, 0.975),
                                     nparticles = n())

summary(wasteParticles)
