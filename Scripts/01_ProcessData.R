
# Load libraries and settings ---------------------------------------------
library(SeedSorter)


# Process the profiles ----------------------------------------------------

# This summarises the profiles of optical density and stores them as fst
# in the input files
for(genotype in c("An1", "Bay0", "Bur0", "Col0", "LP26")) {
  for(replicate in c("A", "B", "C")) {
    processFile(filename = paste0("Input/Training/",genotype, "_", replicate, "_prf.txt"))
  }
}
processFile(filename = paste0("Input/Training/waste_prf.txt"))
    


# Create classification tasks and save them -------------------------------

createAndSaveTask = function(main_file, profile_file, taskName) {
  data = getTrainingData(main_file = main_file, 
                 profile_file = profile_file, 
                 main_waste_file = "waste.txt", 
                 profile_waste_file = "waste_prf.fst",
                 datadir = "Input/Training", clean = TRUE)
  task = createTrainingTask(data, id = taskName)
  saveRDS(task, file = file.path("Intermediate",paste0(taskName, ".rds")))
  NULL
}

for(genotype in c("An1", "Bay0", "Bur0", "Col0", "LP26")) {
  for(replicate in c("A", "B", "C")) {
    taskName = paste0(genotype, replicate)
    main_file = paste0(genotype, "_", replicate, ".txt")
    profile_file = paste0(genotype, "_", replicate, "_prf.fst")
    createAndSaveTask(main_file, profile_file, taskName)
  }
}

