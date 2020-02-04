
# Load require libraries and settings -------------------------------------
library(SeedSorter)
library(future)
library(furrr)
library(purrr)

plan(multiprocess)

# Load classification tasks -----------------------------------------------

taskNames = paste0(rep(c("An1", "Bay0", "Bur0", "Col0", "LP26"), each = 3),
                   rep(c("A", "B", "C"), times = 5))

tasks = map(taskNames, ~readRDS(paste0("Intermediate/",.x,".rds")))



# Benchmark algorithms across all tasks -----------------------------------

benchmarkAlgorithm = function(algorithm, control = list()) {
  bench = compareAlgorithms(algorithms = algorithm, task = tasks, tuning = TRUE, control = control)
  saveRDS(object = bench, file = paste0("Intermediate/BenchmarkAllFiles_",algorithm,".rds"))
  NULL
}

benchmarkEnsemble = function(algorithms, control = list()) {
  bench = compareEnsemble(algorithms = algorithms, task = tasks, tuning = TRUE, control = control)
  saveRDS(object = bench, file = paste0("Intermediate/BenchmarkAllFiles_Ensemble.rds"))
  NULL
}

benchmarkAlgorithm("extinction")
benchmarkAlgorithm("lda")
benchmarkAlgorithm("qda")
benchmarkAlgorithm("knn", control = list(parallel = TRUE))
benchmarkAlgorithm("logistic", control = list(parallel = TRUE))
benchmarkAlgorithm("naiveBayes", control = list(parallel = TRUE))
benchmarkAlgorithm("randomforest", control = list(parallel = FALSE))
benchmarkAlgorithm("svm", control = list(parallel = TRUE))
benchmarkAlgorithm("xgboost", control = list(parallel = TRUE))
benchmarkEnsemble(list(c("extinction", "lda", "qda", "naiveBayes", "logistic")), control = list(parallel = TRUE))

# Benchmark algorithms within tasks -----------------------------------

benchmarkAlgorithmWithin = function(algorithm) {
  bench = future_map(tasks, ~compareAlgorithms(algorithms = algorithm, task = .x, tuning = TRUE))
  saveRDS(object = bench, file = paste0("Intermediate/BenchmarkAllFilesWithin_",algorithm,".rds"))
  NULL
}

benchmarkEnsembleWithin = function(algorithms) {
  bench = future_map(tasks, ~compareEnsemble(algorithms = algorithms, task = .x, tuning = TRUE))
  saveRDS(object = bench, file = paste0("Intermediate/BenchmarkAllFilesWithin_Ensemble.rds"))
  NULL
}

benchmarkAlgorithmWithin("extinction")
benchmarkAlgorithmWithin("lda")
benchmarkAlgorithmWithin("qda")
benchmarkAlgorithmWithin("knn")
benchmarkAlgorithmWithin("logistic")
benchmarkAlgorithmWithin("naiveBayes")
benchmarkAlgorithmWithin("randomforest")
benchmarkAlgorithmWithin("svm")
benchmarkAlgorithmWithin("xgboost")
benchmarkAlgorithmWithin("kmeans")
benchmarkEnsembleWithin(list(c("extinction", "lda", "qda", "naiveBayes", "logistic")))
