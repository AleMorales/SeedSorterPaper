# Seed sorting data and scripts

This repository contains all the raw data and R script files required for the
paper entitled "A high throughput method for quantifying number and size distribution
of *Arabidopsis* seeds using large particle flow cytometry" (under review) that
makes use of the [*SeedSorter*](https://github.com/AleMorales/SeedSorter) R package.
Running the scripts (after all dependencies are installed) will result in the `Output`
folder being populated with the figures used in the paper as PNG files. For 
comparison, we include these figures inside `Output/FiguresPublication`.

## Data

The raw data is located in the folder `Input`, distinguishing between the data used
for the training of algorithms (`Input/Training`) and the data used to demonstrate
the application of the trained algorithms with new, unlabelled samples (`Input/Application`).

For each sample, two types of files are retrieved from the large particle
flow cytometer: a file with properties measured for each particle and a file with
the profile of optical density for each particle. Both types of filters are stored
as plain text files (i.e. `txt`) and we distinguish between them by the use of the
suffix `_prof` when referring to profiles of optical density. Because files with
profiles of optical density can be quite large, we only include them for the training
samples, but not for the unlabelled samples. In both cases, we include the result
of processing the optical density profiles with *SeedSorter*. The result of this 
processing is stored in files with the `fst` extension. These files are much 
smaller and can be used directly for training algorithms and making predictions.

## Scripts

The R scripts required to reproduce the figures in the paper are included in the
folder `Scripts` and are numbered according to the order in which they should be
executed. Running these scripts will generate a series of intermediate files in
the folder `Intermediate` which can be used to restart the analysis at several
points within the data analysis pipeline. Since these intermediate files can be
quite large, they have not been included in this Github repository.


