# Dynamic Exploratory Graph Analysis of Emotions in Politics

Repository for the paper *Dynamic Exploratory Graph Analysis of Emotions in Politics*.

Inputs are in the `data` folder:
- `gps.csv` Data from [Global Party Survey](https://www.globalpartysurvey.org/)
- `videos-face-selection.csv` Face selection file from [Face of Populism](https://github.com/atomashevic/face-of-populism) repository
- `*-t330.csv` Time series of emotions and neutral score each video, also from [Face of Populism](https://github.com/atomashevic/face-of-populism) repository

Results from the paper are reporduced by running:

1. `src/analysis-additive.R` the main analysis presented in the paper
2. `src/figures.R` the main figures presented in the paper
3. `src/analysis-center.R` the supplementary analysis based on center transformation of compositional data

Output is located in `figures`, `results,` and `data` directories.
