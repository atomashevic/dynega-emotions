# Dynamic Exploratory Graph Analysis of Emotions in Politics

Repository for the paper [Dynamic Exploratory Graph Analysis of Emotions in Politics](https://osf.io/preprints/psyarxiv/zgtrb/).

Inputs are in the `data` folder:
- `gps.csv` Data from [Global Party Survey](https://www.globalpartysurvey.org/)
- `videos-face-selection.csv` URL and face selection file from [Face of Populism](https://github.com/atomashevic/face-of-populism) repository

Results from the paper are reporduced by running two R scripts:

1. `src/process-videos.R` Process the videos and extracts the FER scores, saves results to `data/revision/` 
2. `src/analysis-revision.R` produces all results and plots from the paper

The entire output is located in `figures` and `data` directories.
