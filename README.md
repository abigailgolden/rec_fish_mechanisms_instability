# Mechanisms of instability in recreational fisheries social-ecological systems

This repository contains the scripts to run the model and analysis used in the manuscript "Focusing on what matters most: Evaluating multiple challenges to stability in recreational fisheries."

## Notes for using this repository

The "model" folder includes the script used to run the model. 

The "analysis" folder includes scripts used to run analyses of the model's sensitivity to parameter values.

The "fig_scripts" folder includes scripts used to create those figures in the manuscript that are based on modeled data.

The scripts in "analysis" and "fig_scripts" both call the ``` model.R ``` script, so you should not need to run that script before running other scripts. 

Before generating the figures in "fig_scripts," users should create a subdirectory labeled "figures" within the repository. Figures will read out to this directory.

The scripts in this directory require the following packages:
```cowplot```
```dichromat```
```fishualize```   
```fmsb```
```GGally```
```ggnewscale```
```ggpubr```
```gridExtra```
```gridGraphics```
```here```
```patchwork```
```RColorBrewer```
```scales```
```tidyverse```
```viridis```
