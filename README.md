## Repository of the measurements and R codes used in the analyses of coloration changes in *Bombina variegata*

<p align="center"> 
  Ariel Rodríguez
  <p align="center">
  Institut für Zoologie, Stiftung Tierärztliche Hochschule Hannover
  <p align="center">
  Bünteweg 17, 30559 Hannover, Germany
<p>




Collection of R scripts and associated data used for the processing and analysis of spectral meassurements applied in the manuscript "Evidence for local adaptation and coloration plasticity in dorsal crypsis of a deimatic species, the yellow-bellied toad (*Bombina variegata*)". Original meassurements and intermediate results and plots are provided in the (./data) and (./plots) folders. Please consult the manuscript for additional details.


### Field measurements- Local adaptation

We calculated the color (chromatic) and brightness (achromatic) contrasts of each toad against the three background substrates via visual modelling incorporating the irradiance measurements of the respective locality using R scripts (see below). We also calculated the color and brightness contrasts between the ventral yellow and black patches (inner contrast) and the contrasts between the ventral patches and the three substrates. Statistical analyses of the effects of the source locality on the color (ΔS) and brightness contrast (ΔL) of the dorsal and ventral coloration of the toads were conducted as detailed in script *Substrate_contrasts_Field.R*.

### Experiment - Coloration plasticity

The six measurements of each individual were stored in a MS Excel spreadsheet and the files with one spreadsheet per individual were imported into Pavo (https://rafaelmaia.net/pavo/index.html) in R using a custom script (*Batch_Pavo_import.R*). Using this script, spectrometric data were zero corrected, filtered between 300 – 700 nm, and re-sampled by one nanometer using Pavo functions and finally exported as .csv files. The brightness contrast (ΔL) of each toad to the substrate it was placed on during the habituation and experimental phases using a visual model including the irradiances measured in the experimental rooms (R script: *Visual_models.Pavo.R*). Total brightness and the effect of experimental substrate treatments on total brightness of the toads and related plots were calculated with the script *Total_brightness_statistics.R*. The brightness contrast (ΔL) of each toad to the substrate it was placed on during the experimental phase was compared to the (hypothetical) brightness contrast it would have on that substrate in the habituation phase. Statistics of brightness contrast data and plots were calculated with the script *Brightness_contrasts_statistics.R*.
