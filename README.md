# phytomelanin
Data and scripts from **"Evolution of the occurrence of phytomelanin in aerial stems of American Vernonieae subtribes (Asteraceae)"**, submitted and under review in Flora. 

Authors: Josiane Wolff, Benoit Francis Patrice Loeuille, Carolina M. Siniscalchi, Fernanda Maria Cordeiro de Oliveira, Ana Claudia Rodrigues, Makeli Garibotti Lusa

## How to use this repo:

### scripts: 
Contains all scripts used to [calculate](/scripts/all.models.AIC.R) and [plot](/scripts/phytomelanin.ADR.plot.R) character state evolution, calculate [phylogenetic signal](/scripts/phylogenetic.signal.R) and [environmental tests](scripts/environmental_tests.R). All input files needed to run these scripts are in the [data](/data) folder. 

### data:
Contains the trimmed [phylogenetic tree](data/tree.tre), [phytomelanin traits](data/traits.bi.csv) and [environmental traits](data/variables_phytomelanin.csv) (this is similar to Supplemental Table 1 in the manuscript).

### occurrences:
Contains occurrence records for each species included in the study. Used to generate the map on Figure 4. 

### pnos_average:
Contains species averages for each environmental variable tested. Also contained in [variables_phytomelanin.csv](data/variables_phytomelanin.csv).

### supplementals
Contains supplemental files indicated in the manuscript.
