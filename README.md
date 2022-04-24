<div align="center">
 
 <h1>Intermittent hypoxia and cerebellar development</h1>

 [![License](https://img.shields.io/badge/license-MIT-blue.svg)](/LICENSE)
 [![DOI](https://zenodo.org/badge/484983414.svg)](https://zenodo.org/badge/latestdoi/484983414)

</div>

Data and R companion code for (Leroux S., Rodriguez-Duboc A., Arabo A., Basille-Dugay M., Vaudry D., & Burel D.) paper: *"Hypoxia is associated with a retardation of cerebellar development and long-term functional deficits in a mouse model of apnea of prematurity"*

## 📖 Citation:

- Paper: *To be disclosed*  

- Code: Marc-Aurèle RIVIERE, & Agalic Rodriguez-Duboc. (2022). ma-riviere/LT-AoP-22: Initial release (v1.0.0). Zenodo. https://doi.org/10.5281/zenodo.6480948

## ❔ Requirements:

- R version 4.1 (or later). We recommend using R 4.1.3 since the project was last tested using that version.

## 💻 Repository structure:

- `LT-AoP-22.RProj`: R Studio project (open this first).
- `analysis`: Analysis (R Markdown) files, split by sub-section (e.g. behavioral data, ...). 
  - The first code chunck of any of the .Rmd files will load all the packages required for the project.
- `data`: Raw data used for this paper.
- `fig`: Figures outputted by the RMarkdown files.
- `R`: R scripts declaring the functions called withing the RMarkdown analysis files (e.g. `viz.R` for the figures, `data.R` for the data loading).
- `config.yml`: Specifies the paths to the data files.

## 📜 Licence:

[MIT](LICENSE)

## ✨ Contributors:

- **Rivière Marc-Aurèle**:  
[![ORCID](https://img.shields.io/badge/ORCID-A6CE39?style=flat-square&labelColor=white&logo=orcid&logoColor=A6CE39)][ORCID_MAR]
[![Research Gate](https://img.shields.io/badge/ResearchGate-00CCBB?style=flat-square&labelColor=white&logo=researchgate&logoColor=00CCBB)][RG_MAR]

- **Rodriguez-Duboc Agalic**:  
[![ORCID](https://img.shields.io/badge/ORCID-A6CE39?style=flat-square&labelColor=white&logo=orcid&logoColor=A6CE39)][ORCID_ARD]
[![Research Gate](https://img.shields.io/badge/ResearchGate-00CCBB?style=flat-square&labelColor=white&logo=researchgate&logoColor=00CCBB)][RG_ARD]

## 📫 Contact:

For any question, please contact the primary author of the paper, **Agalic Rodriguez-Duboc**:  
<a href="mailto:agalic.rd@gmail.com?subject=Intermittent%20Hypoxia%20and%20Cerebellar%20Development">![Gmail](https://img.shields.io/badge/Gmail-C71610?style=flat-square&labelColor=white&logo=Gmail&logoColor=C71610)</a>


<!----------------------------------->

[RG_MAR]: https://www.researchgate.net/profile/Marc_Aurele_Riviere2
[ORCID_MAR]: https://orcid.org/0000-0002-5108-3382
[RG_ARD]: https://www.researchgate.net/profile/Agalic-Rodriguez-Duboc
[ORCID_ARD]: https://orcid.org/0000-0002-2084-3780
