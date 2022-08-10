<div align="center">
 
 <h1>Intermittent hypoxia and cerebellar development</h1>
 <h3><i>GitHub Pages branch</i></h3>

 [![License](https://img.shields.io/badge/license-MIT-blue.svg)](/LICENSE)
 [![DOI](https://zenodo.org/badge/484983414.svg)](https://zenodo.org/badge/latestdoi/484983414)
 
 <a href = "https://ma-riviere.github.io/LT-AoP-22/">Link to the documentation</a>

</div>

Data and R companion code for (Leroux S., Rodriguez-Duboc A., Arabo A., Basille-Dugay M., Vaudry D., & Burel D., 2022) paper: *"Hypoxia is associated with a retardation of cerebellar development and long-term functional deficits in a mouse model of apnea of prematurity"*

## 📖 Citation:

- **Paper:** *In Press*  

- **Code:** Marc-Aurèle Rivière, & Agalic Rodriguez-Duboc. (2022). ma-riviere/LT-AoP-22: final release (v2.0). Zenodo. https://doi.org/10.5281/zenodo.6480947

## ❔ Requirements:

- R version 4.1 or newer (4.2.1 recommended)
- R Studio version 2022.02 or newer (2022.07 recommended)

## 💻 Repository structure:

- `LT-AoP-22.RProj`: R Studio project (open this first).
- `data`: The raw data used for this paper.
- `docs`: The [webpage](https://ma-riviere.github.io/LT-AoP-22/) version of this repository, hosted through GitHub pages.
  - This webpage showcases the code of the project (and output of said code) in a more accessible manner.
- `quarto`: The quarto files (.qmd) used to generate the [webpage](https://ma-riviere.github.io/LT-AoP-22/) of this repository.
  - Reading and knitting .qmd files requires R Studio *v2022.02* or newer (in version *2022.02*, Quarto has to be enabled in the Global Options *R Markdown -> Quarto*).
- `R`: R scripts declaring the functions called withing the analysis files (e.g. `viz.R` for the figures, `data.R` for the data loading).
- `config.yml`: Indexes the paths to various external files used within the code (e.g. data, templates, ...).

## 📜 Licence:

[MIT](LICENSE)

## ✨ Contributors:

- **Marc-Aurèle Rivière**:  
[![ORCID](https://img.shields.io/badge/ORCID-A6CE39?style=flat-square&labelColor=white&logo=orcid&logoColor=A6CE39)][ORCID_MAR]
[![Research Gate](https://img.shields.io/badge/ResearchGate-00CCBB?style=flat-square&labelColor=white&logo=researchgate&logoColor=00CCBB)][RG_MAR]

- **Agalic Rodriguez-Duboc**:  
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
