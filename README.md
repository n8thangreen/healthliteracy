# Health Literacy Estimation in Newham Using MRP

Welcome to the GitHub repository for estimating health literacy levels in [Newham, London](https://www.newham.gov.uk/) using multilevel regression and poststratification (MRP) with the Skills for Life survey data.

This work is carried out as part of a [Newham Policy Fellowship Award](https://www.ucl.ac.uk/statistics/news/2024/may/newham-policy-fellowship-award#:~:text=The%20Fellowship%20Programme%20strengthens).

## Table of Contents

- [Overview](#overview)
- [Associated Paper](#associated-paper)
- [Data Sources](#data-sources)
- [Methodology](#methodology)
- [Setup](#setup)
- [Results](#results)
- [Contributing](#contributing)
- [License](#license)

## Overview

This project aims to estimate the health literacy of residents in Newham, London, using MRP techniques applied to the Skills for Life survey data. The goal is to provide detailed, small-area estimates.

## Associated Paper

This is the code for the associated paper:

**Green N, Hawkings Y-R.** Estimating the determinants of health literacy for policy prioritisation: a local level case study in Newham, London. *BMC Public Health*. 2026 Jan 26;26(1):649. doi: [10.1186/s12889-025-26067-9](https://doi.org/10.1186/s12889-025-26067-9).

**PubMed**: [PMID: 41588414](https://pubmed.ncbi.nlm.nih.gov/41588414/)
**PMCID**: [PMC12918084](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC12918084/)

## Data Sources

- **[Skills for Life Survey](https://www.gov.uk/government/publications/2011-skills-for-life-survey)**: A survey measuring basic skills in literacy, numeracy, and ICT among adults in England.
- **[Newham Resident Survey](https://www.newham.info/residents-survey/)**: Data from an annual probability sample survey on the population characteristics of the London Borough of Newham.
- **[Labour Force Survey (LFS)](https://www.ons.gov.uk/surveys/informationforhouseholdsandindividuals/householdandindividualsurveys/labourforcesurvey), [Census 2021](https://www.ons.gov.uk/census)**: Population demographic data.

## Methodology

1. **Multilevel Regression**: A statistical technique that models the relationship between individual characteristics and health literacy while accounting for group-level variations (e.g., age, geography, socioeconomic status).
   
2. **Poststratification**: Adjusts the regression estimates to match the population structure of Newham, using demographic data to ensure estimates are representative.

## Setup

**Clone the Repository**

```sh
git clone https://github.com/n8thangreen/healthliteracy.git
cd healthliteracy
```


## Results

The results of the analysis, including estimated health literacy rates across different demographic groups in Newham, can be found in the `results` directory. Visualizations and detailed summary statistics are provided.

## Contributing

Contributions are welcome! Please follow these steps:

1. Fork the repository.
2. Create a new branch (`git checkout -b feature-name`).
3. Commit your changes (`git commit -m 'Add feature'`).
4. Push to the branch (`git push origin feature-name`).
5. Open a Pull Request.

## License

This project is licensed under the MIT License. See the [LICENSE](LICENSE) file for details.
