# Health Literacy Estimation in Newham Using MRP

Welcome to the GitHub repository for estimating health literacy levels in Newham using multilevel regression and poststratification (MRP) with the Skills for Life survey data.

## Table of Contents

- [Overview](#overview)
- [Data Sources](#data-sources)
- [Methodology](#methodology)
- [Setup](#setup)
- [Results](#results)
- [Contributing](#contributing)
- [License](#license)

## Overview

This project aims to estimate the health literacy of residents in Newham, London, using MRP techniques applied to the Skills for Life survey data. The goal is to provide detailed, small-area estimates that can be used to inform health policy and intervention strategies.

## Data Sources

- **Skills for Life Survey**: A survey measuring basic skills in literacy, numeracy, and ICT among adults in England.
- **Newham Resident Survey**: Data from the from an annual probability sample survey on the population characteristics of Newham.
- **Census 2021 and Annual Population Survey**: Local health profiles and indicators from the Office for National Statistics.

## Methodology

1. **Multilevel Regression**: A statistical technique that models the relationship between individual characteristics and health literacy while accounting for group-level variations (e.g., age, gender, education level).
   
2. **Poststratification**: Adjusts the regression estimates to match the population structure of Newham, using demographic data to ensure estimates are representative.

## Setup

1. **Clone the Repository**

    ```sh
    git clone https://github.com/n8thangreen/healthliteracy.git
    cd healthliteracy
    ```


## Results

The results of the analysis, including estimated health literacy rates across different demographic groups in Newham, can be found in the `results` directory. Visualizations and detailed summary statistics are also available.

## Contributing

Contributions are welcome! Please follow these steps:

1. Fork the repository.
2. Create a new branch (`git checkout -b feature-name`).
3. Commit your changes (`git commit -m 'Add feature'`).
4. Push to the branch (`git push origin feature-name`).
5. Open a Pull Request.

## License

This project is licensed under the MIT License. See the [LICENSE](LICENSE) file for details.

---

*Note*: For detailed information on multilevel regression and poststratification, refer to the [MRP book by Gelman and Hill](https://www.cambridge.org/core/books/data-analysis-using-regression-and-multilevelhierarchical-models/B58FAFC3B30B969D5BA8C2A18F88C2C5).

