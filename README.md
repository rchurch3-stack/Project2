# ST 558 Project 2 – Bank Marketing Explorer

## Overview

Bank Marketing Explorer is an interactive Shiny application that allows users to explore a banking marketing dataset through filtering, summary statistics, tables, visualizations, and data downloads.

The application demonstrates reactive programming in Shiny by allowing users to subset the data using both categorical and numeric filters before generating updated summaries and plots.

---

## Dataset

This project uses the **Bank Marketing Dataset**, which contains customer information collected during direct marketing campaigns conducted by a Portuguese banking institution. The goal of the original dataset is to predict whether a customer will subscribe to a term deposit.

The training and testing datasets were combined before analysis.

Dataset source:

https://www.kaggle.com/datasets/prakharrathi25/banking-dataset-marketing-targets :contentReference[oaicite:0]{index=0}

---

## Features

The application includes:

- Interactive filtering using categorical and numeric variables
- Dynamic sliders for numeric filtering
- Apply Filters button to update all results
- About page describing the application and dataset
- Interactive data exploration
  - One-way contingency tables
  - Two-way contingency tables
  - Summary statistics
  - Bar charts
  - Histograms
  - Scatter plots
- User-selectable variables for plots and summaries
- Download filtered data as a CSV file

---

## Packages Used

- shiny
- dplyr
- ggplot2
- readr
- DT
- shinycssloaders

---

## Running the App

1. Clone this repository.
2. Open the project in RStudio.
3. Install any required packages.
4. Open `app.R`.
5. Click **Run App**.

---

## Author

Ralph Church

ST 558 – Data Science for Statisticians
NC State University
