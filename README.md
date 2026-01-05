# LIQPLAT Statistical Analysis

This repository contains the Statistical Analysis Plan (SAP) and associated code for the **LIQPLAT** trial (*The implementation of liquid biopsies in routine care of patients with advanced solid cancer*).

For details on the trial design and protocol, see the medRxiv preprint:
[The implementation of liquid biopsies in routine care of patients with advanced solid cancer (LIQPLAT): a study protocol for a single arm trial](https://www.medrxiv.org/content/10.1101/2025.02.13.25322206v1)

## Overview

LIQPLAT is a single-arm trial (SAT) investigating ctDNA implementation in routine cancer care, with a randomized comparison against an external comparator from a prospective research registry.

## Main Documents

- **`SAP.qmd`**: The central Statistical Analysis Plan. It aggregates and summarizes the methods and results for the primary clinical endpoints (Quality of Life, Time Alive and Out of Hospital, and Overall Survival).
- **`process-evluation-sap.qmd`**: Details the statistical analyses for the process evaluation.

## Analysis Modules

The analysis is modularized by clinical endpoint. Note: Access to a historical dataset is required to run the full pipeline. Simulated datasets may be provided for reproducibility of specific steps.

- **`1-qol/` (Quality of Life):** Bayesian First-Order Markov Ordinal Transition Model. 
- **`2-hosp/` (Time Alive and Out of Hospital):** Bayesian Second-Order Markov Ordinal Transition Model.
- **`3-survival/` (Overall Survival):** Bayesian Proportional Hazards model using M-splines and RMST calculations.
