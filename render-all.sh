#!/bin/bash
echo "--- Starting Quarto render process ---"

quarto render 2-hosp/8.4-sim-resampling-markov-H0-no-int-cluster.qmd && \

echo "--- Shutting Down Instance ---"
sudo shutdown -h now