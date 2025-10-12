#!/bin/bash
echo "--- Starting Quarto render process ---"

quarto render 2-hosp/9-markov-H0-matching-sim.qmd && \
quarto render 2-hosp/9-markov-H0-misspec-clustered.qmd && \
quarto render 2-hosp/9-markov-H0-misspec.qmd && \

echo "--- Shutting Down Instance ---"
sudo shutdown -h now