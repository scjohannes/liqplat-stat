#!/bin/bash
echo "--- Starting Quarto render process ---"

quarto render 2-hosp/8.1-sim-resampling-markov-H0-no-int.qmd
quarto render 2-hosp/8.2-sim-resampling-markov-H0-linear-int.qmd
quarto render 2-hosp/8.3-sim-resampling-markov-H0-full-int.qmd

echo "--- All rendering complete. Shutting down now. ---"
sudo shutdown -h now
