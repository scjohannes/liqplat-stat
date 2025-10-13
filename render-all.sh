#!/bin/bash
echo "--- Starting Quarto render process ---"

quarto render 2-hosp/10-power-or-09.qmd && \
quarto render 2-hosp/10-power-or-08.qmd && \
quarto render 2-hosp/10-power-or-07.qmd && \

echo "--- Shutting Down Instance ---"
sudo shutdown -h now