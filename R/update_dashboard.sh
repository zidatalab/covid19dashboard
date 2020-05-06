#!/usr/bin/env bash
echo "Read Env Variables"
source ~/myenv.sh
echo "Starting conda"
source /opt/tljh/user/bin/conda
conda activate "R_36"
echo "Pulling Code from git"
cd ~/covid19dashboard
git pull
echo "Starting DB-Update"
cd ~/covid-19/R/
/home/jupyter-lekroll/.conda/envs/R_36/bin/R CMD BATCH renderdashboard.R
git add *
git commit -m "Updated Dashboard"
git push