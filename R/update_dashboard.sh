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
cd ~/covid19dashboard/R/
/home/jupyter-lekroll/.conda/envs/R_36/bin/R CMD BATCH renderdashboard.R
git add *
git commit -m "Auto update Dashboard"
git push