#!/usr/bin/env bash
echo "Read Env Variables"
source ~/zi_covid/myenvs.sh
echo "Starting conda"
source /opt/tljh/user/bin/conda
conda activate "edgar_covid_r"
echo "Pulling Code from git"
cd ~/zi_covid/covid19dashboard/
git pull
echo "Starting DB-Update"
cd ~/zi_covid/covid19dashboard/R/
/home/jupyter-esteiger/.conda/envs/edgar_covid_r/bin/R CMD BATCH renderdashboard.R
cd ~/zi_covid/covid19dashboard/
git add *
git commit -m "Auto update Dashboard"
git push