#!/usr/bin/env bash
echo "Read Env Variables"
source ~/zi_covid/myenvs.sh
echo "Starting conda"
# source /opt/tljh/user/bin/conda
conda activate "edgar_covid_r4"
echo "Pulling Code from git"
cd ~/zi_covid/covid19dashboard/
git pull
echo "Starting excel-Update"
cd ~/zi_covid/covid19dashboard/R/
/home/jupyter-esteiger/.conda/envs/edgar_covid_r4/bin/R CMD BATCH tables_4wochenbericht.R
cd ~/zi_covid/covid19dashboard/
git add *
git commit -m "Auto update excel for corona report"
git push