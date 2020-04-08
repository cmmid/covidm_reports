
#SBATCH --job-name lmic-interventions
#SBATCH –t 60
#SBATCH –array=2-189

make $(DATADIR)/cases_bytime_sceni_$(SLURM_ARRAY_TASK_ID)_Uganda.rds