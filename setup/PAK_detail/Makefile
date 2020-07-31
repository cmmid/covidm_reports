
-include ../../local.makefile

COVIDMPATH ?= ../../../covidm
DATADIR ?= ~/Dropbox/covidm_reports
#DATADIR ?= ~/Dropbox/covidm_test
SAMPN ?= 500
INDIR := ${DATADIR}/generation
OUTDIR := ${DATADIR}/hpc_inputs
DETDIR := ${DATADIR}/hpc_detailed

R = Rscript $^ $@

# TODO other directory structure dependencies
${DETDIR}:
	mkdir -p $@

params: create_params_set.R ${DETDIR}/worldpop5yr.lfs.csv
	Rscript $^ ${COVIDMPATH} ${DETDIR}

# currently, no explicit province contact matrices, so use national one
${DETDIR}/contact_matrices.rds: ${OUTDIR}/PAK/contact_matrices.rds
	cp $< $@

# TODO update dependencies
# ${DETDIR}/%/params_set.rds: create_params_set.R ${INDIR}/data_contacts_missing.csv
#	mkdir -p $(@D)
#	Rscript $^ ${COVIDMPATH} $* $@

${DETDIR}/timing.rds: ${OUTDIR}/PAK/timing.rds
	cp $< $@

${DETDIR}/run_options.rds: ${OUTDIR}/run_options.rds
	cp $< $@

${DETDIR}/alt_scenarios.rds: generate_scenario_parameters.R
	Rscript $^ $@

${DETDIR}/report_ref.rds: ${DETDIR}/alt_scenarios.rds

dosetup: params ${DETDIR}/alt_scenarios.rds