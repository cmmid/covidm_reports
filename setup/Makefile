
-include ../local.makefile

COVIDMPATH ?= ../../covidm
DATADIR ?= ~/Dropbox/covidm_reports
#DATADIR ?= ~/Dropbox/covidm_test
SAMPN ?= 500
INDIR := ${DATADIR}/generation
OUTDIR := ${DATADIR}/hpc_inputs

R = Rscript $^ $@

# TODO other directory structure dependencies
${OUTDIR}:
	mkdir -p $@

# This file forms the reference locales for which we generate reports
# used to be -f1
LMIC.txt: ${INDIR}/data_contacts_missing.csv
	cut -d',' -f6 $< | sed '1d' > $@

###############################################################################
# This is for generating contact matrices
LMICcontact_matrices.txt: LMIC.txt
	sed "s/$$/\/contact_matrices\.rds/" $^ > $@

# there are some special cases, so we don't overwrite contact matrices currently
${OUTDIR}/%/contact_matrices.rds: bootstrap_empirical_contact_matrices.R \
../data/empirical_contact_matrices/%/contacts.csv \
../data/empirical_contact_matrices/%/participants.csv
	Rscript $^ $* ${SAMPN} $@

# there are some special cases, so we don't overwrite contact matrices currently
${OUTDIR}/%/contact_matrices.rds: create_contact_matrices.R \
${INDIR}/data_contacts_missing.csv
	Rscript $^ ${COVIDMPATH} $* ${SAMPN} $@

###############################################################################
# This is for generating parameter sets
LMICparams_set.txt: LMIC.txt
	sed "s/$$/\/params_set\.rds/" $^ > $@

${OUTDIR}/%/params_set.rds: create_params_set.R ${INDIR}/data_contacts_missing.csv
	mkdir -p $(@D)
	Rscript $^ ${COVIDMPATH} $* $@
# TODO: write override for Cox's Bazar Camp


###############################################################################
# This is for generating initial conditions
LMICinits.txt: LMIC.txt
	sed "s/$$/\/timing\.rds/" $^ > $@

${OUTDIR}/%/timing.rds: create_timing_ref.R ${OUTDIR}/../generation/data_contacts_missing.csv \
../data/interventions/digested.csv ../data/introductions/digested.csv
	mkdir -p $(@D)
	Rscript $^ $* $@