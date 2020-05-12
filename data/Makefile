
RAWPHSMDIR ?= ~/Downloads/phsm-who
DATADIR ?= ~/Dropbox/covidm_reports
INTINPUTDIR := ${DATADIR}/generation

INTERDIR := interventions
INTRODIR := introductions

default: intdig

${INTERDIR} ${INTRODIR}:
	mkdir -p $@

INTDATA := ${INTERDIR}/clean.csv

intdata: ${INTDATA}

${INTDATA}: $(wildcard ${RAWPHSMDIR}/*.csv) | ${INTERDIR}
	cp $< $@

INTDIG := ${INTERDIR}/digested.csv

${INTDIG}: interventions_digest.R ${INTDATA} ${INTINPUTDIR}/data_contacts_missing.csv
	Rscript $^ $@

INTROS := ${INTRODIR}/digested.csv

${INTROS}: introductions.R ${INTINPUTDIR}/data_contacts_missing.csv | ${INTRODIR}
	Rscript $^ $@

intdig: ${INTDIG} ${INTROS}
