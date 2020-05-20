
-include local.makefile

COVIDMPATH ?= ../covidm
REPDIR ?= ~/Dropbox/Covid_LMIC/Country_Modelling_Reports
DATADIR ?= ~/Dropbox/covidm_reports
INTINPUTDIR := ${DATADIR}/hpc_inputs
HPCDIR ?= ~/Dropbox/covidm_hpc_output

${REPDIR} ${DATADIR} ${INTINPUTDIR} ${HPCDIR}:
	mkdir -p $@

R = Rscript $^ $@

# all setup; TODO move to subdirectory

include setup/setup.makefile