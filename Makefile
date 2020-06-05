
-include local.makefile

COVIDMPATH ?= ../covidm
REPDIR ?= ~/Dropbox/Covid_LMIC/Country_Modelling_Reports
DATADIR ?= ~/Dropbox/covidm_reports
INTINPUTDIR := ${DATADIR}/hpc_inputs
GENDIR := ${DATADIR}/generation
HPCDIR ?= ~/Dropbox/covidm_hpc_output

${REPDIR} ${DATADIR} ${INTINPUTDIR} ${HPCDIR}:
	mkdir -p $@

R = Rscript $^ $@

# all setup; TODO move to subdirectory

include setup/setup.makefile

include plotting/plotting.makefile

include simulate/simulate.makefile

include report/report.makefile