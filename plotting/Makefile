
DATADIR ?= ~/Dropbox/covidm_reports
INTINPUTDIR := ${DATADIR}/hpc_inputs

R = Rscript $^ $@

#PLOTRS := $(shell ls *.R)

#allsupport: $(patsubst %.R,%.rda,${PLOTRS})

plotfuns.rda: plotting_support.R
	${R}

intplots.rda: intervention_plots.R $(patsubst %,${INTINPUTDIR}/%.rds,scenarios scenarios_overview)
	${R}

plotpars.rda: plot_parameters.R
	${R}