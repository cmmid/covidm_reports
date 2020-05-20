
plotfuns.rda: plotting_support.R
	${R}

intplots.rda: intervention_plots.R $(patsubst %,${INTINPUTDIR}/%.rds,scenarios scenarios_overview)
	${R}

plotpars.rda: plot_parameters.R
	${R}

PLOTREF := plotfuns.rda intplots.rda plotpars.rda

