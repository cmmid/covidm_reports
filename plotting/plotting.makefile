
plotting/%.rda:
	@cd plotting && make $(@F)

plotsupport: $(patsubst %,plotting/%.rda,plotfuns intplots plotpars)