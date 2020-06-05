
-include ../local.makefile

# TODO start passing these in from linking makefile
COVIDMPATH ?= ../../covidm
DATADIR ?= ~/Dropbox/covidm_reports
INTINPUTDIR := ${DATADIR}/hpc_inputs
HPCDIR ?= ~/Dropbox/covidm_hpc_output

../utils/%:
	cd $(@D) && $(MAKE) $(@F)

# also depends on other bits, but 001 guaranteed to change if
# run sim has changed
${HPCDIR}/%/peak.qs: digest.R ${HPCDIR}/%/001.qs ../utils/sharedconst.rda
	Rscript $^ $@

${HPCDIR}/%/accs.qs ${HPCDIR}/%/alls.qs: ${HPCDIR}/%/peak.qs

${HPCDIR}/%.qs: run_scenarios.R
	mkdir -p $(@D)
	Rscript $^ ${COVIDMPATH} $(subst /, ,$*) ${INTINPUTDIR} $@

SIMRUNS ?= 2

# if asking local examples; uses % length to differentiate between local
# and HPC runs
%/peak.qs: digest.R ${HPCDIR}/%/001.qs ../utils/sharedconst.rda
	Rscript $^ $@
	
%/accs.qs %/alls.qs: %/peak.qs

%.qs: run_scenarios.R
	@mkdir -p $(@D)
	SIMRUNS=${SIMRUNS} Rscript $^ ${COVIDMPATH} $(subst /, ,$*) ${INTINPUTDIR} $@

# TODO: this depends on the all countries, peak|accs|001.qs
${HPCDIR}/summary.tar.gz:
	cd $(@D) && tar -czvf $(@F) */peak.qs */accs.qs */alls.qs */001.qs
