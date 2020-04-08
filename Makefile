
-include local.makefile

COVIDMPATH ?= ..
REPDIR ?= ~/Dropbox/Covid_lmic/reports
DATADIR ?= ~/Dropbox/covidm_reports
INTINPUTDIR := ${DATADIR}/interventions/inputs

${REPDIR} ${DATADIR}:
	mkdir -p $@

R = Rscript $^ $@

LMIC.txt: X-LMIC.R
	Rscript $^ ${COVIDMPATH} $@

LMICargs.txt: LMIC.txt
	sed "s/ //g" $^ > $@
	sed -i '' "s/$$/-res\.rds/" $@

TARS := $(addprefix ${DATADIR}/,$(shell cat LMICargs.txt))

alltars: ${TARS}

testrep: ${REPDIR}/CaboVerde.pdf

allreps: $(patsubst %-res.rds,%.pdf,$(subst ${DATADIR},${REPDIR},${TARS}))

${DATADIR}/%-res.rds: X2-LMIC.R LMICargs.txt LMIC.txt | ${DATADIR}
	Rscript $< .. 100 $(filter-out $<,$^) $@

${REPDIR}/%.pdf: report.R ${DATADIR}/%-res.rds report-template.Rmd | ${REPDIR}
	${R}

INTCOUNTRIES := Kenya Uganda Zimbabwe
#INTSCENARIOS := $(foreach C,${INTCOUNTRIES},$(patsubst %,%_${C},$(shell seq 2 189)))
UNMITSCENARIOS := $(patsubst %,1_%,${INTCOUNTRIES})
#TSTSCENARIOS := $(patsubst %,5_%,${INTCOUNTRIES})

precursors: $(patsubst %,${DATADIR}/interventions/cases_sceni_%.rds,${UNMITSCENARIOS})

${DATADIR}/interventions/cases_sceni_%.rds: covidm_interventions_run_scenario.R helper_functions.R
	time Rscript $< ${COVIDMPATH} $(filter-out $<,$^) $(subst _, ,$*) ${INTINPUTDIR} $@

# consolidate.R actually consolidates everything
consolidate: ${DATADIR}/interventions/Uganda_consolidated.rds

# TODO: also rm the now-consolidated files?
${DATADIR}/interventions/%_consolidated.rds: consolidate.R
	time Rscript $^ $(@D)


cleanrds:
	rm -f *-res.rds
