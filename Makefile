
-include local.makefile

COVIDMPATH ?= ../covidm
REPDIR ?= ~/Dropbox/Covid_lmic/reports
DATADIR ?= ~/Dropbox/covidm_reports
INTINPUTDIR := ${DATADIR}/interventions/inputs
HPCDIR ?= ~/Dropbox/covidm_hpc_output

${REPDIR} ${DATADIR}:
	mkdir -p $@

R = Rscript $^ $@

# This file forms the reference locales for which we generate reports

LMIC.txt: create_reference_countries.R ${INTINPUTDIR}/../generation_data/data_contacts_missing.csv
	Rscript $^ ${COVIDMPATH} $@

# This is for output root targets

LMICroots.txt: LMIC.txt
	sed "s/[^a-zA-Z]//g" $^ > $@-tmp
	tr '[:upper:]' '[:lower:]' < $@-tmp > $@


###############################################################################
# This is for generating contact matrices

LMICcontact_matrices.txt: LMICroots.txt
	sed "s/$$/\/contact_matrices\.rds/" $^ > $@

CMS := $(addprefix ${INTINPUTDIR}/,$(shell cat LMICcontact_matrices.txt))

allcontactmatrices: ${CMS} | LMICcontact_matrices.txt

${INTINPUTDIR}/%/contact_matrices.rds: create_contact_matrices.R ${INTINPUTDIR}/../generation_data/data_contacts_missing.csv
	mkdir -p $(@D)
	Rscript $^ ${COVIDMPATH} $* $@
	[ -f "$@" ] && touch $@

testcm: ${INTINPUTDIR}/uganda/contact_matrices.rds | LMICcontact_matrices.txt

###############################################################################
# This is for generating parameter sets

LMICparams_set.txt: LMICroots.txt
	sed "s/$$/\/params_set\.rds/" $^ > $@

PSS := $(addprefix ${INTINPUTDIR}/,$(shell cat LMICparams_set.txt))

allparamsets: ${PSS} | LMICparams_set.txt

${INTINPUTDIR}/%/params_set.rds: create_params_set.R ${INTINPUTDIR}/../generation_data/data_contacts_missing.csv
	mkdir -p $(@D)
	Rscript $^ ${COVIDMPATH} $* $@

testps: ${INTINPUTDIR}/caboverde/params_set.rds | LMICparams_set.txt


# All the interventions

ROOTS := $(shell cat LMICroots.txt)

#${HPCDIR}/%/001.sev: translate.R ${HPCDIR}/%/001.qs
#	time Rscript $< ${HPCDIR} ${COVIDMPATH} $@

# TODO: this depends on country/001 - 189.qs
${HPCDIR}/%/peak.qs: digest.R
	Rscript $^ $@

${HPCDIR}/%/accs.qs ${HPCDIR}/%/alls.qs: ${HPCDIR}/%/peak.qs

${HPCDIR}/%.qs: run_scenarios.R helper_functions.R
	mkdir -p $(@D)
	Rscript $^ ${COVIDMPATH} $(subst /, ,$*) ${INTINPUTDIR} $@

# TODO: this depends on the all countries, peak|accs|001.qs
${HPCDIR}/summary.tar.gz:
	cd $(@D) && tar -czvf $(@F) */peak.qs */accs.qs */alls.qs */001.qs




### TESTING TARGETS

TESTCTY := caboverde

${TESTCTY}/peak.qs: digest.R
	time Rscript $^ $@

${TESTCTY}/%.qs: run_scenarios.R helper_functions.R
	mkdir -p $(@D)
	SIMRUNS=10 time Rscript $^ ${COVIDMPATH} ${TESTCTY} $(subst /, ,$*) ${INTINPUTDIR} $@

testint1: $(patsubst %,${TESTCTY}/%.qs,001)

testint: $(patsubst %,${TESTCTY}/%.qs,$(shell seq -f%03g 1 189))

testdig: ${TESTCTY}/peak.qs

# TODO: this depends on the all countries, peak|accs|001.qs
test.tar.gz:
	tar -czvf $@ */peak.qs */accs.qs */alls.qs */001.qs

#LMICargs.txt: LMIC.txt
#	sed "s/[^a-zA-Z]//g" $^ > $@
#	sed -i '' "s/$$/-res\.rds/" $@




#TARS := $(addprefix ${DATADIR}/,$(shell cat LMICargs.txt))

#alltars: ${TARS}

#allreps: $(patsubst %-res.rds,%.pdf,$(subst ${DATADIR},${REPDIR},${TARS}))

RUNS ?= 100

testres: ${DATADIR}/Angola-res.rds

allres: $(addprefix ${DATADIR}/,$(shell cat LMICargs.txt))

${DATADIR}/%-res.rds: X2-LMIC.R LMICargs.txt LMIC.txt ${INTINPUTDIR}/../generation_data/data_contacts_missing.csv | ${DATADIR}
	Rscript $^ ${COVIDMPATH} ${RUNS} $@

${REPDIR}/%.pdf: report.R ${DATADIR}/%-res.rds report-template.Rmd COVID.bib | ${REPDIR}
	Rscript $(filter-out %.bib,$^) ${INTINPUTDIR} ${INTINPUTDIR}/../generation_data/lmic_early_deaths.csv $@

REPS := $(addprefix ${REPDIR}/,$(subst -res.rds,.pdf,$(shell cat LMICargs.txt)))

allrep: ${REPS}

TESTREP := Comoros

testreport.pdf: report.R ${DATADIR}/${TESTREP}-res.rds report-template.Rmd COVID.bib
	Rscript $(filter-out %.bib,$^) ${INTINPUTDIR} ${INTINPUTDIR}/../generation_data/lmic_early_deaths.csv $@
	

#INTCOUNTRIES := Kenya Uganda Zimbabwe
#INTSCENARIOS := $(foreach C,${INTCOUNTRIES},$(patsubst %,%_${C},$(shell seq 2 189)))
#UNMITSCENARIOS := $(patsubst %,1_%,${INTCOUNTRIES})
#TSTSCENARIOS := $(patsubst %,5_%,${INTCOUNTRIES})

#precursors: $(patsubst %,${DATADIR}/interventions/cases_sceni_%.rds,${UNMITSCENARIOS})

#${DATADIR}/interventions/cases_sceni_%.rds: covidm_interventions_run_scenario.R helper_functions.R
#	time Rscript $< ${COVIDMPATH} $(filter-out $<,$^) $(subst _, ,$*) ${INTINPUTDIR} $@

# consolidate.R actually consolidates everything
#consolidate: ${DATADIR}/interventions/Uganda_consolidated.rds

# TODO: also rm the now-consolidated files?
#${DATADIR}/interventions/%_consolidated.rds: consolidate.R
#	time Rscript $^ $(@D)


#cleanrds:
#	rm -f *-res.rds
