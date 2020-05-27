# include this to add all setup actions
# this will attempt to update everything via runsetup
# to see more easily if anything is error-ing / what's updating:
# make -k runsetup | grep -v 'is up to date'

simulate/LMIC%.txt:
	@cd simulate && make $(@F)

SIMRUNS ?= 2

%.qs: FORCE
	@SIMRUNS=${SIMRUNS} cd simulate && make $@

FORCE:

SCENMAX := $(shell Rscript -e "cat(max(readRDS('${INTINPUTDIR}/alt_scenarios.rds')[['scen_id']]))")
ALLSCENIDS := $(shell seq -f%03g 2 ${SCENMAX})

TESTISO ?= CPV

testunmit: ${TESTISO}/001.qs

testallqs: $(patsubst %,${TESTISO}/%.qs,${ALLSCENIDS})

testdigest: ${TESTISO}/peak.qs

testclean:
	rm simulate/${TESTISO}/*.qs
