# include this to add all setup actions
# this will attempt to update everything via runsetup
# to see more easily if anything is error-ing / what's updating:
# make -k runsetup | grep -v 'is up to date'

simulate/LMIC%.txt:
	@cd simulate && make $(@F)

%.qs: FORCE
	@cd simulate && make $@

FORCE:

SCENMAX := $(shell Rscript -e "cat(tail(readRDS('${INTINPUTDIR}/scenarios_overview.rds')[['index']], 1))")
ALLSCENIDS := $(shell seq -f%03g 2 ${SCENMAX})

testunmit: CPV/001.qs

testallqs: $(patsubst %,CPV/%.qs,${ALLSCENIDS})

testdigest: CPV/peak.qs
