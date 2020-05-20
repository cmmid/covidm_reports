
LMICreps.txt: LMIC.txt
	sed "s/$$/\/report\.pdf/" $^ > $@

${HPCDIR}/%/peak.qs: digest.R
	Rscript $^ $@

${HPCDIR}/%/accs.qs ${HPCDIR}/%/alls.qs: ${HPCDIR}/%/peak.qs

${HPCDIR}/%.qs: run_scenarios.R helper_functions.R
	mkdir -p $(@D)
	Rscript $^ ${COVIDMPATH} $(subst /, ,$*) ${INTINPUTDIR} $@

testqs1: ${HPCDIR}/palestine/001.qs

testqs: $(patsubst %,${HPCDIR}/palestine/%.qs,$(shell seq -f%03g 2 51))

tmpdig: ${HPCDIR}/palestine/peak.qs

# TODO: this depends on the all countries, peak|accs|001.qs
${HPCDIR}/summary.tar.gz:
	cd $(@D) && tar -czvf $(@F) */peak.qs */accs.qs */alls.qs */001.qs
