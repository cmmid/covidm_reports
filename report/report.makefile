
repsetup: report/LMICreps.txt

report/LMICreps.txt:
	@cd report && $(MAKE) $(@F)

REPS := $(addprefix ${REPDIR}/,$(shell cat report/LMICreps.txt))

allreps: ${REPS}

testrep: CPV/report.pdf

%/report.pdf: FORCE
	@cd report && $(MAKE) $@

FORCE: