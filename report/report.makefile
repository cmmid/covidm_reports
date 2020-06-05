
repsetup: report/LMICreps.txt

report/LMICreps.txt:
	@cd report && make LMICreps.txt

REPS := $(addprefix ${INTINPUTDIR}/,$(shell cat report/LMICreps.txt))

allreps: ${REPS}

testrep: CPV/report.pdf

%/report.pdf: FORCE
	@cd report && make $@

FORCE: