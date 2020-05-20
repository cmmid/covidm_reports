# include this to add all setup actions
# this will attempt to update everything via runsetup
# to see more easily if anything is error-ing / what's updating:
# make -k runsetup | grep -v 'is up to date'

SETUPFILES := $(patsubst %,setup/%.txt,LMICparams_set LMICinits LMICcontact_matrices)

setupfiles: ${SETUPFILES}

cleansetup:
	rm -f ${SETUPFILES}

simulate/LMIC%.txt:
	make -C $(@D) $(@F)

REPS := $(addprefix ${INTINPUTDIR}/,$(shell cat setup/LMICreps.txt))

runsetup: allinits allparamsets allcontactmatrices

%/timing.rds: FORCE
	@cd simulate && make $@

%/contact_matrices.rds: FORCE
	@cd simulate && make $@

%/params_set.rds: FORCE
	@cd simulate && make $@

FORCE:
