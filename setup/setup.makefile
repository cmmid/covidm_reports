# include this to add all setup actions
# this will attempt to update everything via runsetup
# to see more easily if anything is error-ing / what's updating:
# make -k runsetup | grep -v 'is up to date'

SETUPFILES := $(patsubst %,setup/%.txt,LMICparams_set LMICinits LMICcontact_matrices)

setupfiles: ${SETUPFILES} ${GENDIR}/data_contacts_missing.csv

cleansetup:
	rm -f ${SETUPFILES}

setup/LMIC%.txt: FORCE
	make -C $(@D) $(@F)

CMS := $(addprefix ${INTINPUTDIR}/,$(shell cat setup/LMICcontact_matrices.txt))
PSS := $(addprefix ${INTINPUTDIR}/,$(shell cat setup/LMICparams_set.txt))
INITS := $(addprefix ${INTINPUTDIR}/,$(shell cat setup/LMICinits.txt))

allcontactmatrices: ${CMS}

allparamsets: ${PSS}

allinits: ${INITS}

runsetup: allinits allparamsets allcontactmatrices ${INTINPUTDIR}/alt_scenarios.rds

%/timing.rds: FORCE
	@cd setup && make $@

%/contact_matrices.rds: FORCE
	@cd setup && make $@

%/params_set.rds: FORCE
	@cd setup && make $@

%/alt_scenarios.rds: FORCE
	@cd setup && make $@

FORCE:
