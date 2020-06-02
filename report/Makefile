
REPDIR ?= ~/Dropbox/Covid_LMIC/Country_Modelling_Reports
DATADIR ?= ~/Dropbox/covidm_reports
INTINPUTDIR := ${DATADIR}/hpc_inputs
HPCDIR ?= ~/Dropbox/covidm_hpc_output

LMICreps.txt: ../setup/LMIC.txt
	sed "s/$$/\/report\.pdf/" $^ > $@

${REPDIR}/%/report.pdf: report.R ${HPCDIR}/%/001.qs ${HPCDIR}/%/alls.qs \
${HPCDIR}/%/accs.qs ${HPCDIR}/%/peak.qs \
template.Rmd $(addprefix ../plotting/,${PLOTREF}) COVID.bib
	mkdir -p $(@D)
	Rscript $(filter-out %.bib,$^) \
	${INTINPUTDIR} ${INTINPUTDIR}/../generation/data_contacts_missing.csv \
	$@

# custom report
${REPDIR}/IDP/report.pdf: report.R ${HPCDIR}/IDP/001.qs ${HPCDIR}/IDP/alls.qs \
${HPCDIR}/IDP/accs.qs ${HPCDIR}/IDP/peak.qs \
IDP.Rmd ../utils/sharedconst.rda ../plotting/plotfuns.rda COVID.bib
	mkdir -p $(@D)
	Rscript $(filter-out %.bib,$^) \
	${INTINPUTDIR} ${INTINPUTDIR}/../generation/data_contacts_missing.csv \
	$@


%/report.pdf: report.R ../simulate/%/001.qs ../simulate/%/alls.qs \
../simulate/%/accs.qs ../simulate/%/peak.qs \
template.Rmd ../utils/sharedconst.rda ../plotting/plotfuns.rda COVID.bib
	mkdir -p $(@D)
	Rscript $(filter-out %.bib,$^) \
	${INTINPUTDIR} ${INTINPUTDIR}/../generation/data_contacts_missing.csv \
	$@