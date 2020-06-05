
REPDIR ?= ~/Dropbox/Covid_LMIC/Country_Modelling_Reports
DATADIR ?= ~/Dropbox/covidm_reports
INTINPUTDIR := ${DATADIR}/hpc_inputs
HPCDIR ?= ~/Dropbox/covidm_hpc_output

LMICreps.txt: ../setup/LMIC.txt
	sed "s/$$/\/report\.pdf/" $^ > $@

figshared.rda: figshared.R ../utils/sharedconst.rda
	Rscript $^ $@

tabshared.rda: tabshared.R
	Rscript $^ $@


#${REPDIR}/%/report.pdf: report.R ${HPCDIR}/%/001.qs ${HPCDIR}/%/alls.qs \
#${HPCDIR}/%/accs.qs ${HPCDIR}/%/peak.qs \
#template.Rmd $(addprefix ../plotting/,${PLOTREF}) COVID.bib
#	mkdir -p $(@D)
#	Rscript $(filter-out %.bib,$^) \
#	${INTINPUTDIR} ${INTINPUTDIR}/../generation/data_contacts_missing.csv \
#	$@

.PRECIOUS: ${REPDIR}/%/figs/fig1.png ${REPDIR}/%/figs/fig2a.png ${REPDIR}/%/figs/fig2b.png ${REPDIR}/%/figs/fig3a.png ${REPDIR}/%/figs/fig3b.png

#${REPDIR}/%/report.pdf: ${REPDIR}/%/figs/fig1.png ${REPDIR}/%/figs/fig2a.png ${REPDIR}/%/figs/fig2b.png ${REPDIR}/%/figs/fig3a.png ${REPDIR}/%/figs/fig3b.png
#	echo temporary

${REPDIR}/%/figs/fig1.png: fig_unmitigated.R ${HPCDIR}/%/alls.qs ${HPCDIR}/%/peak.qs \
${INTINPUTDIR}/%/timing.rds figshared.rda
	mkdir -p $(@D)
	Rscript $^ $@

${REPDIR}/%/figs/fig2a.png ${REPDIR}/%/figs/fig2b.png: fig_interventions.R ${HPCDIR}/%/alls.qs ${HPCDIR}/%/peak.qs \
${INTINPUTDIR}/%/timing.rds figshared.rda
	mkdir -p $(@D)
	Rscript $^ $@

${REPDIR}/%/figs/fig3a.png ${REPDIR}/%/figs/fig3b.png: fig_totals.R ${HPCDIR}/%/alls.qs ${HPCDIR}/%/accs.qs \
${INTINPUTDIR}/%/timing.rds figshared.rda
	mkdir -p $(@D)
	Rscript $^ $@

# custom report
#${REPDIR}/IDP/report.pdf: report.R ${HPCDIR}/IDP/001.qs ${HPCDIR}/IDP/alls.qs \
#${HPCDIR}/IDP/accs.qs ${HPCDIR}/IDP/peak.qs \
#IDP.Rmd ../utils/sharedconst.rda ../plotting/plotfuns.rda COVID.bib
#	mkdir -p $(@D)
#	Rscript $(filter-out %.bib,$^) \
#	${INTINPUTDIR} ${INTINPUTDIR}/../generation/data_contacts_missing.csv \
#	$@

%/report.pdf: report-rev.R template-rev.Rmd figshared.rda tabshared.rda COVID.bib
	mkdir -p $(@D) && cd $(@D) && \
	Rscript $(addprefix ${CURDIR}/,$(filter-out %.bib,$^)) \
	${INTINPUTDIR} ${INTINPUTDIR}/../generation/data_contacts_missing.csv \
	$(notdir $*) $(@F)

%/figs/fig1.png: fig_unmitigated.R ../simulate/%/alls.qs ../simulate/%/peak.qs \
${INTINPUTDIR}/%/timing.rds figshared.rda
	mkdir -p $(@D)
	Rscript $^ $@


