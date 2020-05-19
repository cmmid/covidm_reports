
DBDIR ?= ~/Dropbox
SYDIR ?= ~/Sync

OUTDIR := ${DBDIR}/covidm_hpc_output
REPDIR := ${DBDIR}/Covid_LMIC/Country_Modelling_Reports
SREPDIR := ${SYDIR}/COVID_projections_reports

DATE ?= $(shell date +%Y_%m_%d)

version: ${OUTDIR}/${DATE}_archive.tar.gz ${REPDIR}/${DATE}_archive.tar.gz

zipcsvs: ${OUTDIR}/${DATE}_csvs.tar.gz

mirror:
	cd ${REPDIR}; rsync -avr --exclude=old --exclude=*/.DS_Store . ${SREPDIR}

cleancsvs:
	cd ${OUTDIR}; find . -type f -name '*.csv' -exec rm {} \;

# call as: make update DLDIR=path/to/newdata
update:
	cd ${DLDIR}; rsync -avr . ${OUTDIR}

${OUTDIR}/${DATE}_archive.tar.gz: | ${OUTDIR}
	cd $|; find . -path ./old -prune -o -name "*.qs" -print | xargs tar -czvf $@

${OUTDIR}/${DATE}_csvs.tar.gz: | ${OUTDIR}
	find $| -name "*.csv" | xargs tar -czvf $@

${REPDIR}/${DATE}_archive.tar.gz: | ${REPDIR}
	cd $|; find . -path ./old -prune -o -name "*.pdf" -print | xargs tar -czvf $@