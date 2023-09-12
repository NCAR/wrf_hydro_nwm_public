SPHINXOPTS  = -c .
SPHINXBLD   = sphinx-build
SPHINXPROJ  = wrfhydro
DOCDIR      = doc/
DOCBACK     = ../
DCXFROMDOC  = ../
BLDDIR      = build/doc/
STPLS       = $(wildcard $(DOCDIR)*.stpl)
STPLTGTS    = $(STPLS:%.stpl=%)
SRCS        = $(filter-out $(DOCDIR)index.rest,$(wildcard $(DOCDIR)*.rest))
SRCSTPL     = $(wildcard $(DOCDIR)*.rest.stpl)
IMGS        = \
	$(wildcard $(DOCDIR)*.pyg)\
	$(wildcard $(DOCDIR)*.eps)\
	$(wildcard $(DOCDIR)*.tikz)\
	$(wildcard $(DOCDIR)*.svg)\
	$(wildcard $(DOCDIR)*.uml)\
	$(wildcard $(DOCDIR)*.dot)\
	$(wildcard $(DOCDIR)*.eps.stpl)\
	$(wildcard $(DOCDIR)*.tikz.stpl)\
	$(wildcard $(DOCDIR)*.svg.stpl)\
	$(wildcard $(DOCDIR)*.uml.stpl)\
	$(wildcard $(DOCDIR)*.dot.stpl)
PNGS=$(subst $(DOCDIR),$(DOCDIR)_images/,\
	$(patsubst %.eps,%.png,\
	$(patsubst %.pyg,%.png,\
	$(patsubst %.tikz,%.png,\
	$(patsubst %.svg,%.png,\
	$(patsubst %.uml,%.png,\
	$(patsubst %.dot,%.png,\
	$(patsubst %.eps.stpl,%.png,\
	$(patsubst %.dot.stpl,%.png,\
	$(patsubst %.tikz.stpl,%.png,\
	$(patsubst %.svg.stpl,%.png,\
	$(patsubst %.uml.stpl,%.png,$(IMGS)))))))))))))
DOCXS = $(subst $(DOCDIR),$(BLDDIR)docx/,$(SRCS:%.rest=%.docx))\
	$(subst $(DOCDIR),$(BLDDIR)docx/,$(SRCSTPL:%.rest.stpl=%.docx))
PDFS  = $(subst $(DOCDIR),$(BLDDIR)pdf/,$(SRCS:%.rest=%.pdf))\
	$(subst $(DOCDIR),$(BLDDIR)pdf/,$(SRCSTPL:%.rest.stpl=%.pdf))
.PHONY: docx help Makefile docxdir pdfdir stpl index imgs
stpl: $(STPLTGTS)
%:%.stpl
	@cd $(DOCDIR) && stpl "$(<F)" "$(@F)"
imgs: $(PNGS)
$(DOCDIR)_images/%.png:$(DOCDIR)%.pyg
	@cd $(DOCDIR) && python $(DCXFROMDOC)dcx.py $(<F)
$(DOCDIR)_images/%.png:$(DOCDIR)%.eps
	@cd $(DOCDIR) && python $(DCXFROMDOC)dcx.py $(<F)
$(DOCDIR)_images/%.png:$(DOCDIR)%.tikz
	@cd $(DOCDIR) && python $(DCXFROMDOC)dcx.py $(<F)
$(DOCDIR)_images/%.png:$(DOCDIR)%.svg
	@cd $(DOCDIR) && python $(DCXFROMDOC)dcx.py $(<F)
$(DOCDIR)_images/%.png:$(DOCDIR)%.uml
	@cd $(DOCDIR) && python $(DCXFROMDOC)dcx.py $(<F)
$(DOCDIR)_images/%.png:$(DOCDIR)%.dot
	@cd $(DOCDIR) && python $(DCXFROMDOC)dcx.py $(<F)
docxdir: ${BLDDIR}docx
pdfdir: ${BLDDIR}pdf
MKDIR_P = mkdir -p
${BLDDIR}docx:
	@${MKDIR_P} ${BLDDIR}docx
${BLDDIR}pdf:
	@${MKDIR_P} ${BLDDIR}pdf
index:
	@cd $(DOCDIR) && python $(DCXFROMDOC)dcx.py
help:
	@$(SPHINXBLD) -M help "$(DOCDIR)" "$(BLDDIR)" $(SPHINXOPTS) $(O)
	@echo "  docx        to docx"
	@echo "  pdf         to pdf"
#http://www.sphinx-doc.org/en/stable/usage/builders/
html dirhtml singlehtml htmlhelp qthelp applehelp devhelp epub latex text man texinfo pickle json xml pseudoxml: Makefile index stpl imgs
	@$(SPHINXBLD) -M $@ "$(DOCDIR)" "$(BLDDIR)" $(SPHINXOPTS) $(O)
docx:  docxdir index stpl imgs $(DOCXS)
$(BLDDIR)docx/%.docx:$(DOCDIR)%.rest
pdf: pdfdir index stpl imgs $(PDFS)
$(BLDDIR)pdf/%.pdf:$(DOCDIR)%.rest
