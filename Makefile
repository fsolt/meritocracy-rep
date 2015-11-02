all: doc/paper/avoid.pdf

clean :
	cd doc/paper; rm avoid.pdf *.aux *.bbl *.blg *.fls *.log *.out *latexmk 

vpath %.tex doc/paper:doc/figures
vpath %.pdf doc/figures
vpath %.R R

03_examine_all_available_data_t2.pdf: 03_examine_all_available_data.R 
	Rscript --verbose $<

03_examine_all_available_data_t2_by_survey.pdf: 03_examine_all_available_data.R 
	Rscript --verbose $<

doc/figures/05_wrangle_with_care_bush_al.tex: 05_wrangle_with_care.R
	Rscript $<;\
	sed -e s/ccc/lcc/ doc/figures/05_wrangle_with_care_bush_al.tex > 05.tmp &&\
	mv 05.tmp doc/figures/05_wrangle_with_care_bush_al.tex

07_plot_interaction_terms_t1m1.pdf: 07_plot_interaction_terms.R
	Rscript $<

doc/paper/avoid.pdf: avoid.tex\
	  00_introduction.tex\
	  01_ensure_reproducibility.tex\
	  02_work_in_public.tex\
	  03_examine_all_available_data.tex\
	  03_examine_all_available_data_t2.pdf\
	  03_examine_all_available_data_t2_by_survey.pdf\
	  04_use_consistent_measures.tex\
	  05_wrangle_with_care.tex\
	  05_wrangle_with_care_bush_al.tex\
	  06_multiply_impute.tex\
	  07_plot_interaction_terms.tex\
	  07_plot_interaction_terms_t1m1.pdf
	cd doc/paper;\
	  latexmk -pdf avoid.tex;\
	  open -a Preview avoid.pdf
 
.PHONY: all clean