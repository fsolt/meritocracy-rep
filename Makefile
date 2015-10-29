all: doc/paper/avoid.pdf

clean :
	-rm doc/paper/avoid.pdf

vpath %.tex doc/paper
vpath %.pdf doc/figures
vpath %.R R

doc/figures/03_examine_all_available_data_t2.pdf: 03_examine_all_available_data.R 
	Rscript $<

doc/figures/03_examine_all_available_data_t2_by_survey.pdf: 03_examine_all_available_data.R 
	Rscript $<

doc/figures/07_plot_interaction_terms_t1m1.pdf: 07_plot_interaction_terms.R
	Rscript $<

doc/paper/avoid.pdf: avoid.tex\
	  00_introduction.tex\
	  01_ensure_reproducibility.tex\
	  02_work_in_public.tex\
	  03_examine_all_available_data.tex\
	  03_examine_all_available_data_t2.pdf\
	  03_examine_all_available_data_t2_by_survey.pdf\
	  04_use_consistent_measures.tex\
	  05_recode_with_care.tex\
	  06_multiply_impute.tex\
	  07_plot_interaction_terms.tex\
	  07_plot_interaction_terms_t1m1.pdf
	cd doc/paper;\
	  pdflatex avoid.tex;\
	  bibtex avoid.tex;\
	  pdflatex avoid.tex;\
	  pdflatex avoid.tex
