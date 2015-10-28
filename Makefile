all: doc/paper/avoid_master.pdf

clean :
	-rm doc/paper/avoid_master.pdf

vpath %.tex doc/paper
vpath %.pdf doc/figures
vpath %.R R

doc/figures/07_plot_interaction_terms_t1m1.pdf: 07_plot_interaction_terms.R data/study_26584/Meritocracy\ Replication\ Data\ -\ Table\ 1.tab
	Rscript $<

doc/paper/avoid_master.pdf: avoid_master.tex\
	  00_introduction.tex\
	  01_ensure_reproducibility.tex\
	  02_code_in_public.tex\
	  03_examine_all_available_data.tex\
	  04_use_consistent_measures.tex\
	  05_recode_with_care.tex\
	  06_multiply_impute.tex\
	  07_plot_interaction_terms.tex\
	  07_plot_interaction_terms_t1m1.pdf
	cd doc/paper;\
	  pdflatex avoid_master.tex;\
	  bibtex avoid_master.tex;\
	  pdflatex avoid_master.tex;\
	  pdflatex avoid_master.tex
