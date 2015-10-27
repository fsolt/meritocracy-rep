all: doc/paper/avoid_master.pdf

clean :
	-rm edit $(objects)

vpath %.tex doc/paper
vpath %.pdf doc/figures
vpath %.R R

doc/figures/07_plot_interaction_terms_t1m1.pdf: 07_plot_interaction_terms.R data/study_26584/Meritocracy\ Replication\ Data\ -\ Table\ 1.tab
	Rscript $<

doc/paper/avoid_master.pdf: avoid_master.tex\
	  07_plot_interaction_terms.tex\
	  07_plot_interaction_terms_t1m1.pdf
	cd doc/paper;\
	  pdflatex avoid_master.tex;\
	  bibtex avoid_master.tex;\
	  pdflatex avoid_master.tex;\
	  pdflatex avoid_master.tex