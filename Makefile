SD = scripts
FD = figures
DD = data
CD = .cache
CMD = R CMD BATCH --no-save
SCR = Rscript -e


all: $(DD)/balanced_mean.csv $(DD)/inter_exp.csv $(FD)/joindist.pdf $(DD)/balanced_mean.csv $(FD)/va_decade.pdf $(DD)/coxme_pre_match.csv $(FD)/va_map.png $(DD)/match_tab.csv $(DD)/cox_tab.csv $(DD)/nn_tab.csv $(FD)/year_p.png

clean:
	-rm -f *.Rout
	-rm -f *.pdf

cleanall: 
	-rm -f *.Rout
	-rm -f $(DD)/*
	-rm -f $(FD)/*
	-rm -f $(CD)/*

$(DD)/balanced_mean.csv: $(SD)/balance-mean.R $(CD)/matched_data.rda
	$(CMD) $(SD)/balance-mean.R

$(FD)/joindist.png: $(SD)/distribution-matched.R $(CD)/matched_data.rda
	$(CMD) $(SD)/distribution-matched.R

$(DD)/match_tab.csv: $(SD)/post-psm.R $(CD)/matched_data.rda
	$(CMD) $(SD)/post-psm.R

$(CD)/matched_data.rda: $(SD)/matching.R $(CD)/psm_data.rda
	$(CMD) $(SD)/matching.R

$(CD)/psm_data.rda: $(SD)/pre-psm.R
	$(CMD) $(SD)/pre-psm.R

$(DD)/coxme_pre_match.csv: $(SD)/format_coxme_pre.R $(CD)/coxme_pre_match.rda
	$(CMD) $(SD)/format_coxme_pre.R

$(FD)/va_decade.pdf: $(SD)/vis_va_decade.R $(CD)/coxme_exp_model.rda
	$(CMD) $(SD)/vis_va_decade.R

$(DD)/inter_exp.csv: $(SD)/format_exp_model.R $(CD)/coxme_exp_model.rda
	$(CMD) $(SD)/format_exp_model.R

$(CD)/coxme_exp_model.rda: $(SD)/coxme_exp.R $(CD)/exp_data.rda
	$(CMD) $(SD)/coxme_exp.R

$(CD)/coxme_pre_match.rda: $(SD)/coxme_pre_match.R $(CD)/ehd_spells.rda
	$(CMD) $(SD)/coxme_pre_match.R
	
$(CD)/exp_data.rda: $(SD)/pre-exp-model.R $(CD)/ehd_spells.rda
	$(CMD) $(SD)/pre-exp-model.R

$(CD)/ehd_spells.rda: $(SD)/ehd_spells.R 
	$(CMD) $(SD)/ehd_spells.R

$(FD)/va_map.png: $(SD)/spatial-partition.R
	$(CMD) $(SD)/spatial-partition.R