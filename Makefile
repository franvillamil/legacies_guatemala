.PHONY: all clean taskflow
.DELETE_ON_ERROR:

# ------------------------
# Variable (file groups)

out_lm = lm/output/pp_FRG_panam.pdf lm/output/pp_FRG_roads.pdf lm/output/pp_URNG_panam.pdf lm/output/pp_URNG_roads.pdf lm/output/tab_lm_panam.tex lm/output/tab_lm_roads.tex lm/output/tab_lm_base.tex

out_robust = lm_robust/output/pp_FRG_panam_year.pdf lm_robust/output/pp_FRG_roads_year.pdf lm_robust/output/pp_URNG_panam_year.pdf lm_robust/output/pp_URNG_roads_year.pdf lm_robust/output/tab_FRG_panam_year.tex lm_robust/output/tab_URNG_panam_year.tex lm_robust/output/tab_FRG_roads_year.tex lm_robust/output/tab_URNG_roads_year.tex lm_robust/output/tab_FRG_base_year.tex lm_robust/output/tab_URNG_base_year.tex lm_robust/output/pp_fulldcha_panam.pdf lm_robust/output/pp_fulldcha_roads.pdf lm_robust/output/tab_fulldcha.tex lm_robust/output/tab_lm_panam_ceh.tex lm_robust/output/tab_lm_roads_ceh.tex

out_alt = alt_exp/output/tab_rebels_vi.tex alt_exp/output/tab_rebels_vi_hv.tex alt_exp/output/tab_govt_vi_hv.tex alt_exp/output/tab_govt_vi.tex

# ------------------------
# Main recipes

all: $(out_lm) $(out_robust) $(out_alt) taskflow

clean:
	rm -rvf $(out_lm) $(out_robust) $(out_alt)

taskflow:
	Rscript --no-save --verbose taskflow/create_dependency_graph.R
	dot -Grankdir=LR -Tpdf taskflow/dependency_list.txt -o taskflow/workflow.pdf
	sips -s format jpeg taskflow/workflow.pdf --out taskflow/workflow.jpeg

# ------------------------
# Analyses

$(out_lm): lm/lm.R func/predprob_df.R func/my_stargazer.R data/output/data.csv
	mkdir -p $(@D)
	Rscript --no-save --verbose $< > $<out
	mkdir -p writing/tab writing/img
	cp lm/output/*.tex writing/tab/
	cp lm/output/*.pdf writing/img/

$(out_robust): lm_robust/robust.R func/predprob_df.R func/my_stargazer.R data/output/data.csv
	mkdir -p $(@D)
	Rscript --no-save --verbose $< > $<out
	mkdir -p writing/tab writing/img
	cp lm_robust/output/*.tex writing/tab/
	cp lm_robust/output/*.pdf writing/img/

$(out_alt): alt_exp/alt.R func/predprob_df.R func/my_stargazer.R data/output/data.csv
	mkdir -p $(@D)
	Rscript --no-save --verbose $< > $<out
	mkdir -p writing/tab #writing/img
	cp alt_exp/output/*.tex writing/tab/
	# cp alt_exp/output/*.pdf writing/img/
