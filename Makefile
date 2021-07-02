.PHONY: all clean taskflow wsubdir copylatex
.DELETE_ON_ERROR:

# ------------------------
# Variables

out_data = dataset/output/data.csv dataset/dataset.Rout
out_desc = descriptives/desc.Rout
out_lm = lm/lm.Rout
out_robust = lm_robust/robust.Rout
out_alt = alt_exp/alt.Rout

# ------------------------
# Main recipes

all: wsubdir $(out_data) $(out_desc) analyses taskflow copylatex

analyses: $(out_lm) $(out_robust) $(out_alt) #copylatex

clean:
	rm -rvf $(out_data) $(out_desc) $(out_lm) $(out_robust) $(out_alt)
	rm -rvf */output/*

taskflow:
	Rscript --no-save --verbose taskflow/create_dependency_graph.R
	dot -Grankdir=LR -Tpdf taskflow/dependency_list.txt -o taskflow/workflow.pdf
	sips -s format jpeg taskflow/workflow.pdf --out taskflow/workflow.jpeg

wsubdir:
	mkdir -p writing/tab writing/img

copylatex: analyses
	cp */output/*.pdf writing/img/
	cp */output/*.tex writing/tab/

# ------------------------
# Data

# NOTE just download input data, unzip etc -- no dataset creation for now

# NOTE EXAMPLE FROM CUARTELES
# input_data/cuarteles.csv input_data/secc_censal_renta.csv input_data/secc_censal_indic_demograficos.csv: | input_data
# 	curl -L -O https://github.com/franvillamil/franvillamil.github.io/raw/master/files/input_cuarteles_militares.zip
# 	unzip input_cuarteles_militares.zip
# 	rm input_cuarteles_militares.zip
# 	mv *.csv input_data

$(out_data): dataset/dataset.R input/munilist.csv input/ciidh.csv input/ceh_massacres_80_85.csv input/terrain_vars.csv input/census_73_81.csv input/elections_1999-2015.csv
	mkdir -p $(@D)/output
	Rscript --no-save --verbose $< 2>&1 | tee $<out

# ------------------------
# Descriptives & analyses

$(out_desc): descriptives/desc.R dataset/output/data.csv input/GTM_adm2_updated.shp input/caminos_gtm.shp input/panamericana.shp input/elections_1999-2015.csv
	mkdir -p $(@D)/output
	Rscript --no-save --verbose $< 2>&1 | tee $<out
	pdfcrop descriptives/output/corrplot.pdf descriptives/output/corrplot.pdf

$(out_lm): lm/lm.R func/predprob_df.R func/my_stargazer.R dataset/output/data.csv
	mkdir -p $(@D)/output
	Rscript --no-save --verbose $< 2>&1 | tee $<out

$(out_robust): lm_robust/robust.R func/predprob_df.R func/my_stargazer.R dataset/output/data.csv
	mkdir -p $(@D)/output
	Rscript --no-save --verbose $< 2>&1 | tee $<out

$(out_alt): alt_exp/alt.R func/predprob_df.R func/my_stargazer.R dataset/output/data.csv
	mkdir -p $(@D)/output
	Rscript --no-save --verbose $< 2>&1 | tee $<out

# ------------------------
# Latex

# cp */output/*.pdf writing/img/
# cp */output/*.tex writing/tab/
