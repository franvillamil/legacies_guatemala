Replication files for '[Violence, co-optation, and postwar voting in Guatemala](https://nbviewer.org/github/franvillamil/legacies_guatemala/blob/master/writing/preprint.pdf)', by Francisco Villamil, forthcoming at [Conflict Management and Peace Science](https://journals.sagepub.com/home/cmp).

**Abstract:**

> Wartime civilian victimization produces a counter-reaction against the perpetrator. However, this effect hinges on the creation of collective memories of wartime events. In many countries, former fighting actors and political elites try to redirect memories of wartime events through denial, propaganda, and co-optation. Previous works have ignored these aspects. I argue that the effect of violence is conditional on the capacity of local communities to build collective memories and bypass those efforts. I test this argument using local-level data from Guatemala. Results show that the effects of state violence on postwar voting depend on prewar exposure to political mobilization.

### Instructions

The code in this repository is organized in tasks, each in one folder. Some tasks are dependent on the output of previous tasks:

<img src="taskflow/workflow.jpeg" width=50%>

In order to replicate the results, there are two options:

1. Run each `R` script separately. The final dataset can be found in `dataset/output/data.csv`, and to replicate the main results:
  * `lm/lm.R` produces the results in the main text.
  * `alt_exp/alt.R` and `lm_robust/robust.R` produce the suplementary analyses from the Appendix, and `descriptives/desc.R` creates the maps and descriptive graphs.

2. From the command line (Unix/macOS), you can do a full replication using `make`. If you want to remove all output files first and run it from scratch, run `make clean` followed by `make` (`R`, `git` and `pdfcrop` need be installed). To download the repository and do a full replication automatically:

```shell
git clone https://github.com/franvillamil/legacies_guatemala
cd legacies_guatemala
make clean
make
```
