# Info:

These are the codes used in the article _Pizzol M, Scotti M, Identifying marginal supplying countries of wood products via trade network analysis, The International Journal of Life Cycle Assessment (2016) DOI: 10.1007/s11367-016-1222-6_

The [publisher version](http://link.springer.com/article/10.1007/s11367-016-1222-6) and supplementary material (SM) are available at the IJLCA website, a free [preprint version](https://www.researchgate.net/publication/309890664_Identifying_marginal_supplying_countries_of_wood_products_via_trade_network_analysis) is available on my Researchgate profile. All material is available on email request (massimo@plan.aau.dk).

# The repository includes:

* _Forestry_Trade_Flows_E_All_Data_reduced.csv_ is a reduced sample of the **trade data**, i.e. trade of _Sawnwood (C)_. If you want to reproduce all results, you need to download the full data from FAOSTAT [here](http://faostat3.fao.org/download/F/FT/E) (select everything and press the .csv icon) and check that they are in the same format as in the example provided here
* _Forestry_E_All_Data_reduced.csv_ is a reduced sample of the **production data**, i.e. production of _Sawnwood (C)_. If you want to reproduce all results, you need to download the full data from FAOSTAT [here](http://faostat3.fao.org/download/F/FO/E) (select everything and press the .csv icon) and check that they are in the same format as in the example provided here
* _Demo.R_ is a R script **demo** where you can play around with [igraph](http://igraph.org/redirect.html) package on the wood trade data
* _Historical_increments.R_ is a R script for calculating **historical increments** from time series of production data
* _Countries_selection.txt_ a list of countries
* _Countries_selection_ROW.txt_ a list of world regions

# How to run this:
#### The next three scripts has to be run in order. The first two take quite some time to run...so if you are only interested in their output I suggest you retrieve the article's SM.
* _1_Wood_trade_network_loop_100.R_ is a R script for **identifying communities** and looping them. You will need the full FAOSTAT trade data see above to make this work.
* _2_Wood_trade_network_clusters_to_contingency.R_ is the R script to build the **contingency tables**.
* _3_Wood trade network_figures.R_ is a script used to generate **graphics**. You can load it using the SM thus skipping 1 and 2.
