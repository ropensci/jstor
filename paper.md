---
title: 'jstor: Extract meta-data from DfR/JSTOR'
authors:
 - affiliation: 1
   name: Thomas Klebel
   orcid: 0000-0002-7331-4751
date: 2018-02-08
output:
  html_document:
    df_print: paged
bibliography: paper.bib
tags:
 - JSTOR
 - DfR
 - text mining
 - text analysis
 - citation analysis
affiliations:
 - index: 1
   name: University of Graz
---

# Summary
The interest in the (quantitative) analysis of textual data has increased
sharply over the last decades [@roberts_introduction_2016]. For researchers
interested in the study of science and the scholarly literature in particular,
the archival content of [JSTOR](http://www.jstor.org) offers a rich and diverse
set of journals and articles. The service 
[Data for Research (DfR)](http://www.jstor.org/dfr/) by JSTOR gives all 
researchers, regardless of whether they have access to JSTOR or not, the
opportunity to analyse this rich dataset, by delivering metadata,
n-grams and, upon special request, fulltext materials about all available
articles and books from JSTOR. The package `jstor` [@jstor] helps in
analysing these datasets by enabling researchers to easily import the metadata
to R [@r_core],
which can either be analysed on its own, or be used in conjunction with n-grams
or fulltext-data. Commonly, the metada include information
on the articles' authors, the title, journal, date of publishing, and quite
frequently all footnotes and references. All this information can be of interest
for specific research questions. For the analysis of n-grams or fulltexts,
the metadata imported with `jstor`
allow the researchers to
filter articles based on specific journals, the dates of publication, the
authors, keywords in titles and other aspects.

Work on `jstor` benefited from financial support for the project "Academic
Super-Elites in Sociology and Economics" by the Austrian Science Fund (FWF), 
project number "P 29211 Einzelprojekte". `jstor` is currently being used by
the project team to analyse academic elites in sociology and economics.



# References
