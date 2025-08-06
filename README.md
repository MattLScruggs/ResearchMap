# ResearchMap
MPH Capstone project for mapping research gaps in public health and epidemiology.

## Overview
This project applies NLP to peer reviewed journal abstracts to identify areas of research that are underexplored, unresolved, or both. \
Research abstracts used come from the CORD-19 dataset, found [here](https://github.com/allenai/cord19). \

"Lit review functions, final" is a set of algorithms for NLP processing and embedding. \

"Recent CORD-19 studies" pulls and formats abstracts of studies from the CORD-19 dataset, based on date of publication. \
Studies used for this analysis are dated Jan-Mar, 2020.

"Embedding Research" parses the text and creates a TF-IDF matrix, with each row representing an abstract, and each column a word, \
with the correspoding value being the TF-IDF score for the corresponding word and abstract.

