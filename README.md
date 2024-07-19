# Political Islam as Discursive Practice: Reproducing Results

This repository provides the data and script necessary to reproduce the results of the article *Political Islam as Discursive Practice*. [1]  It also includes a shiny app as an accessible approach to try out the methods used in the article.

## Contents

- See the folder 'corpus' for all the documents of the corpus in .txt format. The corpus consists of founding statements and self descriptions of opposition groups in the Syrian Conflict. 
- The folder 'support_files' contains .csv files with meta data of the documents and organisations, as well as the lists of stop words, stemmed characters and English translations that were used.   
- The script 'islamism_as_discursive_practice.R' does the analysis. The text_mining and analysis follows the approach as set out by Lebart et al [2].
- At the end, the script includes an (optional) call to a interactive shiny application that includes all the tables, plots and texts that were referenced in the article.

## Getting Started
To get a local copy up and running do the following

### prerequisits 
[Install R](https://cloud.r-project.org/) on your system.

When working in VSCode, it is also recommended to install the [REditorSupport extension](https://marketplace.visualstudio.com/items?itemName=reditorsupport.r).

### Installation 
1. Clone the repository. 
    
```bash 
    git clone https://github.com/teijehidde/Islamism-as-Discursive-Practice 
```

2. Run the script. The script installs any missing dependencies automatically. 

## References 

- [1] Donker, Teije Hidde. 2021. ‘Political Islam as Discursive Practice: The Social Construction of Political Ideas in Contentious Episodes’. In *ECPR Joint Sessions - Hybrid Pathways to Resistance in the Muslim World: Islamist Groups and the Modern State in a Comparative Perspective*. European Consortium for Political Research. *The paper is available [here](https://teijehidde.files.wordpress.com/2021/09/ecpr-js-2021-paper.pdf).*
- [2] Lebart, Ludovic, A. Salem, and L. Berry. 1997. Exploring Textual Data. Springer Science & Business Media

