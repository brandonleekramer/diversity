---
title: "Hypothesis 4"
description: "Comparing semantic similarity of diversity-related terminology"
output: 
  html_document:
  self_contained: no
weight: 4
---



After discovering that diversity and its many analougues are rising, but race/ethnicity are stagnating or declining, we began to wonder whether there was a way to test whether diversity was replacing race/ethnicity in biomedical abstracts. Drawing on <ins>[Berrey's (2015)](https://www.youtube.com/watch?time_continue=96&v=AyZJdEfYUjw&feature=emb_logo)</ins> argument that diversity acts as a mechanism to whitewash attempts to materially address racial inequality in other contexts, we thought there was a possibility that something similar is happening in biomedicine. Below, we test whether is race, ethnicity and diversity are becoming more or less semantically similar over time. Moreover, we examine whether these terms are becoming more or less semantically similar to other diversity-related terminology over time as well. Understanding these trends should help us better contextualize how diversity is used and what the political implications of such shifts mean for diversity in biomedical research. 

<br>
<center> 
<span style="color:black"> H4: Diversity is replacing racial and ethnic terms in the context of <br> human population research spanning from 1990-2020. </span>
</center>
<br>

To do this, we used `word2vec` - a word embedding algorithm commonly used for comparing word similarities.

Explain word2vec here... 

<center> 

Supplementary Figure 3A: Visual Comparison of Top-10 Terms in 
Race, Ethnicity and Diversity Vectors from 1990-2000 Word2Vec Model

![](/results/hypothesis4_files/w2v_tsne_19902000.png)

Supplementary Figure 3B: Visual Comparison of Top-10 Terms in 
Race, Ethnicity and Diversity Vectors from 2010-2020 Word2Vec Model



</center>

Discussion 

<center> 


</center>


Supplementary Figures 2A and 2B visualize the top-10 terms in the race, racial, ethnic, ethnicity, diverse and diversity word vectors for the 1990-2000 and 2010-2020 Word2Vec models. To visualize these vectors, we used the t-Distributed Stochastic Neighbor Embedding function from Python’s scikit-learn package (Pedregosa et al. 2011), which is an algorithm used to decompose the model into 2 dimensions for plotting. This process allows us to evaluate the vectors relative to one another in a 2-dimensional scatterplot. Above, we find that diversity moves further away from the racial and ethnic vectors over time.

Discussion







