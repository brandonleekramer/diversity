
### BERT Workflow 

My main goal is to use BERT to compare the terms diversity and race/ethnicity. I have already done this using Word2Vec and found that diversity was too polysemous to be captured using Word2Vec's algorithm. Thus, I moved to BERT because I read that BERT can account for this polysemy by creating multiple vectors of the term diversity, which can then be compared to the race and ethnicity vectors. Here are the steps I think I need to take:

    ├── Pick a BERT benchmark model
        ├── BioBERT 
        ├── BlueBERT
        
    ├── Conduct transfer learning with labeled data 
        ├── Dataframe would be n of abstracts labeled "social diversity" or "other diversity" 

    ├── 03_github-sectoring
        ├── Full sectoring approach
        ├── Academic 
        ├── Business
        ├── Government
        ├── Household
        ├── Non-Profit
        
### References 

[BlueBERT on GitHub](https://github.com/ncbi-nlp/bluebert)

[BlueBERT Paper](https://arxiv.org/pdf/1906.05474.pdf)

[BlueBERT on Medium](https://medium.com/@manasmohanty/ncbi-bluebert-ncbi-bert-using-tensorflow-weights-with-huggingface-transformers-15a7ec27fc3d)

[BioBERT Paper](https://academic.oup.com/bioinformatics/article/36/4/1234/5566506)

[BioBERT on GitHub](https://github.com/dmis-lab/biobert)

[Cohn's Systematic Review on BERT in Biomedicine](https://via.library.depaul.edu/cgi/viewcontent.cgi?article=1029&context=cdm_etd)

[BERT Fine Tuning with PyTorch](https://mccormickml.com/2019/07/22/BERT-fine-tuning/)

[Extracting Word Embeddings from BERT](https://mccormickml.com/2019/05/14/BERT-word-embeddings-tutorial/#3-extracting-embeddings)


