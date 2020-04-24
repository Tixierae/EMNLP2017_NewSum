## Combining graph degeneracy and submodularity for unsupervised extractive summarization
This repository contains the code of the EMNLP 2017 workshop paper **Combining graph degeneracy and submodularity for unsupervised extractive summarization**. [[link to paper]](https://www.researchgate.net/publication/322587935_Combining_Graph_Degeneracy_and_Submodularity_for_Unsupervised_Extractive_Summarization.) [[ACL Anthology link]](https://www.aclweb.org/anthology/W17-4507/)

> We present a fully unsupervised, extractive text summarization system that leverages a submodularity framework introduced by past research. The framework allows summaries to be generated in a greedy way while preserving near-optimal performance guarantees. Our main contribution is the novel coverage reward term of the objective function optimized by the greedy algorithm. This component builds on the graph-of-words representation of text and the k-core decomposition algorithm to assign meaningful scores to words. We evaluate our approach on the AMI and ICSI meeting speech corpora, and on the DUC2001 news corpus. We reach state-of-the-art performance on all datasets. Results indicate that our method is particularly well-suited to the meeting domain.

### Web app
The system can be tested with this interactive web app: https://safetyapp.shinyapps.io/meet_summ_proto/

### Command line script
A script that can be run from the command line/terminal can be found here: https://github.com/linto-project/linto-sp5/blob/master/local_directory/offline_exe.R#L29

### Cite
If you use some of the code in this repo in your own work, of if you just want to refer to our paper, please cite:

```BibTeX
@inproceedings{tixier2017combining,
  title={Combining graph degeneracy and submodularity for unsupervised extractive summarization},
  author={Tixier, Antoine and Meladianos, Polykarpos and Vazirgiannis, Michalis},
  booktitle={Proceedings of the workshop on new frontiers in summarization},
  pages={48--58},
  year={2017}
}
```
