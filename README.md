# Analysis of the expression of ephrin proteins and their receptors
Recent studies indicate the role of ephrin proteins and their receptors in the processes affecting the progression of melanoma. Overexpression of genes encoding Eph receptor tyrosine kinases and their ephrin ligands is often found in neoplastic diseases, including melanomas. However, there are no reports on the expression of these proteins in normal tissue, such as skin or nevus, in relation to primary melanoma that is not in the metastatic stage.

## Purpose of work
The aim of this study was to verify whether the expression of ephrin receptors differs between healthy skin, benign nevus and primary melanoma. It was made on the basis of statistical analysis of data from microarrays available in the Gene Expression Omnibus (GEO) database record - GDS1375 and their visualization using the R programming language.

## Materials and methods
- *Database characteristics*

Bioinformatics analysis was performed on microarray data available in the National Center for Biotechnology Information/Genbank GEO database (GDS1375 series). The dataset included expression profiles of selected genes for 70 pathologist-assessed clinical samples, which consisted of 7 normal skin, 18 benign moles and 45 primary melanoma. Most of the primary melanomas that were included in the analysis represented the early stage of the disease and had a thickness of <4 mm. Primary melanoma and benign nevus tissues contained more than 50% melanocytes. The diagnosis of the tumor was confirmed by hematoxylin and eosin staining and immunohistochemistry using antibodies (S100 and HMB45). Normal skin tissues were from patients without melanoma. Samples were collected according to a protocol approved by the Institutional Review Board (IRB) and then hybridized on a high-density Affymetrix Hu133A array (Santa Clara, CA) containing 22,000 sets of probes. The level of expression was assessed in 11 receptors, including 6 from class A (EPHA1,-2,-3,-4,-5,-7) and 5 from class B (EPHB1,-2,-3,-4,-6 ).

In addition (apart from the main aim of the work), the expression of 8 ephrins was investigated, including 5 from class A (EFNA1,-2,-3,-4,-5) and 3 from class B (EFNB1,-2,-3).

- *Bioinformatic analysis*

In order to determine the degree of differentiation of the tested samples, data on the relative expression levels of ephrin receptors were subjected to statistical analysis using the R programming language. Records were initially assigned to appropriate groups according to the distinguished disease stages. Expression values were converted using binary logarithm.

- *Statistical analysis*

Due to the small size of the considered groups, the statistical procedure was performed using the non-parametric Kruskal-Wallis test, based on ranks. The test result only suggests a significant difference between the studied groups. Accurate pairwise comparisons using the Dunn test were performed only for those receptors for which the Kruskal-Wallis test yielded a p < 0.05. Due to the problem of multiple testing in the post-hoc analysis, the Bonferroni correction was applied to reduce the probability of committing a type I error. The distribution of variables in the studied groups was presented in a graphical form, taking into account the differences between the medians. Test probability results lower than 0.05 were considered statistically significant.

## Results

It has been shown that EPH receptors undergo variable expression depending on the studied group, which is the basis for further verification with experimental data.

![image](https://github.com/AleksandraGom/R-Project/assets/62109995/e3879c33-bd1d-4cdc-a6f1-072a436ad455)
![image](https://github.com/AleksandraGom/R-Project/assets/62109995/ffcaa4a8-7dc2-4228-a37d-cf7dd5ba42bd)
![image](https://github.com/AleksandraGom/R-Project/assets/62109995/e8be429c-9c4b-4e21-9470-7ee23d361064)
![image](https://github.com/AleksandraGom/R-Project/assets/62109995/ed29ba60-9146-4d48-8db6-33e939211aa5)
![image](https://github.com/AleksandraGom/R-Project/assets/62109995/af04e0ed-29d2-4f5b-9834-d2b4e7760267)
![image](https://github.com/AleksandraGom/R-Project/assets/62109995/3eb3b1dc-531e-4a1c-90fe-c68ae0f1bbe5)
![image](https://github.com/AleksandraGom/R-Project/assets/62109995/c8db7664-0304-485e-8f8c-ceb6dfa7a431)
![image](https://github.com/AleksandraGom/R-Project/assets/62109995/05e077a8-896e-4821-8e24-f1a669d0164c)

## Sources

- M. Michalska-Jakubus, T. Jakubus, D. Krasowska (2006). Czerniak – epidemiologia etiopatogeneza i rokowanie, Med Rodz, 2, 45-53.
- M. Pokrywka, A. Lityńska (2012). Celując w czerniaka, Postępy biologii komórki, 39, 3-23
- E. Wybieralska, E. Łączna, Z. Madeja (2009). Rola efryn w regulacji migracji komórek nowotworowych, Postępy biochemii, 55, Numer 2
- https://www.ncbi.nlm.nih.gov/sites/GDSbrowser?acc=GDS1375
- Talantov D, Mazumder A, Yu JX, Briggs T, Jiang Y, Backus J, Atkins D, Wang Y (2005). Novel genes associated with malignant melanoma but not benign melanocytic lesions. Clin Cancer Res,11(20):7234-42. 
- London M, Gallo E (2020). The EphA2 and cancer connection: potential for immune-based interventions. Mol Biol Rep,47(10):8037-8048. 
- Vearing C, Lee FT, Wimmer-Kleikamp S, Spirkoska V, To C, Stylianou C, Spanevello M, Brechbiel M, Boyd AW, Scott AM, Lackmann M (2005). Concurrent binding of anti-EphA3 antibody and ephrin-A5 amplifies EphA3 signaling and downstream responses: potential as EphA3-specific tumor-targeting reagents. Cancer Res,65(15):6745-54. 
- McGrath, J.A., Eady, R.A.J. and Pope, F.M. (2004). Anatomy and Organization of Human Skin. In Rook's Textbook of Dermatology (eds T. Burns, S. Breathnach, N. Cox and C. Griffiths). 
- W. Sawicki (1993). Histologia, Wydawnictwo Lekarskie PZWL, Wydanie IV
- Roh, M.R., Eliades, P., Gupta, S. and Tsao, H. (2015). Genetics of melanocytic nevi. Pigment Cell Melanoma Res, 28: 661-672. 
- Damsky WE, Bosenberg M (2017). Melanocytic nevi and melanoma: unraveling a complex relationship. Oncogene, 36(42):5771-5792.                                      - Easty DJ, Hill SP, Hsu MY, Fallowfield ME, Florenes VA, Herlyn M, Bennett DC (1999). Up-regulation of ephrin-A1 during melanoma progression. Int J Cancer.,84(5):494-501.
- Tandon M, Vemula SV, Mittal SK (2011). Emerging strategies for EphA2 receptor targeting for cancer therapeutics. Expert Opin Ther Targets, 15(1):31-51.
- Light TP, Gomez-Soler M, Wang Z, Karl K, Zapata-Mercado E, Gehring MP, Lechtenberg BC, Pogorelov TV, Hristova K, Pasquale EB (2021). A cancer mutation promotes EphA4 oligomerization and signaling by altering the conformation of the SAM domain. J Biol Chem, 297(1):100876.
- Luan W, Ding Y, Xi H, Ruan H, Lu F, Ma S, Wang J (2021). Exosomal miR-106b-5p derived from melanoma cell promotes primary melanocytes epithelial-mesenchymal transition through targeting EphA4. J Exp Clin Cancer Res,40(1):107. 
- Piffko A, Broggini T, Harms C, Adams RH, Vajkoczy P, Czabanka M (2021). Ligand-Dependent and Ligand-Independent Effects of Ephrin-B2-EphB4 Signaling in Melanoma Metastatic Spine Disease. Int J Mol Sci, 22(15):8028.
- Hafner C, Bataille F, Meyer S, Becker B, Roesch A, Landthaler M, Vogt T. (2003). Loss of EphB6 expression in metastatic melanoma. Int J Oncol, 23(6):1553-9.

