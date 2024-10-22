# Game of Thrones: A Song of Networks and Homophily

<p align = "center">
	<i> “You know nothing, Jon Snow.” — Ygritte </i>
</p>

Game of Thrones TV show became a world-wide phenomenon, braking records of viewership with each new episode. Originally an adaptation of a series of fantasy novels from George R. R. Martin by the name of A Song of Ice and Fire—a seven volume series, out of which two last volumes are still in writing. The books are generally praised for their medieval realism combining historical elements with fantasy and realistic characters—protagonists die from minor stab wounds and often fail in their endeavours. Realism of A Song of Ice and Fire is something that appeals to wide audiences and makes it possible for the readership to empathise with the main characters while being engaged in a fantastic world full of dragons, mysticism, and political machinations.

What is the charm of the realism in fantasy literature? I argue that it is not only the physical realism of the world (the stabbing and dying), but it is also the social setting that makes the novel so successful—the believable and functioning social system that the reader engages with through the story-line. It is the fact that protagonists participate in social exchanges in a way as an average reader would in their situations. As Lieberman (2013) argues, social cognition—our ability to connect and relate with other people—is one of the default traits of human beings. According to his research, “people typically have a strong interest in the social world and are likely to choose to think about it when they have free time” (Lieberman, 2013: 19). A fantasy novel with realistic and engaging social relations will hence have a higher chance of being stuck in readers’ minds to contemplate about after they finish reading. As a result, I hypothesise that part of the success of A Song of Ice and Fire can be attributed to the realism of its social relations.

In order to verify this hypothesis, I chose one of the most robust traits of the social world found in social sciences: homophily (Lazarsfeld and Merton, 1954; McPherson Smith-Lovin & Cook, 2001). The principle of homophily is a well researched phenomenon in various social settings, ranging from marriage and friendship to political support and information transfer (McPherson et al., 2001). Possible explanations about the source of homophily diverge between two main theoretical streams: structural homophily and choice homophily.

Structural homophily is created through an existing structural constraint (Blau, 1977). People attending the same university, for example, would create homophilous associations with the similarly educated individuals as themselves not due to their personal preference to associate with people of same education, but simply due to participating in education at the same institution. Choice homophily, on the other hand, is created as a result of a conscious choice of the individual to associate with individuals with similar socio-demographic characteristics (McPherson et al., 2001). Even within a homogenous group with regard to education, we might observe ethnic homophily as a result of individuals preferring to associate with people of the same ethnic background. The decision to seek out association with people of the same ethnicity is, therefore, a choice of the individual and not a structural constraint. Although it is almost impossible to disentangle the effects of these two processes, it is likely that their combination can lead to even more homophilous and segregated outcomes. As Brashears argues, “Structure restricts the pool of available associates ensuring that choice can only operate on a limited set of options, while choice ensures that what little opportunity for diversity that remains is mostly foregone” (2008: 401).

In this article, I will research presence of three manifestations of homophily in the first book of A Song of Ice and Fire: gender homophily, house allegiance homophily, and nobility homophily. The former two manifestations—gender and allegiance—are believed to be a manifestation of choice homophily. People interact with others of the same gender or of allegiance to the same house due to their choice and not due to structural constraints as there are ample possibilities to interact with individuals of different gender and allegiances. Gender homophily has been investigated widely in the literature on social networks (Brashears, 2008; Marsden 1987) or in educational setting (Shrum, Cheek & Hunter, 1988). Regarding allegiances, Padgett and Ansell (1993) discuss the case of Medici, where associations with central individuals on important positions were crucial for the success of the Medici family in the Renaissance state of Florence between 1400-1434. On the contrary, the latter manifestation—nobility homophily—emerges due to the structural segregation between the nobility and the commoners. Homophilous behaviour of the nobility has been investigated by Marcassa, Pouyet & Trégouët (2017) on marriage and union patterns of European nobility between 1500s and the 1800s. The nobility tends to live in a safe and insulated environment of the royal palace with scarce contact with the commoners usually living in towns or villages. Hence, the homophily could emerge as a consequence of this structural constraint.

	
## Methodology and Data

In order to investigate the extent of homophily in the Game of Thrones universe, I make use of Exponential Random Graph Models (ERGM). ERGMs are modified logistic regressions that model the probability of tie occurrence within a network, dependent on various network characteristics around the tie or specific attributes of the nodes or ties in question.
	In any case, investigating a hypothetical and non-existent world of the Game of Thrones might seem problematic. A seasoned social scientist, however, should be able to handle the uncertainty attached to investigating ephemeral phenomena connected to unknown and distant populations. The evidence in this article is based on two datasets that were merged together to serve my unique research goals:

1. Character interaction network, parsed from the original book by Andrew J. Beveridge and Jie Shan (2016; 2017). Two characters are coded as interacting each time when two characters’ names (or nicknames) appear within 15 words of one another in the book. Although the dataset provides also weighted ties, I will use only their binary variant, modelling presence or lack of a tie between two observations.
2. Character meta-data scraped from the website “A Wiki of Ice and Fire” by Erin Pierce and Ben Kahle (2014) for their paper predicting character deaths. For every character, Pierce and Kahle collected data about their gender, whether they are part of the nobility, what major house are they loyal to, and in which book (and chapter) the character dies. In order not to spoil anyone’s fun reading or watching the series, I omit the data on character’s death in this paper. Data are accessible via Kahle (2014).

Combining these two datasets, we obtain network data with important node attributes that make investigation of homophily in the Game of Thrones universe possible. After initial cleaning and merging of data, I obtain a dataset with 154 nodes and 531 undirected edges. Since this analysis is based on a book world where it is relatively easy to collect data, no missing data occurs. However, I deleted 33 observations from the original dataset since they represented either long dead individuals (historical flash-backs) or deities, of which neither is important for the analysis.


## Results

The social network generated by parsing character interactions from the first book of A Song of Ice and Fire is relatively sparse with density of 4.5%. Nonetheless, as is expected from data collected from a book, it contains several central individuals (protagonists) that serve as bridges between the centre and the periphery of the network. It seems that there are several distinguishable clusters—firstly, it is the centre that is very tightly clustered together that represents the main protagonist story-line at the high royal court, and then at least three larger peripheral groups—story-lines that are taking place in distant places. The network shows disproportionately fewer females than males, while approximately half of the population is labelled as nobility.

<p align = "center">
	<b>Figure 1.</b> <i>Social network plots by gender, allegiance and nobility</i>
</p>

<p align="center">
	<img src="networkPlotsFinal.png" width="500">
</p>

Initial evidence for homophily can be obtained also from visualization in Figure 1. Even despite a low number of females in the data, there are visible clusters of females located tightly together. Regarding allegiance homophily, on the one hand, we see clusters of individuals of similar allegiance on the periphery of the network, while, on the other hand, the central part of the network seems to be rather mixed. Finally, nobility homophily is clearly visible as the nobility clustered in the centre of the plot while commoners gravitate towards the periphery.

ERGMs are modelling the probability of a tie occurring between two randomly chosen nodes, given the covariates included in the model. The aim is to build up a model that would well represent the dependency structure of the modelled network by adding important nodal or structural covariates. Model estimation followed a stepwise strategy, which is shown in Table 1. First, I started with an empty model, estimating only the density of the network. Second, I added clustering effect in the form shared partner distribution—shared triangular relationships. Third, I added nodal covariates—gender, nobility, and house allegiances. Finally, I added three homophily variables, one for each of the investigated hypotheses—gender, nobility, and house allegiance. Homophily variables are placed at the top of the Table 2. Adding all of these covariates seems to improve the model fit, which is shown by decreasing AIC and BIC in Table 1. for each subsequent model.

**Table 1.** *ERGM: Model Selection*

|   |                                                           | AIC  | BIC  |
|---|-----------------------------------------------------------|------|------|
| 1 | Empty model                                               | 4331 | 4339 |
| 2 | + shared partner distribution (gwesp)                     | 3791 | 3806 |
| 3 | + node covariates (gender, allegiances, nobility)         | 3601 | 3704 |
| 4 | + homophily (node match on gender, allegiances, nobility) | 3260 | 3385 |


The results show significant positive effects of all three homophily variables, while controlling for all structural variables included in the model. First, gender homophily shows that the odds of having a tie between two nodes of the same gender is 1.62-times the odds of having a tie between two different genders. Second, nobility homophily shows that it is 1.25-times more likely to have a tie between people of the same nobility status, compared to individuals of different status. Finally, allegiances homophily shows that it is 5.58-times more likely to have a tie between two individuals with the same house allegiance, compared to different allegiances. These findings verify the hypotheses that the Game of Thrones realm contains realism not only regarding the physical abilities of its characters, but also with regard to their social relations.

It has to be noted, however, that the effect of allegiance homophily is the largest among all three investigated effects—3.4-times larger than the effect of gender homophily and 4.5-times larger than nobility homophily. It might be argued that house allegiance is one of the most important factors for association of individuals in the Game of Thrones realm, as the houses seem to play important institutional roles. In the story, not only do they provide protection against invasion from opposing houses, they also provide representation at the high court. Moreover, marriage strategies often follow findings of Marcassa, Pouyet & Trégouët (2017), using the institution of marriage as a tool to strengthen allegiances among the important houses in the higher political circles.

**Table 2.** *ERGM: Model Coefficients and Standard Errors*

|                                    | Model 1 |      |   | Model 2 |      |   | Model 3 |      |   | Model 4 |      |
|------------------------------------|---------|------|---|---------|------|---|---------|------|---|---------|------|
|                                    | est.    | s.e. |   | est.    | s.e. |   | est.    | s.e. |   | est.    | s.e. |
| **Homophily**                      |         |      |   |         |      |   |         |      |   |         |      |
| Node Match (Gender Homophily)      |         |      |   |         |      |   |         |      |   | 0.48    | 0.16 |
| Node Match (Nobility Homophily)    |         |      |   |         |      |   |         |      |   | 0.22    | 0.11 |
| Node Match (Allegiances Homophily) |         |      |   |         |      |   |         |      |   | 1.72    | 0.09 |
| **Structural parameters**          |         |      |   |         |      |   |         |      |   |         |      |
| Density                            | -3.05   | 0.04 |   | -5.35   | 0.16 |   | -7.10   | 0.40 |   | -6.70   | 0.37 |
| Shared Partner Distribution        |         |      |   | 2.28    | 0.16 |   | 2.03    | 0.16 |   | 1.92    | 0.16 |
| **Node attributes**                |         |      |   |         |      |   |         |      |   |         |      |
| Node Covariate (Sex)               |         |      |   |         |      |   | -0.14   | 0.07 |   | -0.49   | 0.12 |
| Node Covariate (Nobility)          |         |      |   |         |      |   | 0.57    | 0.06 |   | 0.62    | 0.07 |
| Node Factor: Baratheon             |         |      |   |         |      |   | 1.22    | 0.21 |   | 1.27    | 0.20 |
| Node Factor: Greyjoy               |         |      |   |         |      |   | 0.98    | 0.29 |   | 1.10    | 0.29 |
| Node Factor: Lannister             |         |      |   |         |      |   | 0.84    | 0.18 |   | 0.39    | 0.16 |
| Node Factor: Night's Watch         |         |      |   |         |      |   | 0.88    | 0.18 |   | 0.54    | 0.17 |
| Node Factor: None                  |         |      |   |         |      |   | 0.59    | 0.19 |   | 0.34    | 0.17 |
| Node Factor: Stark                 |         |      |   |         |      |   | 0.95    | 0.18 |   | 0.42    | 0.16 |
| Node Factor: Targaryen             |         |      |   |         |      |   | 0.92    | 0.19 |   | 0.65    | 0.17 |
| Node Factor: Tully                 |         |      |   |         |      |   | 0.19    | 0.21 |   | 0.07    | 0.19 |
| Node Factor: Tyrell                |         |      |   |         |      |   | 0.34    | 0.24 |   | 0.34    | 0.23 |
| Node Factor: Wildling              |         |      |   |         |      |   | -0.04   | 0.38 |   | 0.05    | 0.44 |


The smallest effect of nobility homophily can be explained by the nature of the data. The dataset that I use is tracking interactions among individuals—not friendship and alliance networks per se. Nobility in the Game of Thrones realm necessarily interact with the commoners, who are often employed as their servants or men at arms. Although I initially hypothesised nobility homophily as a structural effect which would segregate nobility from commoners, an explanation of the low effect might suggest also an opposite social mechanism. Hypothetically, although nobility would prefer to associate only with nobility, their servants and men at arms are indispensable for their well-being and safety, therefore they are structurally bound to associate also with lower societal ranks.

Finally, the effect of gender homophily is lower than what was expected. I argue that the effect of gender homophily estimated by Model 4 is a severe underestimation of the true gender homophily effect in the Game of Thrones universe. As can be seen on Figure 1, gender representation is heavily favouring male protagonists. Possibly, therefore, the data is biased by selection on reporting predominantly on relationships and interactions of male characters. Underreported or omitted female characters are likely to interact among themselves more, because if they had interacted equally with men, we would not find the homophily effect in the male-biased sample. Another argument for a strong selection bias of the sample is the fact that the Game of Thrones realm is often ravaged by long-lasting wars fought predominantly by men. This necessarily means higher male mortality due to wars, therefore a sample containing 11.6% females is hardly justifiable as representative of the Game of Thrones population.

Even despite the data problems, it seems that the Game of Thrones universe represented in A Song of Ice and Fire manifests features of realism in social interactions among its protagonists. As hypothesised, network data on interactions between characters show evidence for gender, allegiance, and nobility homophily in the Game of Thrones universe.


## Conclusion

It might seem paradoxical to say that realism makes fantasy literature successful. However, our ability to relate and to connect with the fantastic world might be one of the crucial factors determining our liking or disliking of a fantasy story. Talking about realism does not mean only physical realism, but social realism as well. Human brains are wired to perceive, remember, and contemplate on human relations even in the times when we do not engage in social interactions. This paper puts this claim to test with one of the most successful fantasy stories of the 21st century.
This paper measured the extent of homophily in interactions among characters in the Game of Thrones universe. Namely, it focuses on gender, nobility, and allegiance homophily, as these are well researched and robust findings in the social sciences, hence can be used as markers for social realism in the fantasy literature. Findings show that the Game of Thrones realm exhibits attributes of social realism embodied in homophilous interactions among its characters. Allegiance to the same house produced the highest effect on the probability of interacting between the individuals, followed by gender and nobility.
Possible extension of this paper could be to expand this investigation across all available Game of Thrones books. Moreover, using data on more fantasy books (not only Game of Thrones) could provide more evidence on the link between social realism and book’s popularity among the readers.


## References

- Beveridge, A. J. and Shan, J. (2016) Network of Thrones. Math Horizons Magazine, 23 (4), pp. 18-22.
- Beveridge, A. J. and Shan, J. (2017). Character Interaction Networks for George R. R. Martin's "A Song of Ice and Fire" saga. GitHub Repository: https://github.com/mathbeveridge/asoiaf
- Blau, P. M. (1977). A Macrosociological Theory of Social Structure. The American Journal of Sociology, 83 (1), pp. 26-54.
- Brashears, M. E. (2008). Gender and homophily: Differences in male and female association in Blau space. Social Science Research, 37 (2), pp. 400-415.
- Kahle, B. (2014). bayesianGameofThrones. GitHub Repository: https://github.com/benkahle/bayesianGameofThrones
- Kahle, B. and Pierce (December 2014). Bayesian Analysis of Survival Probability in A Song of Ice and Fire. Available on-line: https://github.com/benkahle/bayesianGameofThrones/blob/master/Report%20V2/Bayesian%20Analysis%20of%20Survival%20Probability%20in%20ASOIAF.pdf
- Lazarsfeld, P. and Merton, R. (1954) Friendship as a social process: a substantive and methodological analysis. In: M. Berger (Ed.), Freedom and Control in Modern Society, New York: Van Nostrand.
- Lieberman, M. D. (2013). Social: Why Our Brains Are Wired to Connect. New York: Broadway Books.
- Marcassa, S., Pouyet, J., Trégouët, T. (2017). Marriage Strategy Among European Nobility. THEMA Working Paper n°2017-17 Université de Cergy-Pontoise, France.
- Marsden, P. V. (1987). Core discussion networks of Americans. American Sociological Review, 52 (1), pp. 122-131
- Padgett, J. F. and Ansell, C. K. (1993). Robust Action and the Rise of the Medici, 1400-1434. American Journal of Sociology, 98 (6), pp. 1259-1319
- Shrum, W., Cheek, N. H., Hunter, S. M. (1988). Friendship in School: Gender and Racial Homophily. Sociology of Education, 61 (4), pp. 227-239.


