# University Projects
Welcome to my repository of reports on the university projects I developed from 2019 to 2023 at the University of Padua and the "Sapienza" University of Rome. These projects were implemented using programming languages and analytical tools. Reports are mainly written in Italian. Below, you'll find a brief description of each of these projects.

## Analyzing Forecasting Models for Real Estate Markets: the Case Study of Madrid Prices
### Master's Thesis - R, OpenStreetMap - March 2023
The real estate market is a vital part of a country's economy that deals with the construction, management, and buying and selling of real estate properties that can be used for a multitude of purposes. In this context, the ability to accurately evaluate and predict the selling price of real estate has become a priority for professionals in the industry and investors. The price is often difficult to determine because it is influenced by a series of factors both internal and external to the property itself, such as its size, maintenance condition, geographic location, and economic situation. For this reason, statistical models are a useful tool to support predictive purposes.

In the context of this thesis, different predictive models for the real estate market will be compared to establish the value of homes as accurately as possible. In particular, attention will be paid to the spatial component, as the geographic location of the property is a key element in defining the selling price. Both traditional and spatial predictive models will be used. Traditional regression models have been widely used in this field and take into account the geographic location through the introduction of specific variables. On the other hand, spatial models incorporate information about the geographic proximity of observations directly into the construction method and explicitly express the intensity of these relationships through specific parameters. Therefore, it is interesting to study and compare these two different approaches.

Another important problem is represented by the management of missing data. In this type of problem, missing data is often present in large quantities, especially regarding information on internal characteristics of the property. The correct treatment of missing data is therefore an integral part of the analysis. This involves the need to use appropriate imputation techniques to estimate missing values.

The thesis will focus on the development of an application for the real estate market in Madrid. This city represents an extremely interesting geographic area with a varied, wide, and constantly evolving sector. Moreover, this study area is characterized by the availability of high-quality data from the main Spanish real estate portals. Such properties are described by numerous information, and unlike portals in other countries, the precise address of the home is often indicated.

In summary, this work can represent a contribution to improving the understanding of the real estate market and supporting investment decisions.

## Quality Assessment in the Statistical Register Construction Process
### ISTAT report - R - June 2022
The article discusses the importance of using administrative records for statistical purposes, and the need for quality evaluation tools to measure the accuracy and effectiveness of these records. In this context, the article examines the quality-framework of Statistics Austria presented in the Austrian Journal of Statistics in 2016 and proposes a possible application of this framework to the Italian Integrated System of Registries (SIR). The Statistics Austria quality-framework is a complex system consisting of three progressive phases: input, process, and output. The quality evaluation is carried out for each administrative record at the single-variable level, and four quality dimensions are identified: documentation, pre-processing, external sources, and imputation. The article focuses on the quality indicator for the first phase of the framework.

## Database Construction on NBA Games
### SQL (MySQL), NoSQL (MongoDB) - May 2022
This project is based on the construction and interrogation of a database on NBA games between 2004 and 2021. The 5 tables that make up the database are downloaded from Kaggle at: [https://www.kaggle.com/datasets/nathanlauga/nba-games](https://www.kaggle.com/datasets/nathanlauga/nba-games).
The queries were proposed in order to answer possible questions of interest that may be asked of the database.

I articulated the work by dividing it into 4 phases:
- Construction of the relational database: MySQL WorkBench is used to define the tables, their identification keys and the relationships between tables
- Querying the SQL database
- Query optimization
- Construction of the non-relational database: MongoDB is used to define the corresponding non-relational database and the respective queries, which are then compared


## Parallel Clustering on a Network of Molecules
### Java, Spark - January 2022
The aim of the project is to do a clustering on a network of PPI-type molecules using the algorithm of Girvan and Newmans based on the calculation of Edge-Betweenness. The approach used is Map-Reduce and refers to the Apache Spark framework. The PPI network is a network that represents the interaction between proteins, it contains arcs and nodes to describe the relationship between the proteins; in this specific case the nodes represent the proteins and the arcs the relationships between them.
Clustering is useful to search for groups of proteins: proteins in the same group will have stronger relationships with each other and proteins in different groups will have a weaker relationship.


## Robust Clustering on Italian Provinces
### R - Dicember 2021
Robust clustering models are contrasted with non-robust models that rely on a finite mixture of Gaussian components. Specifically, the tclust and mclust R-packages are compared. The comparison is conducted in three different ways:
- Theoretical level
- Simulation
- Real-life case study

For the actual case study, a demographic dataset sourced from the ISTAT website was utilized, encompassing eight distinct demographic indicators recorded in 2019 across 107 Italian provinces.


## Text Mining on Rock Sub-Genres
### R - June 2021


## The Effect of Migration on Population Dynamics with Application to Italian Provinces
### Bachelor's Thesis - R - October 2020
To understand the phenomenon of migration movements, it is essential to first define what is meant by migration, as there can be various specifications, each with its own characteristics and associated effects. According to Istat, the most relevant migration events at the demographic level are those, treated as flow data, that coincide with registrations or cancellations from the registry office due to a change of residence. Migration therefore has an impact on the size and structure of the resident population by age and gender; it also has economic, social, and cultural implications for the territories concerned.

Migration movements occur between political or administrative territorial areas, whose boundaries must be well defined. In general, internal migrations within national borders (for example, between municipalities, provinces, or regions) are distinguished from external migrations, the so-called international migrations.

While immigration and emigration can be considered phenomena associated respectively with natality and mortality regarding the introduction and exclusion of a subject from a population, there are substantial differences. Natality and mortality are, in fact, the endogenous determinants of changes in the age structure of a population, while migrations are not necessary events and are part of a more complex context that sometimes concerns distinct populations with their own peculiarities and attitudes.

In almost all cases, immigrations and emigrations have a more marginal role in demography than their natural counterparts, but their influence, often significant, cannot be ignored. In this text, the effects of international migrations due to a change of residence on the main demographic dynamics, such as fertility, natality, and age structure of a population, will be analyzed in particular. To do this, synthetic rates and indicators will be used to encompass specific aspects relevant to the description of a population. Then, part of what has been discussed theoretically will be applied to the case of Italian provinces, using some simple linear and generalized linear regression models. The approach used will always be for contemporaneous rather than cohort of births. 

In conclusion, criticisms, reflections, and possible improvements will be left for those who will study this topic in the future.


## Analysis of the Social Mobility of Graduates in Italy
### SAS - June 2020
The aim of this study is to assess the social mobility of Italian high school graduates, using data from ISTAT on the class of 2015. Social mobility is defined as a change in social status in terms of income, education, and status. To measure it, a variable for the initial and final social class is constructed based on the conditions before and four years after graduation. The study also evaluates the differences in terms of employment, university education, and social emancipation among the different categories of the subject's initial class. In summary, the goal is to verify the existence and intensity of social mobility and to determine whether the initial social class influences one's career and educational path.


## Survival Analysis for Covid-19
### R - May 2020
Starting from the Covid-19 dataset, an analysis was conducted to examine the virus trend during the period of January-February 2020 in three different Asian countries: Japan, South Korea, and Hong Kong. The objective of the study is to investigate the impact of Covid-19 in some representative countries neighboring China during the initial months of the epidemic, in order to gain insights into the progression of the virus across different age and gender groups, and the corresponding response of these nations.


## Sampling for Exit Poll
### R - June 2019
In this paper, a straightforward hypothetical sampling plan is presented for an exit-poll survey designed for the upcoming European elections in Italy, scheduled for May 26th, 2019.
