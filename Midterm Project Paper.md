Do conventional strawberries benefit producers more than consumers?

A comparative analysis of economic returns and chemical usage risks in California and Florida

Introduction

Strawberry farming in the United States involves significant chemical input, particularly in conventional systems where fungicides, herbicides, insecticides, and other treatments are widely applied to manage pests and maximize yield. California and Florida—responsible for the majority of national production—exhibit different patterns of chemical use and production outcomes. As production has intensified, so has the reliance on chemical treatments.

Using USDA NASS Census and Survey data, I wanted to examine the relationship between chemical usage and economic outcomes in strawberry farming. It analyzes trends in chemical application volumes, treatment categories, and state-level producer income. The goal is to understand who benefits most from this system—producers or consumers.

This research explores the question:
Are producers the primary beneficiaries of intensified chemical use in strawberry farming, while consumers bear disproportionate health and financial costs?

By comparing chemical usage trends and income data across California and Florida, I wanted to analyze the economic and environmental trade-offs inherent in modern strawberry production.

Data & Methodology

This analysis uses publicly available data from the USDA National Agricultural Statistics Service (NASS), specifically the Census and Survey programs.

2.1 Data Selection and Filtering

The dataset included observations on strawberry production across the United States. For this analysis, I filtered the data to include only records from California and Florida, the two largest strawberry-producing states. I extracted two types of records:

Census data: to compare net income of producers and operations across states.

Survey data: to analyze chemical application trends and treatment patterns.

After looking at all the data I created a list of the number of entries for each column using the table command which helped me see a wholistic view of where a majority of the data was lying. Within the Survey data, only entries related to bearing acreage for the commodity “Strawberries” were selected as they were the most data dense, as such would give me more accurate extrapolation results. These were further split into records categorized by Bearing - Applications and Bearing - Treated, allowing analysis of both frequency and intensity of chemical use.

2.2 Chemical Classification

I extracted the chemical usage data by filtering entries where Domain == "CHEMICAL". I then parsed the Domain Category column to classify chemicals into four categories:

Fungicide

Herbicide

Insecticide

Other

To process the Data Item and Domain Category columns, I created a custom commas variable was created to identify entries with three comma-separated components. I then separated these entries into new columns (Fruit, Category, Item, Metric) for clarity and grouped by year, chemical type, and treatment category.

2.3 Organic vs Conventional Classification

Although the dataset contained some organic records, they were limited in number (13 entries total for California and Florida). As such, I decided to focus my analysis on conventional strawberry farming, using chemical presence as a proxy for non-organic practices.

2.4 Income and Price Analysis

I used census records to extract producer income data. Specifically, I analyzed two income types:

Net income of the producer

Net gain from operations

I compared these values across California and Florida to evaluate differences in producer benefit potentially associated with higher chemical usage and production scale.

Producer Gains in California and Florida

In section I wanted to compare producer income in California and Florida using data from the USDA Census program. My objective was to assess whether producers in high-output states like California benefit more from intensified farming practices, including increased chemical usage, relative to producers in Florida.

Net Income Categories

Two income categories were extracted for comparison:

Net Income of the Producer: Income attributed directly to the farm's owner or operator.

Net Gain from Operations: The total financial gain after expenses.

I calculated summary statistics for each income type by state:

Fig 3.1: Summary statistics

Producers in California reported income levels approximately four to five times higher than those in Florida across all categories. This gap reflects both the scale of production and potential benefits gained from high-input farming practices, including more frequent and intensive chemical use.

The substantially higher earnings among California producers suggest that intensified agricultural practices, supported by increased chemical input, contribute to greater profitability. This supports my initial hypothesis that producers, particularly in California, are the primary beneficiaries of conventional farming systems that rely heavily on chemical treatments. This lead me to explore whether consumers, in contrast, may bear disproportionate financial or health-related costs.

I created grouped bar charts to compare five income categories for each state:

Operations – Gain

Operations – Loss

Operations – Net Income

Producers – Loss

Producers – Net Income

Fig 3.11: Income by type for Florida

This chart shows relatively modest total income values across categories, with noticeable operational losses and net incomes generally below $4 million.

Fig 3.12: Income by Type for California

In contrast, California demonstrates significantly higher earnings, with net incomes exceeding $15 million and total operation income surpassing $50 million. This supports the idea that producers in California benefit far more financially from their production systems.

3.2 Income Distribution Analysis

To explore the spread and concentration of producer income, I created kernel density plots for both states using net income values.

Figure 3.20 – Density Plot of Net Income for Florida

This plot shows a sharp peak near zero, indicating that most Florida producers operate on thinner margins or lower profit levels.

Figure 3.4 – Density Plot of Net Income for California

California's income distribution is much wider, with multiple peaks and a longer tail. This suggests that while not all producers earn high incomes, a substantial number benefit from very high profitability—likely due to larger scale operations and greater use of inputs, including chemical treatments.

3.3 Interpretation

These visualizations illustrate a clear divide: California producers earn significantly more and display more income variability, which likely reflects differences in production scale and chemical input intensity. Florida producers, by comparison, earn less and have a more compressed income distribution.

The combination of bar and density plots supports the conclusion that producers in high-intensity farming environments (like California) are reaping greater economic rewards, reinforcing the asymmetry posed in my research question.

4. Consumer Costs: Chemical Exposure and Health Trade-offs

While producers—particularly in California—reap economic gains from intensified chemical use, consumers may bear hidden health costs. These costs are not reflected in product pricing but manifest through residue exposure and increased reliance on costly organic alternatives. I wanted to explore rising chemical usage in strawberry farming, especially in California, and its potential implications for consumer health.

4.1 Rising Chemical Usage in Conventional Farming

Between 2021 and 2023, the use of fungicides, herbicides, and insecticides increased substantially across both California and Florida. These inputs are central to maximizing yield and maintaining fruit quality. However, their increased use raises concerns about health risks from chemical residues, particularly given strawberries’ thin skin and common raw consumption.

Across both states, chemical use trends show clear increases:

Fig 4.10: Summary of Chemical Usage by year in California

Fig 4.11: Summary of Chemical Usage by year in California

These trends show significant increases in chemical intensity, particularly in California, which accounts for the vast majority of U.S. strawberry production. To visualize these changes, I created box plots.

Figure 4.21: California Chemical Usage (2021–2023)

This boxplot shows the distribution of chemical usage (in lb/acre) for four categories—fungicide, herbicide, insecticide, and other chemicals—across 2021 and 2023. The boxplot is a little skewed because of the outliers (can be seen as the dots shown).

Figure 4.22:  Florida Chemical Usage (2021–2023):

This boxplot highlights significantly higher and more variable usage, particularly in fungicides and “Other” chemicals. The boxplots clearly showed that California not only has higher variability, but also more concentrated usage at the median and upper quartiles, especially for fungicides and insecticides. The spread of chemical usage in Florida was more compressed, suggesting more uniform and possibly regulated application.

These patterns led me to focus on California’s production practices and their downstream effects—particularly on consumers who frequently purchase strawberries in supermarkets without knowing their origin or treatment history.

Conclusion

In this analysis, I set out to explore a central question:
Do conventional strawberries benefit producers more than consumers?
By comparing producer income and chemical usage patterns in California and Florida, I aimed to uncover whether the gains from intensified farming practices are shared fairly—or disproportionately favor producers at the expense of consumer well-being.

The data shows a clear divide.

California strawberry producers reported significantly higher income levels across all measured categories.

In some cases, net income exceeded $15 million per farm (USDA NASS, 2023), while Florida producers typically operated at smaller scales and earned far less—often under $4 million.

This difference in financial outcomes is closely aligned with higher chemical usage in California, particularly in fungicides and insecticides (USDA NASS, 2023). I observed that fungicide usage alone rose from 395 lb/acre to 492 lb/acre in California between 2021 and 2023, while insecticides increased by 66 lb/acre in the same period.

In contrast, the consumer side of the equation reveals hidden costs.

Strawberries are consistently ranked #1 on the Environmental Working Group’s “Dirty Dozen” list due to their high pesticide residue levels (EWG, 2023).

In review of fungicides such as captan and insecticides like malathion suggests meaningful health concerns—from carcinogenic effects to neurodevelopmental risks in children (EPA, 2021; CDC, 2020).

Yet, consumers often lack the information or financial flexibility to avoid exposure, especially given the 30–50% price premium typically charged for organic strawberries (USDA ERS, 2022).

Taken together, my findings support the hypothesis that producers are the primary beneficiaries of intensified chemical use in strawberry farming.

While they profit from higher yields and scale-driven efficiencies, consumers bear the burden of chemical exposure and rising prices for safer alternatives.

This imbalance highlights a deeper structural issue: the costs of industrialized agriculture are externalized onto public health and consumer decision-making.

Ultimately, this analysis underscores the need for more transparent labeling, affordable access to organic options, and better public understanding of how agricultural intensity shapes both our economy and our health.

Limitations and Further Research

6.1 Limitations

While this analysis provides insights into the economic and environmental trade-offs in conventional strawberry farming, several limitations should be acknowledged:

Limited Organic Data: Only 13 data points for organic strawberries in California and Florida restricted the ability to conduct robust comparisons between organic and conventional systems. As a result, conclusions around consumer pricing and exposure avoidance are based on broader patterns rather than statistical inference.

Lack of Consumer-Level Pricing Data: Retail prices paid by consumers are not directly available in the dataset. Price premium observations for organic strawberries are inferred based on existing literature and USDA reports, not from the raw data.

Chemical Usage as Proxy for Risk: The dataset reports chemical application amounts but does not include residue levels, toxicity, or health outcome data. Therefore, consumer risk is discussed as a potential exposure rather than a measured one.

6.2 Further Research Questions

This analysis lead me to more questions and some several future research directions could include:

What are the long-term health impacts of pesticide residue exposure from conventional strawberry consumption?

How do cost structures differ between organic and conventional producers?

What role do state-level regulations and environmental policies play in shaping chemical usage patterns?

How does consumer demand influence producer decisions regarding organic certification and chemical use?

Can more granular treatment-level data improve risk models for pesticide exposure?

Citations

Bolda, M., Klonsky, K., & De Moura, R. (2014). Sample Costs to Produce Strawberries – Central Coast Region of California. UC Davis Agricultural & Resource Economics. https://coststudies.ucdavis.edu/en/current/commodity/strawberries/

California Strawberry Commission. (2023). Pest Management Tools and Approaches. https://www.californiastrawberries.com/growing-practices/pest-management/

Centers for Disease Control and Prevention (CDC). (2020). Fourth National Report on Human Exposure to Environmental Chemicals.

Environmental Working Group (EWG). (2023). Shopper’s Guide to Pesticides in Produce. https://www.ewg.org/foodnews/summary.php

Goodhue, R. E., Fennimore, S. A., & Ajwa, H. A. (2016). Strawberry production and the economic effects of methyl bromide phase-out. Renewable Agriculture and Food Systems, 31(2), 145–155. https://doi.org/10.1017/S1742170515000116

U.S. Environmental Protection Agency (EPA). (2021). Pesticide Use and California Compliance.

USDA Economic Research Service (ERS). (2022). Fruit and Tree Nuts Outlook: March 2022. https://www.ers.usda.gov/publications/pub-details/?pubid=102931

