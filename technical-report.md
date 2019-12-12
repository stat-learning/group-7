Abstract
========

In this project, we aim to determine which factors have the strongest
influence on the earnings of graduates from different institutions of
higher education. To do so, we use data from the College Scorecard to
predict the earnings of the median graduate from a specific institution
6 years after graduation. On top of personal characteristics such as
demographics and family income, we find that institutional
characteristics such as \_\_\_ may lead to higher incomes
post-graduation. To estimate earnings, we used a number of different
statistical techniques: (1) an OLS regression, with predictors pulled
from the literature; (2) a random forest, with variable importances.
Note also that due to the sheer amount of missing data in our dataset,
we estimated the relationship between our predictors and earnings both
without imputation, which removed many observations, as well as with
observations imputed with a random forest technique.

Introduction
============

In the past decade, concern with analyzing and publicizing the
employment outcomes of higher educational institutions and career
training programs has become increasingly widespread. The release of the
College Scorecard dataset by the Obama Administration in 2013
facilitated greater attention to the capacity of incoming undergraduate
students to negotiate both the economic cost and prospective pay-off of
higher education. In a public announcement, the U.S. Department of
Education expressed their concern with enabling students tp make
informed and economically sound college decisions, citing the Obama
Administration’s commitment to providing “hardworking” students “a real
opportunity to earn an affordable, high quality degree or credential
that offers a clear path to economic security and success.”

Theories of Inequality
----------------------

Scholars examining the politics of social inequality have

Equality of opportunity v. condition

Data
====

Download and clean
------------------

To approach this problem, we use data from the College Scorecard, a
project of the United States Department of Education. Below, we start by
downloading the data. Note that we're starting here with data only from
the 2014-15 academic year.

    college.data <- read.csv("2014_2015_college_data.csv", header = TRUE)

This dataset contains 7,703 observations of 1,977 variables. Each
observation represents an individual college/university for the 2014-15
school year.

Now that we have this data, we need to transform it so that R can
recognize missing values. Additionally, we (1) drop observations which
do not contain our response variable, `MD_EARN_WNE_P6`, which is the
"median earnings of students working and not enrolled 6 years after
entry," and (2) drop variables for which all observations are N/A.

    not_all_na <- function(x) any(!is.na(x))
    college.data <- college.data %>%
      replace(.=="NULL", NA) %>%
      replace(.== "PrivacySuppressed", NA)

    # Drop NAs in response
    college.data.section <- college.data %>% 
      drop_na(MD_EARN_WNE_P6) %>%
      select_if(not_all_na)

There are a lot of variables to work with in this dataset--too many, in
fact. To get around this, we went through our dataset and narrowed down
the variables which we have reason to believe could be related to
earnings after graduation. These variables range from characteristics of
the college, such as the region it is located in or the amount it spends
on each student, to characteristics of the student body, such as the
share of the graduates who had ever received a Pell Grant.

Note also that we have limited the scope of our analysis to only
four-year colleges (`ICLEVEL == 1`), as it's reasonable to think that
the factors that are important for graduates of two-year or more
specialized institutions would be different.

    section1 <- college.data.section %>%
      filter(ICLEVEL == 1) %>%
      select(INSTNM,
             REGION, 
             CONTROL, 
             NUMBRANCH, 
             HIGHDEG, 
             PCIP01:PCIP54, 
             UGDS:PPTUG_EF, 
             PCTPELL, 
             INEXPFTE, 
             TUITFTE, 
             PCTFLOAN, 
             WDRAW_ORIG_YR4_RT, 
             DEBT_MDN, 
             LO_INC_DEBT_MDN:HI_INC_DEBT_MDN,
             PELL_DEBT_MDN:NOTFIRSTGEN_DEBT_MDN, 
             FEMALE, 
             LOAN_EVER:AGE_ENTRY,
             DEPENDENT:FAMINC, -VETERAN, 
             FAMINC,
             MD_EARN_WNE_P6)

In this dataframe, unfortunately not all of the variables are classified
appropriately (as a float, a factor, etc.). In order to get this data in
workable format, we split up the data into the numeric and factor
variables, classify each appropriately, and join them together again.

    # Select numeric columns
    numerics <- section1 %>%
      select(NUMBRANCH:MD_EARN_WNE_P6) %>%
      select(-HIGHDEG)

    # Make columns numeric
    numerics[,] <- sapply(numerics[,], as.numeric)

    # Select categorical columns
    categoricals <- section1 %>%
      select(REGION, 
             CONTROL,
             HIGHDEG)

    categoricals <- categoricals %>%
      mutate(REGION = factor(REGION),
             CONTROL = factor(CONTROL),
             HIGHDEG = factor(HIGHDEG))

    # Create final dataframe
    df_final <- cbind(INSTNM = as.character(section1$INSTNM),
                      numerics, 
                      categoricals)

    df_final <- df_final %>%
      mutate(INSTNM = as.character(INSTNM))

Exploratory data analysis
-------------------------

With the data now in workable format, we can start to take a look at our
response and predictors. Our new dataset has 2,822 observations of 77
variables--significantly cut down from our initial one.

### Response variable

A histogram of our response variable (`MD_EARN_WNE_P6`) is shown below.
Note that while this variable is distributed fairly normally, it has a
long right tail.

    ggplot(data = df_final, aes(df_final$MD_EARN_WNE_P6)) + 
      geom_histogram(binwidth = 10) +
      labs(x = "Median wage", y = "Frequency") +
      theme_minimal()

![](technical-report_files/figure-markdown_strict/Histogram%20of%20response-1.png)

One way to reduce the skew in our dependent would be to take the natural
log of it. (This is common practice in Economics for income variables.)
When we make a new variable equal to the log of the median wage, its
histogram is much more "normal-looking." Moving forward, we use this new
response variable as our dependent variable.

    df_final <- df_final %>%
      mutate(log_md_wage = log(MD_EARN_WNE_P6) + 0.001)

    ggplot(data = df_final, aes(df_final$log_md_wage)) + 
      geom_histogram(bins = 40) +
      labs(x = "Log of the median wage", y = "Frequency") +
      theme_minimal()

![](technical-report_files/figure-markdown_strict/Histogram%20of%20log%20median%20wage-1.png)

### Pairwise scatterplots

Before we jump in to any modeling, we should understand the relationship
our dependent variable has with predictors of interest. Some interesting
pairwise scatterplots are displayed below.

    wage.faminc.plot <- ggplot(df_final, aes(x=FAMINC, y=log_md_wage)) + 
      geom_point(alpha = 0.5) +
      labs(x = "Family income", y = "Log of the median wage") +
      theme_minimal()
    wage.faminc.plot

![](technical-report_files/figure-markdown_strict/Logwage%20v%20faminc-1.png)

The relationship between family income and the log of the median wage
seems fairly linear here, necessitating no transformation.

    wage.pell.plot <- ggplot(df_final, aes(x=PELL_EVER, y=log_md_wage)) + 
      geom_point(alpha = 0.5) +
      labs(x = "Number of students who have received a Pell Grant", y = "Log of the median wage") +
      theme_minimal()
    wage.pell.plot

![](technical-report_files/figure-markdown_strict/Logwage%20v%20pell-1.png)

This, too, seems to be a linear relationship, requiring no
transformation.

One relationship that does require a transformation is that below, of
log wage versus the median wage after graduation. Note that there is a
seeming discontinuity between this relationship after the median debt
exceeds 1000.

    wage.meddebt.plot <- ggplot(df_final, aes(x=DEBT_MDN, y=log_md_wage)) + 
      geom_point(alpha = 0.2) +
      labs(x = "Median debt", y = "Log of the median wage") +
      theme_minimal()
    wage.meddebt.plot

![](technical-report_files/figure-markdown_strict/Logwage%20v%20median%20debt-1.png)

    df_final <- df_final %>%
      mutate(logdebt = log(DEBT_MDN))

    wage.logmeddebt.plot <- ggplot(df_final, aes(x=logdebt, y=log_md_wage)) + 
      geom_point(alpha = 0.2) +
      labs(x = "Log of median debt", y = "Log of the median wage") +
      theme_minimal()
    wage.logmeddebt.plot

![](technical-report_files/figure-markdown_strict/Logwage%20v%20log%20median%20debt-1.png)

While still not perfect, taking the log of the median debt seems to make
this relationship a little better.

### Correlations

We can dive into this data a little more deeply using a correlation
matrix. We've made one and plotted it below.

    # Add in new variables into numerics
    numerics <- numerics %>%
      mutate(logdebt = log(DEBT_MDN),
             log_md_wage = log(MD_EARN_WNE_P6) + 0.001)

    # Correlation matrix
    corr_matrix <- cor(numerics, use = "complete.obs")
    corrplot(corr_matrix, method="color", tl.cex = 0.4)

![](technical-report_files/figure-markdown_strict/Correlation%20matrix-1.png)

Much of the space in this matrix is taken up by the `PCIP01`, `PCIP03`,
etc. variables. To zoom in on the other variables, we can take these out
and re-run our correlation matrix.

    corr_matrix2 <- cor(numerics[,40:ncol(numerics)], use = "complete.obs")
    corrplot(corr_matrix2, method="color", tl.cex = 0.5)

![](technical-report_files/figure-markdown_strict/Correlation%20matrix%202-1.png)

As we'd expect, many of the debt measures are highly correlated with
each other. Additionally, there are very strongly positive and strongly
negative correlations in a cluster toward the bottom right of this
graph, between variables like `FAMINC`, `FIRST_GEN`, and `AGE_ENTRY`.

### Missingness

One piece of the pie that proved tricky for us moving forward in our
analysis has been the amount of missingness present in the College
Scorecard dataset. As shown below, fully 61% of the data in the
2014-2015 College Scorecard file is missing. (Note that we've actually
attached a picture instead of the code output--the image generated by
the code itself takes around 40 Mb.)

    missmap(college.data)

![](Missingness.png)

Fortunately for us, after all of our data cleaning, this picture looks
much better. In our final dataset (`df_final`), 93% of observations are
present.

One interesting thing to note in this missingness map is the giant block
of missing data in the bottom left. This missing data chunk, clearly not
missing at random, is comprised mostly of schools such as Keiser
University, Harrison College, Rasmussen College, Strayer University.
These schools have a couple things in common: for one, all have multiple
branches, and perhaps most tellingly, these are not categorized as
degree-granting institutions. For these schools, the College Scorecard
does not report the shares of degrees awarded in a particular major, the
racial makeup of these schools, or how much tuition revenue these
schools receive per student.

    missmap(df_final)

![](technical-report_files/figure-markdown_strict/Visualize%20missingness%20for%20df_final-1.png)

In our scenario, observations are not exactly a precious resource; we
could afford to get rid of this chunk of observations. This has some
benefits as well. In addition to being theoretically sound (as
non-degree granting institutions may play a different role in
determining future income than degree-granting institutions), these
observations are distinctly not missing at random. This means that any
imputation (done later) to determine these values would be fundamentally
flawed.

After ridding the dataset of this chunk of observations, our new
missingnessmap looks as follows. Note that fully 98% of our observations
are present. Not too bad!

    df_final_section <- df_final[1:2634,]
    missmap(df_final_section)

![](technical-report_files/figure-markdown_strict/Visualize%20missingness%20without%20red%20chunk-1.png)

### Principal component analysis

One last method to examine the structure and content of our data is
principal component analysis (PCA). Conducting principle component
analysis can be quite useful in order to get a sense of what our data
looks like and what different kinds of colleges we're dealing with.
After finding the primary axes of variation (principal components), we
are able to determine whether there are different "clusters" of
colleges: ones that focus more on certain kinds of areas of study, are
more or less career focused, etc.

    # Drop odd chunk of obs from numerics dataset
    numerics.section <- numerics[1:2634,]
    numerics.nona <- drop_na(numerics.section)
    df_final_section_nona <- drop_na(df_final_section)

    # PCA
    set.seed(40)
    pca <- prcomp(numerics.nona, scale. = TRUE)
    pcs <- as.data.frame(pca$x)
    pc1 <- pca$rotation[, 1]
    pc2 <- pca$rotation[, 2]
    pca3 <- pca$rotation[, 3]
    pcs$INSTNM <- df_final_section_nona$INSTNM

    # Plot
    ggplot(pcs, aes(x = PC1, y = PC2)) +
      geom_point(alpha = .5) +
      theme_minimal()

![](technical-report_files/figure-markdown_strict/PCA%20generation%20and%20plot-1.png)

This is a very interesting biplot. Along the first two principal
components, there are two clear groups, one of which has a greater PC2
value than the other. Additionally, in the group of observations at the
bottom half of the biplot, there appears to be the most density at high
values for PC1 and low values for PC2. In the upper group of
observations, the most density occurs at low/mid values of PC1 and
moderate values of PC2.

    scree.data <- tibble(PC = 1:75,
                         PVE = pca$sdev^2 /
                           sum(pca$sdev^2))
    ggplot(scree.data, aes(x = PC, y = PVE)) +
      geom_line() + 
      geom_point() +
      theme_minimal()

![](technical-report_files/figure-markdown_strict/Scree%20plot-1.png)

As seen in the scree plot above, there is a clear, distinct elbow in our
data. The first two principal components explain a little less 30% of
the variation in the data, after which each individual principal
component becomes increasingly meaningless.

#### A deeper dive

Clearly, the first and second principal components capture important
sources of variation among colleges. However, we don't yet know what
they actually are. Taking inspiration from group 9 (Paul Nguyen and
David Herrero Quevedo), we dove deeper into which variables are actually
important in making up these components.

As an initial step, it may be illuminating to label a random sample of
these observations to see if any obvious patterns pop out. After, we
plot the location of traditional groups (e.g. the Ivies) to see if there
is any validity to grouping these elite colleges together.

    set.seed(532)
    random.schools <- sample(pcs$INSTNM, 10)
    ivies <- c("Brown University", "Columbia University", "Cornell University", "Dartmouth College", "Harvard University", "University of Pennsylvania", "Princeton University", "Yale University")

    # Plot random schools
    ggplot(pcs, aes(x = PC1, y = PC2)) +
      geom_point(alpha = .1) +
      geom_text_repel(data = subset(pcs, INSTNM %in% random.schools),
                aes(label = INSTNM)) +
      geom_point(color = "red",
                 data = subset(pcs, INSTNM %in% random.schools,
                 alpha = .5)) +
      theme_minimal()

![](technical-report_files/figure-markdown_strict/Sample%20schools%20in%20biplot-1.png)

Unfortunately, it's not immediately obvious through labeling some
schools on these principal components what the components actually mean.
To try to get more recognizable names, we've labeled the Ivy League.

    ggplot(pcs, aes(x = PC1, y = PC2)) +
      geom_point(alpha = .1) +
      geom_text_repel(data = subset(pcs, INSTNM %in% ivies),
                aes(label = INSTNM)) +
      geom_point(color = "red",
                 data = subset(pcs, INSTNM %in% ivies,
                 alpha = .5)) +
      theme_minimal()

![](technical-report_files/figure-markdown_strict/Ivies%20in%20biplot-1.png)

Clearly here, the Ivies (or at least the ones which survived our purge
of non-NA data) are all on the left side of the biplot, indicating that
the left side of the plot (when PC1 is negative) may be a region with
more prestigious, smaller schools. Harvard is a notable outlier from
both main groups.

For sheer curiosity's sake, Reed's location on this biplot is shown
below:

    ggplot(pcs, aes(x = PC1, y = PC2)) +
      geom_point(alpha = .1) +
      geom_text_repel(data = subset(pcs, INSTNM %in% c("Reed College")),
                aes(label = INSTNM)) +
      geom_point(color = "red",
                 data = subset(pcs, INSTNM %in% c("Reed College"),
                 alpha = .5)) +
      theme_minimal()

![](technical-report_files/figure-markdown_strict/Reed%20in%20biplot-1.png)

Good news for Reed graduates; we're almost in exactly the same spot as
most of the Ivies!

While plotting the locations of any given college on the principal
component biplot is fun (and a little informative), it doesn't give us a
deep understanding of what these components mean. Moving forward, we
took a more statistical approach toward understanding this.

Below, we've plotted the "rotation" of our first principal component.
Essentially, this plot shows how each variable is weighted when making
PC1: a strong, positive number indicates that PC1 is strongly associated
with a given variable, and vice versa.

    pca_rotations <- data.frame(pca$rotation)
    pca_rotations$variables <- rownames(pca_rotations)
    pc1.ordered.rotations <- pca_rotations[order(-pca_rotations$PC1),]
    pc2.ordered.rotations <- pca_rotations[order(-pca_rotations$PC2),]

    ggplot(pca_rotations, 
           aes(x = variables, y = PC1)) +
      geom_col() +
      coord_flip() +
      labs(x = "Variables", y = "First PC") +
      theme(text = element_text(size = 5.5)) 

![](technical-report_files/figure-markdown_strict/Understanding%20PC1-1.png)

Due to the sheer amount of variables in our PCA, it's hard to read which
variables are positively and negatively correlated with the first PC.
For the reader's sake, we've also sorted the variables by their impact
on PC1.

As seen below, the 6 variables most associated with PC1 are:

1.  `FIRST_GEN`
2.  `AGE_ENTRY`
3.  `PELL_EVER`
4.  `PCTPELL`
5.  `WDRAW_ORIG_YR4_RT`
6.  `LO_INC_DEBT_MDN`

The six most negatively associated with PC1 are:

1.  `DEPENDENT`
2.  `PCIP26`
3.  `PCIP45`
4.  `PCIP23`
5.  `PCIP54`
6.  `FAMINC`

One way of interpreting PC1, then, could be as a measure of the
privelege (or lack thereof) of the students.

    head(pc1.ordered.rotations)

    ##                         PC1         PC2          PC3         PC4
    ## FIRST_GEN         0.2230471  0.01276022 -0.005342524  0.10124358
    ## AGE_ENTRY         0.2198535  0.04969136 -0.029925488  0.05404839
    ## PELL_EVER         0.2193488  0.00783683 -0.035923025  0.15099365
    ## PCTPELL           0.2059191  0.08313005  0.060319970  0.19531526
    ## WDRAW_ORIG_YR4_RT 0.1959303  0.04487854  0.040806438 -0.01099273
    ## LO_INC_DEBT_MDN   0.1468980 -0.11358878 -0.128444035  0.12256608
    ##                           PC5         PC6         PC7          PC8
    ## FIRST_GEN         -0.05037822 -0.12112829 -0.08036704  0.051544957
    ## AGE_ENTRY          0.10955724 -0.06183666 -0.12896385  0.051147004
    ## PELL_EVER         -0.20234813 -0.11350540  0.02883731 -0.008988718
    ## PCTPELL           -0.12534426 -0.00930182  0.04015504  0.015278278
    ## WDRAW_ORIG_YR4_RT -0.09189424 -0.05362971 -0.02390558 -0.096465466
    ## LO_INC_DEBT_MDN   -0.02399183  0.23741315 -0.04807335 -0.003766103
    ##                           PC9          PC10         PC11         PC12
    ## FIRST_GEN          0.01526783  0.0670808621 -0.009817387 -0.003157325
    ## AGE_ENTRY          0.12390595  0.0932626633  0.042649373 -0.014829363
    ## PELL_EVER         -0.03132591 -0.0008811584 -0.005178184 -0.042009909
    ## PCTPELL           -0.10137783 -0.0259237411 -0.017582462 -0.110667613
    ## WDRAW_ORIG_YR4_RT  0.11243434  0.1720816554  0.115872551  0.046690527
    ## LO_INC_DEBT_MDN    0.05927808 -0.0623828556  0.096999106  0.017600681
    ##                          PC13         PC14        PC15         PC16
    ## FIRST_GEN         0.007963201  0.020234963  0.02664137  0.024944220
    ## AGE_ENTRY         0.105684593  0.034898830 -0.02682400 -0.019363480
    ## PELL_EVER         0.027235946  0.030630737 -0.00878657 -0.004823432
    ## PCTPELL           0.010877300  0.004057436  0.06812188  0.011201298
    ## WDRAW_ORIG_YR4_RT 0.101573157  0.002142574 -0.12502063  0.032657798
    ## LO_INC_DEBT_MDN   0.015496396 -0.020788856  0.02272953  0.069314910
    ##                           PC17        PC18         PC19         PC20
    ## FIRST_GEN         -0.024732803 0.068104281  0.007142534 -0.017517640
    ## AGE_ENTRY         -0.003317276 0.118646068 -0.015701803  0.013812470
    ## PELL_EVER         -0.002641422 0.000897999 -0.046083287  0.006351864
    ## PCTPELL           -0.006296721 0.017417435 -0.021571034  0.009104071
    ## WDRAW_ORIG_YR4_RT -0.025186780 0.021578111 -0.045752446  0.015999292
    ## LO_INC_DEBT_MDN    0.112547721 0.022798748  0.135444504  0.036514304
    ##                           PC21         PC22         PC23        PC24
    ## FIRST_GEN          0.014504844  0.002169676  0.001812249 -0.03499086
    ## AGE_ENTRY         -0.027540395 -0.005520504 -0.013462330 -0.02653365
    ## PELL_EVER         -0.023321117  0.013620295  0.033301806  0.01332638
    ## PCTPELL           -0.039380057  0.011737729 -0.028643737  0.06400700
    ## WDRAW_ORIG_YR4_RT -0.007733321 -0.066844044  0.019868663  0.05247440
    ## LO_INC_DEBT_MDN   -0.063479090  0.054111011  0.092778883 -0.01348631
    ##                          PC25         PC26         PC27         PC28
    ## FIRST_GEN         -0.03726820 -0.004765725  0.002350382  0.012036130
    ## AGE_ENTRY          0.06002558  0.012479242 -0.046014288  0.058367661
    ## PELL_EVER         -0.03537291 -0.051213468 -0.020730849 -0.011615353
    ## PCTPELL            0.02240394  0.058637509 -0.052733060 -0.010652956
    ## WDRAW_ORIG_YR4_RT  0.06506973  0.045777625 -0.034683931 -0.003060449
    ## LO_INC_DEBT_MDN    0.05569200 -0.084115108  0.092590915  0.049577121
    ##                            PC29         PC30        PC31         PC32
    ## FIRST_GEN         -0.0175280051  0.048074051 -0.07257270  0.056080021
    ## AGE_ENTRY         -0.0195236433  0.030780810 -0.13838445  0.031574818
    ## PELL_EVER         -0.0230318673  0.065867511 -0.03859907 -0.001985532
    ## PCTPELL           -0.0006807837  0.009870097  0.07495563 -0.016755921
    ## WDRAW_ORIG_YR4_RT -0.0347537078  0.035870851 -0.03236212 -0.012798065
    ## LO_INC_DEBT_MDN    0.2398252440 -0.129415233 -0.09461671  0.090533543
    ##                            PC33        PC34         PC35         PC36
    ## FIRST_GEN          0.0456216410 -0.02252339 -0.027221650  0.010004420
    ## AGE_ENTRY          0.0897004450 -0.09792052 -0.005606188 -0.007899966
    ## PELL_EVER          0.0005952842  0.02848972 -0.010869226 -0.022878096
    ## PCTPELL           -0.0502128943  0.02611678  0.007328866 -0.008343181
    ## WDRAW_ORIG_YR4_RT  0.0272758149 -0.06861385  0.033649010 -0.063117439
    ## LO_INC_DEBT_MDN    0.0340744442  0.06133451 -0.162398516  0.032302683
    ##                           PC37          PC38        PC39         PC40
    ## FIRST_GEN          0.037229714  0.0005776855 -0.01156460 -0.019723287
    ## AGE_ENTRY          0.038343240  0.0163573329  0.06563486  0.025276340
    ## PELL_EVER          0.006942896 -0.0229873282  0.01371463  0.062339138
    ## PCTPELL            0.027506617  0.0184370448 -0.10578616  0.002569837
    ## WDRAW_ORIG_YR4_RT  0.120689372  0.0215810433 -0.07978499 -0.105289355
    ## LO_INC_DEBT_MDN   -0.104501030  0.0740754936  0.02141310  0.042241775
    ##                          PC41         PC42        PC43         PC44
    ## FIRST_GEN         -0.01629915 -0.055501916 -0.05297570  0.135278145
    ## AGE_ENTRY          0.01525848 -0.072816580 -0.03599657  0.016217672
    ## PELL_EVER         -0.02062972  0.001622180 -0.01599686  0.033772597
    ## PCTPELL           -0.07362687  0.001072942  0.05447696  0.070088132
    ## WDRAW_ORIG_YR4_RT -0.03032896 -0.013601177 -0.03616994 -0.006127376
    ## LO_INC_DEBT_MDN   -0.07880276  0.045970651 -0.11622768  0.048557186
    ##                          PC45        PC46          PC47          PC48
    ## FIRST_GEN         -0.06227407 -0.01170344 -0.1736769458 -0.1226670573
    ## AGE_ENTRY         -0.02822686 -0.04419742 -0.1091750756 -0.0387832967
    ## PELL_EVER         -0.03624842  0.04366137 -0.0160440072 -0.0008651276
    ## PCTPELL            0.04331290  0.08151602 -0.0009117928 -0.0727394290
    ## WDRAW_ORIG_YR4_RT  0.06009700  0.05953263  0.0196297959 -0.2230006950
    ## LO_INC_DEBT_MDN    0.18484070  0.15282119  0.1458374797 -0.0011676782
    ##                          PC49         PC50        PC51        PC52
    ## FIRST_GEN         -0.14383782 -0.109334977  0.02151154  0.13574028
    ## AGE_ENTRY         -0.04662785 -0.130119728 -0.01033188  0.12963448
    ## PELL_EVER         -0.04073432  0.032742498 -0.01294829  0.04782772
    ## PCTPELL           -0.05508265  0.002216849 -0.01167241  0.01210459
    ## WDRAW_ORIG_YR4_RT -0.07342420 -0.144553356  0.06428316 -0.05290487
    ## LO_INC_DEBT_MDN    0.06059108  0.027880597  0.03036908 -0.09703619
    ##                          PC53         PC54        PC55        PC56
    ## FIRST_GEN          0.03216344  0.002761355 -0.05407648  0.10654789
    ## AGE_ENTRY          0.01608756  0.042313238 -0.01088048  0.01788980
    ## PELL_EVER         -0.01251492 -0.025088992 -0.03766041  0.06551005
    ## PCTPELL           -0.07197423 -0.085521438 -0.07762451  0.10032803
    ## WDRAW_ORIG_YR4_RT  0.10641083 -0.184409779 -0.15066958 -0.03845022
    ## LO_INC_DEBT_MDN    0.04951983 -0.058370496 -0.04740951 -0.35614820
    ##                           PC57         PC58        PC59         PC60
    ## FIRST_GEN         -0.124404363 -0.080178290  0.04023956 -0.066919089
    ## AGE_ENTRY          0.003060023  0.046881354 -0.02521330  0.035867193
    ## PELL_EVER         -0.118285862 -0.024721874  0.05504533 -0.099575956
    ## PCTPELL           -0.187097662  0.008381827  0.06747890 -0.207671566
    ## WDRAW_ORIG_YR4_RT  0.247653432  0.399421746 -0.24528709  0.262992768
    ## LO_INC_DEBT_MDN    0.019714233  0.224524205  0.28825085  0.001958827
    ##                           PC61         PC62         PC63         PC64
    ## FIRST_GEN         -0.006516485  0.011797083  0.017963113 -0.027841311
    ## AGE_ENTRY          0.058186680 -0.124181766 -0.003512327  0.015666680
    ## PELL_EVER         -0.014411304 -0.023006231 -0.029988402 -0.008740964
    ## PCTPELL           -0.025774478  0.006323249 -0.033617597  0.042873973
    ## WDRAW_ORIG_YR4_RT  0.066860952  0.426609157  0.056681576 -0.094600709
    ## LO_INC_DEBT_MDN   -0.191396659 -0.082403242  0.072308567  0.067062372
    ##                          PC65         PC66        PC67        PC68
    ## FIRST_GEN          0.22117459  0.572476931 -0.05282086  0.37250857
    ## AGE_ENTRY          0.33519710 -0.205258811 -0.00160803 -0.24793170
    ## PELL_EVER         -0.01483004  0.187058673 -0.11878887  0.08940863
    ## PCTPELL           -0.18605063 -0.082091159 -0.12225961 -0.03081176
    ## WDRAW_ORIG_YR4_RT -0.26128129 -0.009104307 -0.01899566  0.04560229
    ## LO_INC_DEBT_MDN   -0.03436853  0.225428746 -0.24554309 -0.12340919
    ##                            PC69        PC70        PC71         PC72
    ## FIRST_GEN         -0.1487042984  0.15127862  0.24030850  0.282502805
    ## AGE_ENTRY          0.1935737227 -0.07188720 -0.02663811 -0.010705113
    ## PELL_EVER         -0.1061187928 -0.30127696 -0.10323237 -0.667424171
    ## PCTPELL           -0.0009505331 -0.42633870 -0.47674866  0.448863868
    ## WDRAW_ORIG_YR4_RT -0.0417276593 -0.02375634  0.03654734 -0.013608176
    ## LO_INC_DEBT_MDN    0.2427414014  0.05562231  0.03672553  0.005531918
    ##                           PC73        PC74         PC75         variables
    ## FIRST_GEN          0.088627448 -0.05824153 -0.006645697         FIRST_GEN
    ## AGE_ENTRY          0.078409581 -0.04675468 -0.668309752         AGE_ENTRY
    ## PELL_EVER         -0.400410633  0.14344914 -0.068827799         PELL_EVER
    ## PCTPELL            0.106050601 -0.04802145  0.010585911           PCTPELL
    ## WDRAW_ORIG_YR4_RT -0.015097809 -0.01264634 -0.005240916 WDRAW_ORIG_YR4_RT
    ## LO_INC_DEBT_MDN    0.001843944 -0.09296069  0.008482170   LO_INC_DEBT_MDN

    tail(pc1.ordered.rotations)

    ##                  PC1         PC2          PC3        PC4         PC5
    ## FAMINC    -0.1757293  0.02755402  0.091957316 -0.1783462  0.07370341
    ## PCIP54    -0.1863051 -0.05755756  0.049972704  0.1808012 -0.10202176
    ## PCIP23    -0.1936778 -0.05809287  0.034521707  0.1632782 -0.08925888
    ## PCIP45    -0.1938171 -0.05610580 -0.062178825  0.2053182 -0.07496957
    ## PCIP26    -0.1967149 -0.06945384  0.009816268  0.1591185 -0.09904183
    ## DEPENDENT -0.2249687 -0.04369329  0.055706989 -0.0658221 -0.09901875
    ##                    PC6          PC7         PC8          PC9        PC10
    ## FAMINC     0.028810118 -0.001219184 -0.07020687  0.077332798  0.01699370
    ## PCIP54     0.015823403 -0.035994420  0.11606774  0.107251610  0.02915906
    ## PCIP23    -0.008529532 -0.014464553  0.09613608  0.107728646  0.02930428
    ## PCIP45    -0.082900445 -0.034812952  0.07207129 -0.006946813  0.07438891
    ## PCIP26     0.017465649 -0.038418451  0.03659351 -0.058706882  0.02469547
    ## DEPENDENT  0.060552800  0.123093753 -0.06479958 -0.096405910 -0.09471507
    ##                  PC11         PC12        PC13         PC14       PC15
    ## FAMINC     0.04435428 -0.037041448  0.02188331  0.005795019 0.10024948
    ## PCIP54    -0.01085933  0.075022977  0.04723922  0.024823387 0.02898210
    ## PCIP23     0.01298794  0.086287442  0.05529029 -0.014080921 0.04975397
    ## PCIP45    -0.03513472  0.073138994  0.05991260  0.010969867 0.04893462
    ## PCIP26    -0.05115366 -0.048319494 -0.01036491  0.084677334 0.03356934
    ## DEPENDENT -0.05640312  0.004538828 -0.07944731 -0.023881365 0.01272327
    ##                   PC16         PC17         PC18        PC19          PC20
    ## FAMINC     0.012302950 -0.003519275 -0.010504897 0.029904867 -0.0255872800
    ## PCIP54     0.034132945 -0.017472697 -0.015189540 0.085498131 -0.0004133672
    ## PCIP23    -0.002340572  0.016632434  0.003722509 0.011385995 -0.0343128358
    ## PCIP45     0.043935636 -0.036772535 -0.037430009 0.024488773 -0.0075384278
    ## PCIP26     0.074806708 -0.042820844  0.036493396 0.013951676  0.0360415969
    ## DEPENDENT  0.007502547  0.007020968 -0.105861049 0.005916525 -0.0132569198
    ##                    PC21         PC22         PC23         PC24
    ## FAMINC     0.0006554791 -0.002804300  0.093196625 0.0607641397
    ## PCIP54    -0.0304048801  0.001792417 -0.003777835 0.0084315614
    ## PCIP23     0.0065973340 -0.011226043 -0.041034470 0.0004487028
    ## PCIP45     0.0174328317 -0.016517602 -0.025727195 0.0196564489
    ## PCIP26    -0.0328168425  0.035905128  0.039662584 0.0191668840
    ## DEPENDENT  0.0258408819  0.015179873  0.010731437 0.0250803160
    ##                   PC25         PC26         PC27         PC28         PC29
    ## FAMINC     0.038019088  0.075470067  0.041168136  0.011581438  0.110457647
    ## PCIP54    -0.049376480  0.068690088 -0.005813782  0.025219546  0.019932024
    ## PCIP23    -0.046528901  0.105198555 -0.071178365  0.007911827  0.038700568
    ## PCIP45    -0.005524226  0.004173464 -0.092263760 -0.036815318  0.003717172
    ## PCIP26     0.031642652 -0.016544118 -0.017624627 -0.006850792 -0.089885521
    ## DEPENDENT -0.042258261 -0.004744555  0.038443200 -0.039977480  0.031109544
    ##                  PC30        PC31        PC32        PC33        PC34
    ## FAMINC    -0.08723088  0.04151329  0.05563356 -0.01785942  0.01324461
    ## PCIP54     0.02037721 -0.07265545  0.01469425  0.08886651  0.04510840
    ## PCIP23    -0.05405660 -0.07375295 -0.07345460  0.07949533  0.05814752
    ## PCIP45     0.02033617 -0.04781130 -0.03684120 -0.02188855 -0.01824147
    ## PCIP26     0.06478659 -0.06224470  0.02916028 -0.01777879  0.08518401
    ## DEPENDENT -0.04436080  0.14254124 -0.02278486 -0.09120423  0.08464662
    ##                   PC35         PC36        PC37         PC38        PC39
    ## FAMINC     0.083772186 -0.021934806  0.20030022  0.023904435 -0.08963279
    ## PCIP54    -0.023987486  0.117790445 -0.04129966  0.059302587 -0.04284599
    ## PCIP23    -0.043453644  0.060417993  0.03247435  0.014361754 -0.02353853
    ## PCIP45    -0.048622592  0.031252911 -0.01097531 -0.023375812 -0.06281566
    ## PCIP26    -0.004395119 -0.005102319 -0.03034406  0.057155600  0.01592351
    ## DEPENDENT  0.009743384  0.005084927 -0.03410847 -0.005555441 -0.05227665
    ##                  PC40        PC41        PC42        PC43          PC44
    ## FAMINC    -0.31877587 -0.11582928 -0.05819110  0.06715697 -1.973954e-01
    ## PCIP54    -0.08776270 -0.06010553  0.04547404 -0.08605074  1.190354e-01
    ## PCIP23    -0.06551551 -0.02427935 -0.01128911 -0.10045355  1.636021e-01
    ## PCIP45     0.08384001  0.01620190  0.03863611 -0.02668037 -6.102932e-05
    ## PCIP26     0.06529564 -0.14159268 -0.05590973  0.02645751 -1.607270e-01
    ## DEPENDENT -0.02625580 -0.02094414  0.04712229  0.04264704 -1.080062e-02
    ##                  PC45        PC46        PC47         PC48        PC49
    ## FAMINC     0.10924953 -0.12015899 0.185334486 -0.123932037 -0.36054451
    ## PCIP54    -0.15642585 -0.08574235 0.134035481 -0.036191690 -0.11060945
    ## PCIP23    -0.13166084 -0.16041347 0.017423363 -0.146684382 -0.08320363
    ## PCIP45     0.01998510  0.01404406 0.002912451  0.038761246  0.05962978
    ## PCIP26     0.04660074 -0.02383386 0.141441604 -0.007211334  0.05734720
    ## DEPENDENT  0.03730791  0.04009975 0.097681801  0.023243322  0.05622780
    ##                   PC50        PC51        PC52         PC53        PC54
    ## FAMINC    -0.124839971  0.28537023 -0.01458222  0.423527185  0.03946527
    ## PCIP54     0.024263833 -0.26543310 -0.13665053  0.216647690 -0.22836192
    ## PCIP23     0.007799333 -0.27438178  0.02076217  0.029341161 -0.17889203
    ## PCIP45    -0.100861756 -0.10251189 -0.08612402 -0.002924398 -0.11448562
    ## PCIP26     0.116153783  0.04980927 -0.17745173 -0.194100641 -0.22827570
    ## DEPENDENT  0.121645401  0.01372645 -0.12437969 -0.035880590 -0.02547992
    ##                  PC55        PC56         PC57        PC58        PC59
    ## FAMINC     0.06808158 -0.04585810 -0.115652525 -0.03427718 -0.04731343
    ## PCIP54     0.15452943  0.19001956 -0.085787281  0.01568717  0.15235985
    ## PCIP23     0.24585462  0.02813444  0.270348096  0.03827810  0.05338689
    ## PCIP45     0.02399598  0.15458545  0.039217740 -0.05769888 -0.12977867
    ## PCIP26    -0.13050266 -0.13992662 -0.076357815  0.27484684 -0.48047935
    ## DEPENDENT  0.02185536 -0.04415511 -0.004870082 -0.05699368  0.01202747
    ##                  PC60        PC61         PC62         PC63        PC64
    ## FAMINC    -0.16009718 -0.16062425 -0.146045407  0.038061178  0.01150672
    ## PCIP54     0.23712921  0.44052666 -0.176430425  0.344975006  0.17651305
    ## PCIP23    -0.10572212 -0.26998322 -0.007884318 -0.588024978 -0.17243148
    ## PCIP45    -0.23134516 -0.52493717  0.319260888  0.491893472  0.11330705
    ## PCIP26    -0.28372314  0.27039920 -0.170975148 -0.091697842 -0.07262714
    ## DEPENDENT -0.03089371 -0.04739272  0.123662402  0.001357458 -0.01081184
    ##                   PC65        PC66         PC67         PC68        PC69
    ## FAMINC     0.123178378  0.02250218 -0.036936566  0.002815633 -0.04330186
    ## PCIP54    -0.087021143  0.01827839  0.008381586 -0.040174698 -0.04656141
    ## PCIP23    -0.002905727 -0.05125412 -0.082103714  0.031562087  0.03632457
    ## PCIP45     0.199415551 -0.03407193  0.068658185 -0.101285071  0.01019389
    ## PCIP26     0.271296826  0.07263220 -0.013792849  0.065610963  0.04732229
    ## DEPENDENT -0.300774338  0.17378752 -0.010453471  0.205666959 -0.15724279
    ##                  PC70         PC71         PC72         PC73         PC74
    ## FAMINC    -0.14822597 -0.007420966 -0.034395038 -0.077173265  0.010373911
    ## PCIP54    -0.04927659  0.005872755  0.003389768  0.002736140 -0.009574507
    ## PCIP23    -0.02186083  0.026449434  0.009003132 -0.016310639  0.003509038
    ## PCIP45     0.02556785 -0.008486705 -0.029983852  0.017161040  0.007857611
    ## PCIP26     0.07731928 -0.027631966  0.008805964 -0.005813463 -0.016580948
    ## DEPENDENT  0.08393818  0.023689274  0.042194505  0.011272249 -0.041230550
    ##                   PC75 variables
    ## FAMINC    -0.001446394    FAMINC
    ## PCIP54    -0.012869094    PCIP54
    ## PCIP23    -0.003890230    PCIP23
    ## PCIP45    -0.005731686    PCIP45
    ## PCIP26     0.007208327    PCIP26
    ## DEPENDENT -0.734823923 DEPENDENT

We can do the same thing with the second principal component.

As seen below, the 6 variables most correlated with PC2 are:

1.  `PCIP15`
2.  `NUMBRANCH`
3.  `PCIP11`
4.  `PCTFLOAN`
5.  `PCTPELL`
6.  `PCIP10`

The six most negatively correlated with PC2 are:

1.  `logdebt`
2.  `FEMALE_DEBT_MDN`
3.  `FIRSTGEN_DEBT_MDN`
4.  `PELL_DEBT_MDN`
5.  `NOTFIRSTGEN_DEBT_MDN`
6.  `DEBT_MDN`

This principal component is a little less intuitive to understand, but
clearly relates somewhat to the indebtedness of the student body, in
addition to their specific major interests. Higher PC2 values may imply
more of a focus on more rewarded majors (such as engineering or computer
science).

    head(pc2.ordered.rotations)

    ##                  PC1        PC2         PC3         PC4         PC5
    ## PCIP15    0.07156320 0.16870378 -0.23735699 -0.02242540 -0.16972425
    ## NUMBRANCH 0.11458070 0.16679234 -0.18895943  0.16375944 -0.03649930
    ## PCIP11    0.10698231 0.12900916 -0.21708787  0.12975894 -0.04320222
    ## PCTFLOAN  0.09856918 0.11946093  0.21589251  0.16318089  0.12501847
    ## PCTPELL   0.20591914 0.08313005  0.06031997  0.19531526 -0.12534426
    ## PCIP10    0.02538417 0.05119268 -0.03881619 -0.04698254  0.01534491
    ##                   PC6         PC7          PC8          PC9        PC10
    ## PCIP15     0.25241503 -0.12667447 -0.007246356  0.053542051 -0.04118783
    ## NUMBRANCH  0.25912173 -0.15745096 -0.035943780  0.111601198 -0.07926777
    ## PCIP11     0.18962325 -0.05630268 -0.111675552  0.081707933  0.06878336
    ## PCTFLOAN   0.23816635  0.09037709  0.054525320 -0.062079280  0.10197171
    ## PCTPELL   -0.00930182  0.04015504  0.015278278 -0.101377827 -0.02592374
    ## PCIP10     0.07205125  0.35706348 -0.127321148  0.007590133  0.10686635
    ##                  PC11         PC12        PC13         PC14        PC15
    ## PCIP15    -0.06833581 -0.000102515 -0.04898430 -0.070414643  0.17571316
    ## NUMBRANCH  0.02950695 -0.003347530  0.01948405 -0.022386138  0.13752745
    ## PCIP11     0.01650047  0.155060959 -0.10619646  0.016442461 -0.01480851
    ## PCTFLOAN  -0.04470459 -0.143845674  0.01716113 -0.040531194  0.13604311
    ## PCTPELL   -0.01758246 -0.110667613  0.01087730  0.004057436  0.06812188
    ## PCIP10     0.13263987  0.259816061 -0.03874868 -0.106407985  0.07520314
    ##                  PC16         PC17         PC18         PC19         PC20
    ## PCIP15    -0.01900397  0.029712089  0.060850940  0.039139132  0.020413517
    ## NUMBRANCH -0.05763934  0.026859048  0.042985127  0.044646285 -0.042279693
    ## PCIP11    -0.02775168 -0.005781125  0.027185879  0.061071272 -0.064700293
    ## PCTFLOAN   0.05247837 -0.072727272 -0.002467654  0.007405134  0.026485216
    ## PCTPELL    0.01120130 -0.006296721  0.017417435 -0.021571034  0.009104071
    ## PCIP10    -0.09953089  0.126636546 -0.112095814 -0.049658239  0.189137126
    ##                  PC21        PC22         PC23         PC24         PC25
    ## PCIP15    -0.04094999 -0.02286966  0.029363069  0.029987160  0.001855846
    ## NUMBRANCH  0.01249652  0.01741226  0.012043880  0.006792262 -0.006166915
    ## PCIP11    -0.05399193 -0.07456586  0.002112556 -0.120804618  0.012102946
    ## PCTFLOAN  -0.03831445  0.03949754 -0.059543149  0.081871995  0.133706441
    ## PCTPELL   -0.03938006  0.01173773 -0.028643737  0.064007001  0.022403944
    ## PCIP10    -0.14425207 -0.07996944  0.051947537  0.017423499 -0.051545137
    ##                   PC26        PC27        PC28          PC29         PC30
    ## PCIP15    -0.002750199  0.05264789  0.01101120  0.0806360213 -0.015698563
    ## NUMBRANCH -0.051839695  0.06873736  0.03816461  0.1149932299 -0.102025534
    ## PCIP11    -0.017131824  0.01461452 -0.06326136 -0.0592886775  0.154860700
    ## PCTFLOAN   0.169512367 -0.05419895 -0.02294209 -0.0263888345 -0.060063690
    ## PCTPELL    0.058637509 -0.05273306 -0.01065296 -0.0006807837  0.009870097
    ## PCIP10     0.263125382 -0.01746344  0.26617432 -0.2467804727  0.068116112
    ##                  PC31        PC32         PC33         PC34         PC35
    ## PCIP15    -0.06579973 -0.08136100  0.006139022  0.011787155 -0.011640934
    ## NUMBRANCH -0.07144681  0.01462692  0.033987282  0.007600968 -0.028356604
    ## PCIP11    -0.01458581 -0.10048548 -0.020036357 -0.030256296  0.037096271
    ## PCTFLOAN   0.12720105 -0.07942238 -0.013944429 -0.007570948  0.044804458
    ## PCTPELL    0.07495563 -0.01675592 -0.050212894  0.026116785  0.007328866
    ## PCIP10    -0.07523700  0.28171396 -0.272226801 -0.045602183 -0.044321946
    ##                   PC36        PC37          PC38        PC39         PC40
    ## PCIP15    -0.056749094 -0.01789490 -9.984826e-03 -0.07377655  0.016672714
    ## NUMBRANCH -0.049945201 -0.04857184 -9.583635e-03  0.04932024  0.008603819
    ## PCIP11    -0.115310411  0.05046103  7.280647e-05 -0.00255334 -0.030955024
    ## PCTFLOAN   0.006399555  0.06785825  1.136608e-01 -0.15034828 -0.036150447
    ## PCTPELL   -0.008343181  0.02750662  1.843704e-02 -0.10578616  0.002569837
    ## PCIP10     0.038922451 -0.07568790  2.999100e-02  0.01330928  0.253253059
    ##                   PC41         PC42         PC43         PC44        PC45
    ## PCIP15    -0.001639155 -0.071380582  0.009328315 -0.050959160  0.07038444
    ## NUMBRANCH  0.011459028  0.011180548 -0.019991437 -0.006650922 -0.01367333
    ## PCIP11     0.052395957  0.063966674  0.165526068 -0.166736052 -0.19102491
    ## PCTFLOAN  -0.074155793  0.017302616  0.116627082  0.119099923  0.13117357
    ## PCTPELL   -0.073626873  0.001072942  0.054476963  0.070088132  0.04331290
    ## PCIP10    -0.267583996 -0.064193433 -0.147311878  0.039234248 -0.13822683
    ##                  PC46          PC47        PC48        PC49         PC50
    ## PCIP15    -0.18657314 -0.0980782618 -0.12439548 -0.03156917 -0.193682897
    ## NUMBRANCH -0.06646259  0.0299614381  0.01420557  0.06815934 -0.114393690
    ## PCIP11    -0.13851815  0.1285187704 -0.28102745 -0.05293878  0.364177629
    ## PCTFLOAN   0.11604349 -0.0726757113 -0.27827862 -0.03826154 -0.077326047
    ## PCTPELL    0.08151602 -0.0009117928 -0.07273943 -0.05508265  0.002216849
    ## PCIP10    -0.12497953  0.0411212420 -0.06837535  0.08144840 -0.116657507
    ##                   PC51        PC52        PC53        PC54        PC55
    ## PCIP15    -0.065317157 -0.07877576 -0.06989054 -0.08348678  0.03019315
    ## NUMBRANCH -0.004461537 -0.01818187 -0.01211299 -0.02796742 -0.06645847
    ## PCIP11     0.174296894 -0.07726231 -0.20476963  0.23494251  0.33970136
    ## PCTFLOAN   0.017714924  0.02352286 -0.16682441 -0.09890607 -0.08927383
    ## PCTPELL   -0.011672405  0.01210459 -0.07197423 -0.08552144 -0.07762451
    ## PCIP10     0.076603328 -0.03301353  0.04956952  0.01680330 -0.02340275
    ##                   PC56         PC57         PC58        PC59         PC60
    ## PCIP15    -0.143609696  0.115936790 -0.573088564 -0.27577146 -0.005342914
    ## NUMBRANCH -0.159643245  0.070899475  0.086362941  0.03477256 -0.018918436
    ## PCIP11     0.184873003 -0.002162711  0.182849486  0.06328900  0.011532166
    ## PCTFLOAN   0.159249269 -0.132075507  0.008385077  0.16941261 -0.220460553
    ## PCTPELL    0.100328032 -0.187097662  0.008381827  0.06747890 -0.207671566
    ## PCIP10    -0.004316435 -0.016323570 -0.036492012  0.02474837 -0.027103589
    ##                  PC61         PC62         PC63        PC64        PC65
    ## PCIP15     0.15860978  0.060037771  0.079766622 -0.14837912 -0.16090883
    ## NUMBRANCH -0.05233257 -0.091402618 -0.009705492  0.13701285  0.07621597
    ## PCIP11    -0.08266336 -0.036183623  0.119604164 -0.07577187 -0.01365125
    ## PCTFLOAN   0.03674716 -0.064757786  0.000480310  0.03516017 -0.13225034
    ## PCTPELL   -0.02577448  0.006323249 -0.033617597  0.04287397 -0.18605063
    ## PCIP10    -0.03498522 -0.036446807 -0.001746978  0.01425988  0.01492071
    ##                  PC66        PC67         PC68          PC69          PC70
    ## PCIP15     0.07659116 -0.09893321 -0.086743646  0.1770833847 -0.0288383127
    ## NUMBRANCH -0.29089888  0.29921470  0.238355888 -0.6030589289 -0.0093835056
    ## PCIP11     0.06365874  0.02831253  0.003102425  0.0918415442 -0.0055462426
    ## PCTFLOAN  -0.16421948  0.05610460 -0.105364615  0.0328084159  0.3421261324
    ## PCTPELL   -0.08209116 -0.12225961 -0.030811755 -0.0009505331 -0.4263387044
    ## PCIP10    -0.01442367  0.02003843  0.012324352 -0.0144178668  0.0005806648
    ##                   PC71        PC72        PC73          PC74          PC75
    ## PCIP15     0.017952989 -0.02147781 -0.00672535  0.0095980916  0.0087921277
    ## NUMBRANCH -0.023051294  0.03494966 -0.04573278  0.0280935406 -0.0199913219
    ## PCIP11     0.007421847  0.03272141  0.01671108  0.0079859437 -0.0075526934
    ## PCTFLOAN   0.284337232 -0.17105174 -0.05487570  0.0377531126  0.0015378060
    ## PCTPELL   -0.476748658  0.44886387  0.10605060 -0.0480214475  0.0105859115
    ## PCIP10    -0.003671616  0.01253649 -0.00572684  0.0009663354 -0.0003894056
    ##           variables
    ## PCIP15       PCIP15
    ## NUMBRANCH NUMBRANCH
    ## PCIP11       PCIP11
    ## PCTFLOAN   PCTFLOAN
    ## PCTPELL     PCTPELL
    ## PCIP10       PCIP10

    tail(pc2.ordered.rotations)

    ##                             PC1        PC2          PC3         PC4
    ## logdebt              0.05305026 -0.3111716 -0.004768342 0.008904556
    ## FEMALE_DEBT_MDN      0.07680293 -0.3128787 -0.007926427 0.012333228
    ## FIRSTGEN_DEBT_MDN    0.09731630 -0.3145954 -0.022168933 0.029908904
    ## PELL_DEBT_MDN        0.08645264 -0.3245655 -0.010929142 0.035178553
    ## NOTFIRSTGEN_DEBT_MDN 0.10304945 -0.3275289 -0.007977606 0.021064942
    ## DEBT_MDN             0.10514484 -0.3381817 -0.005091782 0.023598996
    ##                             PC5        PC6         PC7         PC8
    ## logdebt              0.09652152 0.12394441 -0.03797676 -0.05055975
    ## FEMALE_DEBT_MDN      0.03089112 0.08333342  0.02231574 -0.03695632
    ## FIRSTGEN_DEBT_MDN    0.03771932 0.06736681 -0.01251122 -0.03650895
    ## PELL_DEBT_MDN        0.07247384 0.07505106 -0.02061449 -0.03738421
    ## NOTFIRSTGEN_DEBT_MDN 0.02855510 0.06766866 -0.03753543 -0.04849122
    ## DEBT_MDN             0.03030126 0.08255497 -0.03265766 -0.06029951
    ##                               PC9        PC10        PC11        PC12
    ## logdebt              -0.002656036 0.030321503 -0.04558208 -0.04418668
    ## FEMALE_DEBT_MDN      -0.023236832 0.009126896 -0.03446675  0.01940211
    ## FIRSTGEN_DEBT_MDN    -0.000850039 0.072527546  0.04124563 -0.05240733
    ## PELL_DEBT_MDN        -0.015837813 0.064962155  0.02335008 -0.03355200
    ## NOTFIRSTGEN_DEBT_MDN -0.004132730 0.036928392  0.03590755 -0.01061026
    ## DEBT_MDN              0.007477038 0.042894274  0.04001441 -0.02870663
    ##                              PC13        PC14       PC15         PC16
    ## logdebt               0.009298141 -0.03126458 0.16970314 -0.119956658
    ## FEMALE_DEBT_MDN      -0.011089022  0.01176492 0.07374926 -0.044083518
    ## FIRSTGEN_DEBT_MDN     0.046801546 -0.01396225 0.02867684 -0.006396330
    ## PELL_DEBT_MDN         0.028131167 -0.03049008 0.06661598 -0.076272175
    ## NOTFIRSTGEN_DEBT_MDN  0.017745575 -0.05087042 0.01385498  0.018390273
    ## DEBT_MDN              0.025035589 -0.04755148 0.03620346 -0.004851396
    ##                              PC17        PC18          PC19         PC20
    ## logdebt              -0.030957245 -0.05150151 -0.0623400982 -0.052529836
    ## FEMALE_DEBT_MDN      -0.002305317  0.03203695  0.0339369500  0.009258347
    ## FIRSTGEN_DEBT_MDN     0.062281464 -0.02109809  0.0335015828 -0.032740407
    ## PELL_DEBT_MDN         0.041743255 -0.05382442  0.0136776129 -0.017306270
    ## NOTFIRSTGEN_DEBT_MDN  0.031169461 -0.01108951 -0.0005366773 -0.010662338
    ## DEBT_MDN              0.028034798 -0.01716191  0.0141522774 -0.027598769
    ##                             PC21       PC22         PC23       PC24
    ## logdebt               0.03214298 0.03194703 -0.037634915 0.05067116
    ## FEMALE_DEBT_MDN       0.01129720 0.02197733 -0.041430712 0.04444821
    ## FIRSTGEN_DEBT_MDN    -0.03102955 0.03751983  0.006461059 0.04374379
    ## PELL_DEBT_MDN        -0.02515889 0.04311825 -0.010559016 0.04924463
    ## NOTFIRSTGEN_DEBT_MDN -0.03504786 0.02241965  0.006888536 0.03107672
    ## DEBT_MDN             -0.03869609 0.03612174  0.008860936 0.03584713
    ##                             PC25         PC26         PC27       PC28
    ## logdebt              -0.12550587 -0.026919151 -0.137605506 0.05289192
    ## FEMALE_DEBT_MDN      -0.06339165 -0.006187396 -0.010620488 0.03409641
    ## FIRSTGEN_DEBT_MDN    -0.07183889 -0.036274294  0.017989568 0.03915396
    ## PELL_DEBT_MDN        -0.09105534 -0.051595517 -0.004380236 0.03959692
    ## NOTFIRSTGEN_DEBT_MDN -0.05298815 -0.015416401 -0.012750618 0.01163435
    ## DEBT_MDN             -0.06689295 -0.024828022 -0.027356185 0.02014815
    ##                              PC29         PC30         PC31        PC32
    ## logdebt              -0.003233186  0.035813349 0.0352158741 -0.03929501
    ## FEMALE_DEBT_MDN       0.062625432  0.058995406 0.0004180349 -0.04737210
    ## FIRSTGEN_DEBT_MDN     0.038671508  0.020000806 0.0496165644  0.05076066
    ## PELL_DEBT_MDN        -0.000576657  0.007730956 0.0626775454 -0.03258613
    ## NOTFIRSTGEN_DEBT_MDN -0.001463625 -0.017081380 0.0034697718  0.01942553
    ## DEBT_MDN              0.021133069 -0.004210557 0.0220454530  0.03657487
    ##                              PC33          PC34         PC35        PC36
    ## logdebt              -0.125639661 -0.0322249493  0.139586248 -0.08415118
    ## FEMALE_DEBT_MDN      -0.016389179  0.0308405404  0.116695638 -0.10159132
    ## FIRSTGEN_DEBT_MDN    -0.032393679 -0.0245026600 -0.024142558  0.04230989
    ## PELL_DEBT_MDN        -0.065421380  0.0095537135  0.071494840 -0.04987817
    ## NOTFIRSTGEN_DEBT_MDN  0.007751193 -0.0145703894 -0.004260731 -0.02680676
    ## DEBT_MDN             -0.034482615  0.0001278408 -0.007764036 -0.02647425
    ##                              PC37         PC38        PC39         PC40
    ## logdebt               0.038997840 -0.020684622 -0.05055604 -0.068208059
    ## FEMALE_DEBT_MDN       0.053521620 -0.076271292  0.06367392  0.047717570
    ## FIRSTGEN_DEBT_MDN     0.021913609  0.035347106 -0.03395517 -0.014940945
    ## PELL_DEBT_MDN         0.019660496  0.004153146  0.02874174 -0.001106973
    ## NOTFIRSTGEN_DEBT_MDN -0.013618279  0.004102726 -0.08029346 -0.015510417
    ## DEBT_MDN             -0.003815062  0.025143392 -0.08880715 -0.042644123
    ##                              PC41         PC42         PC43         PC44
    ## logdebt              -0.018067139 -0.143915552  0.062399002  0.040047532
    ## FEMALE_DEBT_MDN       0.070374335 -0.036289744  0.130994064 -0.036427532
    ## FIRSTGEN_DEBT_MDN     0.004471643  0.005224307 -0.006687439 -0.037001393
    ## PELL_DEBT_MDN         0.029337670 -0.026253293  0.051522489 -0.006416381
    ## NOTFIRSTGEN_DEBT_MDN  0.006336696  0.002706020 -0.017755706 -0.009644850
    ## DEBT_MDN             -0.010543805 -0.041176855  0.008718807  0.006114518
    ##                             PC45        PC46         PC47        PC48
    ## logdebt              -0.02623407 -0.02997877 -0.036091023 -0.04401070
    ## FEMALE_DEBT_MDN      -0.15915271 -0.25934832 -0.194193272  0.08877395
    ## FIRSTGEN_DEBT_MDN     0.02493447  0.06706351 -0.000374159  0.04061699
    ## PELL_DEBT_MDN        -0.03709610 -0.03814469 -0.013670526  0.01214824
    ## NOTFIRSTGEN_DEBT_MDN  0.03000470  0.03330137  0.022081295 -0.02803872
    ## DEBT_MDN              0.03972126  0.02606591  0.015135780 -0.02220652
    ##                              PC49         PC50        PC51         PC52
    ## logdebt               0.061631198  0.040350758 -0.09567063  0.047571337
    ## FEMALE_DEBT_MDN      -0.099142577 -0.073352683  0.07793707 -0.029431297
    ## FIRSTGEN_DEBT_MDN     0.008693598  0.028218942 -0.02135674 -0.108320450
    ## PELL_DEBT_MDN         0.034046694  0.026104708  0.01368305 -0.091639912
    ## NOTFIRSTGEN_DEBT_MDN -0.052804387  0.005492193 -0.01736096  0.036997673
    ## DEBT_MDN              0.007101724  0.020135762 -0.03327946 -0.004451822
    ##                             PC53        PC54         PC55         PC56
    ## logdebt              -0.09523842 -0.03181554 -0.040230468  0.053059019
    ## FEMALE_DEBT_MDN       0.05308450  0.05328257  0.022685264 -0.051797326
    ## FIRSTGEN_DEBT_MDN     0.10034093  0.03139829  0.079445911  0.004381603
    ## PELL_DEBT_MDN         0.06289006  0.01843863  0.030843828  0.021665441
    ## NOTFIRSTGEN_DEBT_MDN -0.03388274 -0.06887396 -0.026547594  0.055301143
    ## DEBT_MDN             -0.02129460 -0.05179664 -0.008470584  0.029897824
    ##                              PC57         PC58        PC59        PC60
    ## logdebt              -0.137119288  0.005951266 -0.21557003  0.36206127
    ## FEMALE_DEBT_MDN       0.200241690  0.097849579  0.05021692 -0.29042599
    ## FIRSTGEN_DEBT_MDN     0.005169327 -0.077610195  0.23877711 -0.17829163
    ## PELL_DEBT_MDN         0.038061154 -0.002294520  0.06404337 -0.13620189
    ## NOTFIRSTGEN_DEBT_MDN -0.033593245 -0.061371270 -0.12033581  0.05913483
    ## DEBT_MDN             -0.089895356 -0.030035546 -0.11131119  0.18069797
    ##                            PC61       PC62         PC63        PC64
    ## logdebt              -0.2269501 -0.2126635  0.070430473  0.01183878
    ## FEMALE_DEBT_MDN       0.1810924  0.1917104 -0.060836559  0.57955091
    ## FIRSTGEN_DEBT_MDN     0.2115243  0.2292561  0.136271433 -0.51190992
    ## PELL_DEBT_MDN         0.1508775  0.1634161 -0.068396212 -0.22092900
    ## NOTFIRSTGEN_DEBT_MDN -0.0979943 -0.1175096 -0.101005654  0.12600104
    ## DEBT_MDN             -0.1219005 -0.1218521 -0.001004926 -0.02835049
    ##                             PC65        PC66        PC67        PC68
    ## logdebt               0.01185406 -0.12335397 -0.41020838  0.07243824
    ## FEMALE_DEBT_MDN      -0.07881950 -0.04639280 -0.15707325  0.12689715
    ## FIRSTGEN_DEBT_MDN     0.17487278 -0.31892939  0.01687355  0.39215995
    ## PELL_DEBT_MDN         0.06039853  0.25846170  0.05582641 -0.62971570
    ## NOTFIRSTGEN_DEBT_MDN -0.13972815  0.16126847  0.71802490  0.02692056
    ## DEBT_MDN             -0.01029820 -0.01444815  0.04868256  0.09730515
    ##                             PC69        PC70        PC71        PC72
    ## logdebt              -0.12975899  0.06545821 -0.01212664 -0.02501440
    ## FEMALE_DEBT_MDN       0.14368610  0.05985330 -0.01023874 -0.01931630
    ## FIRSTGEN_DEBT_MDN     0.14051010 -0.01279805 -0.02610178 -0.01068623
    ## PELL_DEBT_MDN        -0.43419306 -0.02721241  0.03095754  0.04445391
    ## NOTFIRSTGEN_DEBT_MDN  0.26194014 -0.16219042  0.01693489 -0.01969276
    ## DEBT_MDN              0.06727266  0.01231701 -0.02592619  0.03823083
    ##                              PC73         PC74          PC75
    ## logdebt              -0.103970141 -0.358120220  0.0215196903
    ## FEMALE_DEBT_MDN       0.003081966  0.034779647 -0.0007291892
    ## FIRSTGEN_DEBT_MDN    -0.055030955 -0.122357358  0.0010735579
    ## PELL_DEBT_MDN         0.033383186 -0.002840099  0.0027359904
    ## NOTFIRSTGEN_DEBT_MDN -0.104950081 -0.302789595  0.0002270032
    ## DEBT_MDN              0.224077884  0.812536013 -0.0382035479
    ##                                 variables
    ## logdebt                           logdebt
    ## FEMALE_DEBT_MDN           FEMALE_DEBT_MDN
    ## FIRSTGEN_DEBT_MDN       FIRSTGEN_DEBT_MDN
    ## PELL_DEBT_MDN               PELL_DEBT_MDN
    ## NOTFIRSTGEN_DEBT_MDN NOTFIRSTGEN_DEBT_MDN
    ## DEBT_MDN                         DEBT_MDN

Imputation
----------

As you might note in our exploratory data analysis, there wass quite a
bit of missing data in our original dataset. While our final dataset is
significantly better, with 98% of observations present, one way to get
around this is to impute the missing observations using the other
predictors.

We use two different methods to impute missing data: (1) multiple
imputation using the `mice` package, which uses predictive mean matching
and logistic regression to perform multiple imputation on continuous and
categorical data; and (2) imputation using `missForest`, which uses a
random forest model to predict each missing observation, no matter the
type thereof. Note that both of these techniques assume that the missing
data is "missing at random" (MAR). While this is more likely the case
after our data cleaning, this is still likely a stretch, so any results
generated from the imputed data should be taken with a grain of salt.

### Imputation with `mice`

In order to impute our missing values using the `mice` package, we need
to impute the continuous variables and categorical variables separately.
To impute the continuous variables, we will use predictive mean
matching. Fortunately for us, there are no missing observations for the
categorical variables, so we do not have to impute anything there.

Below we impute the continuous variables.

    numerics.mice <- mice(numerics.section, m=5, maxit = 30, method = 'pmm', seed = 500)

    ## 
    ##  iter imp variable
    ##   1   1  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   1   2  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   1   3  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   1   4  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   1   5  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   2   1  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   2   2  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   2   3  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   2   4  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   2   5  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   3   1  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   3   2  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   3   3  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   3   4  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   3   5  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   4   1  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   4   2  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   4   3  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   4   4  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   4   5  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   5   1  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   5   2  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   5   3  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   5   4  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   5   5  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   6   1  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   6   2  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   6   3  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   6   4  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   6   5  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   7   1  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   7   2  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   7   3  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   7   4  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   7   5  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   8   1  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   8   2  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   8   3  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   8   4  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   8   5  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   9   1  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   9   2  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   9   3  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   9   4  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   9   5  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   10   1  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   10   2  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   10   3  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   10   4  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   10   5  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   11   1  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   11   2  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   11   3  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   11   4  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   11   5  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   12   1  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   12   2  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   12   3  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   12   4  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   12   5  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   13   1  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   13   2  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   13   3  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   13   4  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   13   5  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   14   1  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   14   2  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   14   3  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   14   4  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   14   5  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   15   1  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   15   2  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   15   3  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   15   4  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   15   5  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   16   1  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   16   2  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   16   3  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   16   4  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   16   5  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   17   1  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   17   2  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   17   3  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   17   4  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   17   5  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   18   1  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   18   2  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   18   3  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   18   4  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   18   5  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   19   1  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   19   2  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   19   3  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   19   4  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   19   5  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   20   1  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   20   2  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   20   3  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   20   4  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   20   5  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   21   1  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   21   2  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   21   3  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   21   4  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   21   5  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   22   1  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   22   2  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   22   3  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   22   4  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   22   5  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   23   1  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   23   2  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   23   3  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   23   4  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   23   5  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   24   1  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   24   2  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   24   3  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   24   4  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   24   5  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   25   1  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   25   2  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   25   3  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   25   4  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   25   5  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   26   1  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   26   2  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   26   3  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   26   4  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   26   5  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   27   1  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   27   2  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   27   3  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   27   4  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   27   5  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   28   1  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   28   2  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   28   3  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   28   4  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   28   5  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   29   1  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   29   2  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   29   3  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   29   4  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   29   5  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   30   1  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   30   2  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   30   3  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   30   4  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt
    ##   30   5  PCIP01  PCIP03  PCIP04  PCIP05  PCIP09  PCIP10  PCIP11  PCIP12  PCIP13  PCIP14  PCIP15  PCIP16  PCIP19  PCIP22  PCIP23  PCIP24  PCIP25  PCIP26  PCIP27  PCIP29  PCIP30  PCIP31  PCIP38  PCIP39  PCIP40  PCIP41  PCIP42  PCIP43  PCIP44  PCIP45  PCIP46  PCIP47  PCIP48  PCIP49  PCIP50  PCIP51  PCIP52  PCIP54  UGDS  UGDS_WHITE  UGDS_BLACK  UGDS_HISP  UGDS_ASIAN  UGDS_AIAN  UGDS_NHPI  UGDS_2MOR  UGDS_NRA  UGDS_UNKN  PPTUG_EF  PCTPELL  INEXPFTE  TUITFTE  PCTFLOAN  WDRAW_ORIG_YR4_RT  DEBT_MDN  LO_INC_DEBT_MDN  MD_INC_DEBT_MDN  HI_INC_DEBT_MDN  PELL_DEBT_MDN  NOPELL_DEBT_MDN  FEMALE_DEBT_MDN  MALE_DEBT_MDN  FIRSTGEN_DEBT_MDN  NOTFIRSTGEN_DEBT_MDN  FEMALE  LOAN_EVER  PELL_EVER  AGE_ENTRY  DEPENDENT  FIRST_GEN  FAMINC  logdebt

    numerics.mice.df1 <- complete(numerics.mice, 1)
    numerics.mice.df2 <- complete(numerics.mice, 2)
    numerics.mice.df3 <- complete(numerics.mice, 3)
    numerics.mice.df4 <- complete(numerics.mice, 4)
    numerics.mice.df5 <- complete(numerics.mice, 5)

    # Create empty dataframe
    df_numerics.mice <- data.frame(matrix(NA, nrow = 2634, ncol = 75))

    # Replace cells with the mean of the 5 imputed dataframes
    for (i in 1:2634) {
      for (j in 1:75) {
        df_numerics.mice[i,j] = mean(numerics.mice.df1[i,j],
                                     numerics.mice.df2[i,j],
                                     numerics.mice.df3[i,j],
                                     numerics.mice.df4[i,j],
                                     numerics.mice.df5[i,j])
      }
    }

    # Rename columns to the correct names
    for (i in 1:75){
      names(df_numerics.mice)[i] = names(numerics)[i]
    }

    df.final.mice <- cbind.data.frame(INSTNM = df_final_section$INSTNM,
                                      df_numerics.mice,
                                      categoricals[1:2634,])

    df.final.mice <- df.final.mice %>%
      mutate(INSTNM = as.character(INSTNM))

### Multiple imputation with `missForest`

Below, we use the `missForest` package to impute our missing data.
Unlike the `mice` package, we can do this imputation in one step, which
makes it much more simple.

    nonchars.section <- cbind(numerics.section,
                              categoricals[1:2634,])
    missForest.imputation <- missForest(nonchars.section)

    ##   missForest iteration 1 in progress...done!
    ##   missForest iteration 2 in progress...done!
    ##   missForest iteration 3 in progress...done!
    ##   missForest iteration 4 in progress...done!
    ##   missForest iteration 5 in progress...done!
    ##   missForest iteration 6 in progress...done!
    ##   missForest iteration 7 in progress...done!

    missForest_df <- cbind.data.frame(INSTNM = df_final_section$INSTNM,
                                      missForest.imputation$ximp)

Modeling
--------

With our data now in workable format, with two different ways of
imputing missingness, we now turn to modeling our data.

### Linear models

Following the literature cited in our introduction, we built a
theoretical model on what might impact graduates' earnings. This model
is described with the equation below.

$$median \\space earnings = \\alpha + \\beta\_1 college \\space characteristics + \\beta\_2 student \\space demographics + \\epsilon$$

To capture **college characteristics**, we use the following variables:
- percentage of degrees awarded in many different majors (`PCIP01`,
`PCIP02`, etc.) - size of the school (`UGDS`) - instructional
expenditures per student (`INEXPFTE`) - region of the school (`REGION`)
- share of students that are part-time (`PPTUG_EF`) - tuition revenue
per student (`TUITFTE`) - ownership of school (`CONTROL`) - highest
degree awarded (`HIGHDEG`)

To capture **student demographics**, we use the following variables from
our dataset: - racial makeup of the school (`UGDS_WHITE`) - debt after
graduation (`DEBT_MDN`) - gender makeup of school (`FEMALE`) - age
makeup of school (`AGE_ENTRY`) - share of students that are
first-generation (`FIRST_GEN`) - median family income (`FAMINC`) - share
of students who have received a loan (`LOAN_EVER`) - share of students
who have received a Pell Grant (`PELL_EVER`)

#### Without imputation

Since we essentially have three datasets with these variables, it's an
interesting exercise to see how the results of our analysis might differ
with and without imputation. To start, we'll run a linear model of the
above specification on the dataset without any imputation.

    linear.model.1 <- lm(log_md_wage ~ 
                           PCIP01 +
                           PCIP03 +
                           PCIP04 +
                           PCIP05 +
                           PCIP09 +
                           PCIP10 +
                           PCIP11 +
                           PCIP12 +
                           PCIP13 + 
                           PCIP14 +
                           PCIP15 +
                           PCIP16 +
                           PCIP19 +
                           PCIP22 +
                           PCIP23 +
                           PCIP24 +
                           PCIP25 +
                           PCIP26 +
                           PCIP27 +
                           PCIP29 +
                           PCIP30 +
                           PCIP31 + 
                           PCIP38 +
                           PCIP39 +
                           PCIP40 +
                           PCIP41 +
                           PCIP42 +
                           PCIP43 +
                           PCIP44 +
                           PCIP45 +
                           PCIP46 +
                           PCIP47 +
                           PCIP48 +
                           PCIP49 +
                           PCIP50 +
                           PCIP51 +
                           PCIP52 +
                           PCIP54+
                           UGDS +
                           UGDS_WHITE +
                           INEXPFTE +
                           logdebt +
                           FEMALE +
                           AGE_ENTRY +
                           FIRST_GEN +
                           REGION +
                           PPTUG_EF +
                           TUITFTE + 
                           LOAN_EVER +
                           FAMINC +
                           CONTROL +
                           PELL_EVER +
                           HIGHDEG, 
                         data = df_final_section)
    summary(linear.model.1)

    ## 
    ## Call:
    ## lm(formula = log_md_wage ~ PCIP01 + PCIP03 + PCIP04 + PCIP05 + 
    ##     PCIP09 + PCIP10 + PCIP11 + PCIP12 + PCIP13 + PCIP14 + PCIP15 + 
    ##     PCIP16 + PCIP19 + PCIP22 + PCIP23 + PCIP24 + PCIP25 + PCIP26 + 
    ##     PCIP27 + PCIP29 + PCIP30 + PCIP31 + PCIP38 + PCIP39 + PCIP40 + 
    ##     PCIP41 + PCIP42 + PCIP43 + PCIP44 + PCIP45 + PCIP46 + PCIP47 + 
    ##     PCIP48 + PCIP49 + PCIP50 + PCIP51 + PCIP52 + PCIP54 + UGDS + 
    ##     UGDS_WHITE + INEXPFTE + logdebt + FEMALE + AGE_ENTRY + FIRST_GEN + 
    ##     REGION + PPTUG_EF + TUITFTE + LOAN_EVER + FAMINC + CONTROL + 
    ##     PELL_EVER + HIGHDEG, data = df_final_section)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.9472 -0.1018  0.0078  0.1161  1.2699 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  6.119e+00  1.212e-01  50.497  < 2e-16 ***
    ## PCIP01      -1.138e-04  8.446e-05  -1.347 0.178105    
    ## PCIP03      -3.000e-04  7.413e-05  -4.046 5.37e-05 ***
    ## PCIP04       3.843e-04  1.936e-04   1.985 0.047261 *  
    ## PCIP05       1.688e-04  1.590e-04   1.062 0.288458    
    ## PCIP09       4.957e-05  2.710e-05   1.829 0.067518 .  
    ## PCIP10      -2.172e-05  8.842e-05  -0.246 0.805971    
    ## PCIP11       8.462e-05  2.150e-05   3.936 8.53e-05 ***
    ## PCIP12      -5.913e-05  4.482e-05  -1.319 0.187291    
    ## PCIP13      -6.878e-05  1.823e-05  -3.774 0.000165 ***
    ## PCIP14       3.094e-04  4.726e-05   6.547 7.23e-11 ***
    ## PCIP15       1.176e-04  2.931e-05   4.011 6.23e-05 ***
    ## PCIP16       6.071e-05  9.395e-05   0.646 0.518244    
    ## PCIP19      -9.318e-05  4.292e-05  -2.171 0.030028 *  
    ## PCIP22      -1.623e-04  5.251e-05  -3.091 0.002016 ** 
    ## PCIP23      -7.243e-05  5.129e-05  -1.412 0.158040    
    ## PCIP24      -1.418e-05  2.008e-05  -0.706 0.480333    
    ## PCIP25      -2.053e-03  2.797e-03  -0.734 0.463103    
    ## PCIP26       5.294e-05  3.237e-05   1.636 0.102037    
    ## PCIP27       1.086e-04  9.029e-05   1.203 0.229215    
    ## PCIP29       6.544e-03  6.767e-03   0.967 0.333686    
    ## PCIP30      -1.459e-05  3.262e-05  -0.447 0.654671    
    ## PCIP31      -5.466e-05  3.057e-05  -1.788 0.073917 .  
    ## PCIP38      -8.022e-05  1.002e-04  -0.801 0.423377    
    ## PCIP39      -4.084e-04  1.178e-04  -3.467 0.000536 ***
    ## PCIP40       6.622e-05  6.655e-05   0.995 0.319796    
    ## PCIP41       5.798e-04  5.583e-04   1.039 0.299080    
    ## PCIP42       2.116e-05  2.318e-05   0.913 0.361464    
    ## PCIP43      -4.023e-06  1.582e-05  -0.254 0.799264    
    ## PCIP44      -6.749e-06  3.265e-05  -0.207 0.836267    
    ## PCIP45       5.449e-05  3.279e-05   1.661 0.096758 .  
    ## PCIP46      -2.583e-04  1.670e-04  -1.547 0.122080    
    ## PCIP47       9.950e-05  7.933e-05   1.254 0.209868    
    ## PCIP48      -2.831e-04  1.407e-04  -2.013 0.044231 *  
    ## PCIP49       4.888e-05  1.667e-04   0.293 0.769443    
    ## PCIP50      -1.341e-04  2.180e-05  -6.151 9.05e-10 ***
    ## PCIP51      -1.325e-05  9.181e-06  -1.443 0.149022    
    ## PCIP52       1.347e-05  9.287e-06   1.450 0.147152    
    ## PCIP54       6.937e-05  7.104e-05   0.976 0.328977    
    ## UGDS        -7.054e-06  5.972e-06  -1.181 0.237663    
    ## UGDS_WHITE  -3.347e-05  6.520e-06  -5.133 3.09e-07 ***
    ## INEXPFTE     5.968e-07  3.209e-06   0.186 0.852475    
    ## logdebt      1.365e-03  5.611e-03   0.243 0.807839    
    ## FEMALE      -9.655e-06  7.236e-06  -1.334 0.182238    
    ## AGE_ENTRY    5.413e-05  6.664e-06   8.123 7.34e-16 ***
    ## FIRST_GEN    3.281e-05  1.217e-05   2.695 0.007087 ** 
    ## REGION2      7.218e-03  2.367e-02   0.305 0.760423    
    ## REGION3     -2.198e-02  2.428e-02  -0.905 0.365401    
    ## REGION4      4.641e-03  2.661e-02   0.174 0.861588    
    ## REGION5     -6.596e-02  2.377e-02  -2.774 0.005577 ** 
    ## REGION6      3.129e-02  2.800e-02   1.117 0.264011    
    ## REGION7     -1.545e-02  3.335e-02  -0.463 0.643181    
    ## REGION8      3.167e-02  2.608e-02   1.214 0.224759    
    ## REGION9     -7.098e-01  5.311e-02 -13.365  < 2e-16 ***
    ## PPTUG_EF     2.738e-05  7.003e-06   3.910 9.48e-05 ***
    ## TUITFTE     -6.860e-06  3.113e-06  -2.204 0.027635 *  
    ## LOAN_EVER   -5.470e-05  8.614e-06  -6.350 2.58e-10 ***
    ## FAMINC      -2.640e-06  4.756e-06  -0.555 0.578870    
    ## CONTROL2    -6.357e-03  1.719e-02  -0.370 0.711525    
    ## CONTROL3    -2.963e-02  2.600e-02  -1.140 0.254490    
    ## PELL_EVER   -3.553e-04  1.276e-05 -27.847  < 2e-16 ***
    ## HIGHDEG1    -1.733e-01  1.939e-01  -0.894 0.371557    
    ## HIGHDEG2    -1.544e-01  1.024e-01  -1.507 0.131837    
    ## HIGHDEG3    -1.138e-01  9.983e-02  -1.140 0.254519    
    ## HIGHDEG4    -7.889e-02  9.997e-02  -0.789 0.430128    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.235 on 2304 degrees of freedom
    ##   (265 observations deleted due to missingness)
    ## Multiple R-squared:  0.679,  Adjusted R-squared:  0.6701 
    ## F-statistic: 76.15 on 64 and 2304 DF,  p-value: < 2.2e-16

    #Run model diagnostics

    #Residual plot
    plot(linear.model.1, 1)

![](technical-report_files/figure-markdown_strict/Linear%20model%20without%20imputation-1.png)

    #QQ [;pt
    plot(linear.model.1, 2)

![](technical-report_files/figure-markdown_strict/Linear%20model%20without%20imputation-2.png)

    #Scale location plot
    plot(linear.model.1, 3)

![](technical-report_files/figure-markdown_strict/Linear%20model%20without%20imputation-3.png)

    #Residual 2 
    plot(linear.model.1, 4)

![](technical-report_files/figure-markdown_strict/Linear%20model%20without%20imputation-4.png)

    #Quartet
    par(mfrow = c(2, 2))
    plot(linear.model.1)

![](technical-report_files/figure-markdown_strict/Linear%20model%20without%20imputation-5.png)

    # #Check for multicollinearity
    # ols_vif_tol(linear.model.1)
    # corr_matrix <- cor(linear.model.1, use = "complete.obs")

    linear.model.2 <- lm(log_md_wage ~ 
                           PCIP01 +
                           PCIP03 +
                           PCIP04 +
                           PCIP05 +
                           PCIP09 +
                           PCIP10 +
                           PCIP11 +
                           PCIP12 +
                           PCIP13 + 
                           PCIP14 +
                           PCIP15 +
                           PCIP16 +
                           PCIP19 +
                           PCIP22 +
                           PCIP23 +
                           PCIP24 +
                           PCIP25 +
                           PCIP26 +
                           PCIP27 +
                           PCIP29 +
                           PCIP30 +
                           PCIP31 + 
                           PCIP38 +
                           PCIP39 +
                           PCIP40 +
                           PCIP41 +
                           PCIP42 +
                           PCIP43 +
                           PCIP44 +
                           PCIP45 +
                           PCIP46 +
                           PCIP47 +
                           PCIP48 +
                           PCIP49 +
                           PCIP50 +
                           PCIP51 +
                           PCIP52 +
                           PCIP54+
                           UGDS +
                           UGDS_WHITE +
                           INEXPFTE +
                           logdebt +
                           FEMALE +
                           AGE_ENTRY +
                           FIRST_GEN +
                           REGION +
                           PPTUG_EF +
                           TUITFTE + 
                           LOAN_EVER +
                           FAMINC +
                           CONTROL +
                           PELL_EVER +
                           HIGHDEG, 
                         data = df.final.mice)
    summary(linear.model.2)

    ## 
    ## Call:
    ## lm(formula = log_md_wage ~ PCIP01 + PCIP03 + PCIP04 + PCIP05 + 
    ##     PCIP09 + PCIP10 + PCIP11 + PCIP12 + PCIP13 + PCIP14 + PCIP15 + 
    ##     PCIP16 + PCIP19 + PCIP22 + PCIP23 + PCIP24 + PCIP25 + PCIP26 + 
    ##     PCIP27 + PCIP29 + PCIP30 + PCIP31 + PCIP38 + PCIP39 + PCIP40 + 
    ##     PCIP41 + PCIP42 + PCIP43 + PCIP44 + PCIP45 + PCIP46 + PCIP47 + 
    ##     PCIP48 + PCIP49 + PCIP50 + PCIP51 + PCIP52 + PCIP54 + UGDS + 
    ##     UGDS_WHITE + INEXPFTE + logdebt + FEMALE + AGE_ENTRY + FIRST_GEN + 
    ##     REGION + PPTUG_EF + TUITFTE + LOAN_EVER + FAMINC + CONTROL + 
    ##     PELL_EVER + HIGHDEG, data = df.final.mice)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.9603 -0.1154  0.0126  0.1327  1.8871 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  6.724e+00  3.161e-01  21.272  < 2e-16 ***
    ## PCIP01      -1.240e-04  9.843e-05  -1.259 0.208044    
    ## PCIP03      -3.228e-04  8.345e-05  -3.869 0.000112 ***
    ## PCIP04       8.482e-04  2.175e-04   3.900 9.86e-05 ***
    ## PCIP05      -1.016e-04  1.719e-04  -0.591 0.554314    
    ## PCIP09       8.388e-05  3.117e-05   2.691 0.007168 ** 
    ## PCIP10       1.057e-04  1.009e-04   1.048 0.294846    
    ## PCIP11       1.342e-04  2.470e-05   5.432 6.11e-08 ***
    ## PCIP12      -9.456e-05  5.156e-05  -1.834 0.066785 .  
    ## PCIP13      -3.975e-05  2.056e-05  -1.933 0.053331 .  
    ## PCIP14       3.730e-04  5.410e-05   6.896 6.73e-12 ***
    ## PCIP15       1.023e-04  3.374e-05   3.031 0.002462 ** 
    ## PCIP16       1.458e-04  1.065e-04   1.369 0.170986    
    ## PCIP19      -5.279e-05  5.026e-05  -1.050 0.293736    
    ## PCIP22      -1.707e-04  6.108e-05  -2.795 0.005228 ** 
    ## PCIP23      -8.175e-05  5.833e-05  -1.402 0.161164    
    ## PCIP24       2.476e-05  2.177e-05   1.138 0.255433    
    ## PCIP25      -2.098e-03  3.386e-03  -0.620 0.535507    
    ## PCIP26       5.700e-05  3.618e-05   1.575 0.115293    
    ## PCIP27       2.102e-04  1.038e-04   2.025 0.042948 *  
    ## PCIP29      -2.087e-02  7.942e-03  -2.627 0.008662 ** 
    ## PCIP30       5.903e-06  3.729e-05   0.158 0.874238    
    ## PCIP31      -5.889e-05  3.580e-05  -1.645 0.100099    
    ## PCIP38       3.360e-06  1.137e-04   0.030 0.976417    
    ## PCIP39      -3.971e-04  1.265e-04  -3.139 0.001714 ** 
    ## PCIP40       1.221e-04  7.665e-05   1.593 0.111373    
    ## PCIP41       4.710e-04  6.530e-04   0.721 0.470823    
    ## PCIP42       6.133e-05  2.660e-05   2.305 0.021228 *  
    ## PCIP43       1.275e-05  1.868e-05   0.683 0.494817    
    ## PCIP44      -5.849e-06  3.804e-05  -0.154 0.877816    
    ## PCIP45       7.089e-05  3.793e-05   1.869 0.061728 .  
    ## PCIP46      -9.476e-04  1.544e-04  -6.135 9.81e-10 ***
    ## PCIP47       3.656e-04  8.341e-05   4.383 1.22e-05 ***
    ## PCIP48       3.486e-06  1.490e-04   0.023 0.981338    
    ## PCIP49      -4.233e-05  1.899e-04  -0.223 0.823628    
    ## PCIP50      -1.451e-04  2.413e-05  -6.013 2.08e-09 ***
    ## PCIP51      -1.187e-06  1.019e-05  -0.116 0.907281    
    ## PCIP52       2.932e-05  1.044e-05   2.809 0.005005 ** 
    ## PCIP54       1.193e-04  8.227e-05   1.450 0.147168    
    ## UGDS         6.477e-07  6.875e-06   0.094 0.924957    
    ## UGDS_WHITE  -4.286e-05  7.154e-06  -5.991 2.38e-09 ***
    ## INEXPFTE     5.023e-06  3.651e-06   1.376 0.169009    
    ## logdebt     -3.842e-03  6.383e-03  -0.602 0.547288    
    ## FEMALE      -1.016e-05  8.022e-06  -1.267 0.205335    
    ## AGE_ENTRY    7.224e-05  7.189e-06  10.049  < 2e-16 ***
    ## FIRST_GEN    6.223e-05  1.311e-05   4.748 2.17e-06 ***
    ## REGION1     -5.991e-01  2.930e-01  -2.045 0.041002 *  
    ## REGION2     -6.031e-01  2.926e-01  -2.061 0.039425 *  
    ## REGION3     -5.947e-01  2.928e-01  -2.032 0.042302 *  
    ## REGION4     -5.766e-01  2.929e-01  -1.969 0.049093 *  
    ## REGION5     -6.142e-01  2.926e-01  -2.099 0.035895 *  
    ## REGION6     -5.460e-01  2.930e-01  -1.864 0.062490 .  
    ## REGION7     -5.687e-01  2.937e-01  -1.936 0.052920 .  
    ## REGION8     -5.361e-01  2.928e-01  -1.831 0.067246 .  
    ## REGION9     -1.071e+00  2.960e-01  -3.619 0.000302 ***
    ## PPTUG_EF     2.958e-05  7.920e-06   3.735 0.000192 ***
    ## TUITFTE     -8.840e-06  3.592e-06  -2.461 0.013921 *  
    ## LOAN_EVER   -3.388e-05  9.535e-06  -3.554 0.000387 ***
    ## FAMINC       1.282e-06  5.037e-06   0.255 0.799098    
    ## CONTROL2    -3.531e-02  1.915e-02  -1.844 0.065284 .  
    ## CONTROL3    -4.872e-03  2.866e-02  -0.170 0.865009    
    ## PELL_EVER   -4.075e-04  1.382e-05 -29.495  < 2e-16 ***
    ## HIGHDEG1    -3.058e-01  1.589e-01  -1.925 0.054368 .  
    ## HIGHDEG2    -2.640e-01  1.031e-01  -2.560 0.010520 *  
    ## HIGHDEG3    -2.370e-01  9.915e-02  -2.390 0.016901 *  
    ## HIGHDEG4    -2.351e-01  9.917e-02  -2.371 0.017821 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.2849 on 2568 degrees of freedom
    ## Multiple R-squared:  0.6531, Adjusted R-squared:  0.6443 
    ## F-statistic: 74.37 on 65 and 2568 DF,  p-value: < 2.2e-16

    #Run model diagnostics

    #Residual plot
    plot(linear.model.2, 1)

![](technical-report_files/figure-markdown_strict/Linear%20model%20with%20mice%20imputation-1.png)

    #QQ [;pt
    plot(linear.model.2, 2)

    ## Warning: not plotting observations with leverage one:
    ##   1236

![](technical-report_files/figure-markdown_strict/Linear%20model%20with%20mice%20imputation-2.png)

    #Scale location plot
    plot(linear.model.2, 3)

    ## Warning: not plotting observations with leverage one:
    ##   1236

![](technical-report_files/figure-markdown_strict/Linear%20model%20with%20mice%20imputation-3.png)

    #Residual 2 
    plot(linear.model.2, 4)

![](technical-report_files/figure-markdown_strict/Linear%20model%20with%20mice%20imputation-4.png)

    #Quartet
    par(mfrow = c(2, 2))
    plot(linear.model.2)

    ## Warning: not plotting observations with leverage one:
    ##   1236

    ## Warning: not plotting observations with leverage one:
    ##   1236

![](technical-report_files/figure-markdown_strict/Linear%20model%20with%20mice%20imputation-5.png)

    linear.model.3 <- lm(log_md_wage ~ 
                           PCIP01 +
                           PCIP03 +
                           PCIP04 +
                           PCIP05 +
                           PCIP09 +
                           PCIP10 +
                           PCIP11 +
                           PCIP12 +
                           PCIP13 + 
                           PCIP14 +
                           PCIP15 +
                           PCIP16 +
                           PCIP19 +
                           PCIP22 +
                           PCIP23 +
                           PCIP24 +
                           PCIP25 +
                           PCIP26 +
                           PCIP27 +
                           PCIP29 +
                           PCIP30 +
                           PCIP31 + 
                           PCIP38 +
                           PCIP39 +
                           PCIP40 +
                           PCIP41 +
                           PCIP42 +
                           PCIP43 +
                           PCIP44 +
                           PCIP45 +
                           PCIP46 +
                           PCIP47 +
                           PCIP48 +
                           PCIP49 +
                           PCIP50 +
                           PCIP51 +
                           PCIP52 +
                           PCIP54+
                           UGDS +
                           UGDS_WHITE +
                           INEXPFTE +
                           logdebt +
                           FEMALE +
                           AGE_ENTRY +
                           FIRST_GEN +
                           REGION +
                           PPTUG_EF +
                           TUITFTE + 
                           LOAN_EVER +
                           FAMINC +
                           CONTROL +
                           PELL_EVER +
                           HIGHDEG, 
                         data = missForest_df)
    summary(linear.model.3)

    ## 
    ## Call:
    ## lm(formula = log_md_wage ~ PCIP01 + PCIP03 + PCIP04 + PCIP05 + 
    ##     PCIP09 + PCIP10 + PCIP11 + PCIP12 + PCIP13 + PCIP14 + PCIP15 + 
    ##     PCIP16 + PCIP19 + PCIP22 + PCIP23 + PCIP24 + PCIP25 + PCIP26 + 
    ##     PCIP27 + PCIP29 + PCIP30 + PCIP31 + PCIP38 + PCIP39 + PCIP40 + 
    ##     PCIP41 + PCIP42 + PCIP43 + PCIP44 + PCIP45 + PCIP46 + PCIP47 + 
    ##     PCIP48 + PCIP49 + PCIP50 + PCIP51 + PCIP52 + PCIP54 + UGDS + 
    ##     UGDS_WHITE + INEXPFTE + logdebt + FEMALE + AGE_ENTRY + FIRST_GEN + 
    ##     REGION + PPTUG_EF + TUITFTE + LOAN_EVER + FAMINC + CONTROL + 
    ##     PELL_EVER + HIGHDEG, data = missForest_df)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.9637 -0.1138  0.0151  0.1361  1.9690 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  6.725e+00  3.341e-01  20.127  < 2e-16 ***
    ## PCIP01      -1.204e-04  1.042e-04  -1.155 0.248058    
    ## PCIP03      -3.420e-04  8.925e-05  -3.831 0.000131 ***
    ## PCIP04       7.644e-04  2.367e-04   3.229 0.001257 ** 
    ## PCIP05      -1.344e-04  1.835e-04  -0.733 0.463846    
    ## PCIP09       7.796e-05  3.320e-05   2.348 0.018940 *  
    ## PCIP10       6.524e-05  1.071e-04   0.609 0.542523    
    ## PCIP11       1.089e-04  2.640e-05   4.124 3.84e-05 ***
    ## PCIP12      -1.491e-05  5.471e-05  -0.273 0.785203    
    ## PCIP13      -5.277e-05  2.189e-05  -2.410 0.016007 *  
    ## PCIP14       3.503e-04  5.759e-05   6.082 1.37e-09 ***
    ## PCIP15       1.238e-04  3.616e-05   3.423 0.000630 ***
    ## PCIP16       1.057e-04  1.137e-04   0.930 0.352423    
    ## PCIP19      -6.522e-05  5.354e-05  -1.218 0.223247    
    ## PCIP22      -1.894e-04  6.478e-05  -2.924 0.003487 ** 
    ## PCIP23      -4.857e-05  6.212e-05  -0.782 0.434379    
    ## PCIP24       3.295e-06  2.308e-05   0.143 0.886474    
    ## PCIP25      -1.873e-03  3.574e-03  -0.524 0.600272    
    ## PCIP26       3.831e-05  3.863e-05   0.992 0.321425    
    ## PCIP27       2.279e-04  1.106e-04   2.060 0.039509 *  
    ## PCIP29       5.266e-03  8.642e-03   0.609 0.542372    
    ## PCIP30      -5.396e-06  3.980e-05  -0.136 0.892179    
    ## PCIP31      -5.208e-05  3.801e-05  -1.370 0.170761    
    ## PCIP38      -3.035e-05  1.214e-04  -0.250 0.802584    
    ## PCIP39      -6.288e-04  1.324e-04  -4.750 2.15e-06 ***
    ## PCIP40       1.076e-04  8.150e-05   1.321 0.186708    
    ## PCIP41       5.965e-04  6.905e-04   0.864 0.387710    
    ## PCIP42       6.838e-05  2.845e-05   2.404 0.016306 *  
    ## PCIP43       1.290e-05  1.976e-05   0.653 0.514014    
    ## PCIP44      -5.274e-06  4.045e-05  -0.130 0.896274    
    ## PCIP45       1.024e-04  4.036e-05   2.538 0.011219 *  
    ## PCIP46      -1.049e-03  1.720e-04  -6.096 1.25e-09 ***
    ## PCIP47       2.903e-04  9.375e-05   3.096 0.001983 ** 
    ## PCIP48      -3.004e-04  1.618e-04  -1.857 0.063483 .  
    ## PCIP49      -1.590e-04  2.030e-04  -0.783 0.433617    
    ## PCIP50      -1.388e-04  2.564e-05  -5.412 6.81e-08 ***
    ## PCIP51       2.398e-05  1.080e-05   2.220 0.026536 *  
    ## PCIP52       4.308e-05  1.111e-05   3.877 0.000108 ***
    ## PCIP54       1.034e-04  8.770e-05   1.179 0.238414    
    ## UGDS         5.089e-07  7.317e-06   0.070 0.944557    
    ## UGDS_WHITE  -3.806e-05  7.668e-06  -4.964 7.35e-07 ***
    ## INEXPFTE     4.969e-06  3.859e-06   1.288 0.197994    
    ## logdebt     -2.092e-03  6.772e-03  -0.309 0.757447    
    ## FEMALE      -1.994e-05  8.501e-06  -2.346 0.019069 *  
    ## AGE_ENTRY    7.892e-05  7.525e-06  10.488  < 2e-16 ***
    ## FIRST_GEN    1.156e-05  1.377e-05   0.839 0.401335    
    ## REGION1     -5.748e-01  3.097e-01  -1.856 0.063543 .  
    ## REGION2     -5.982e-01  3.092e-01  -1.934 0.053165 .  
    ## REGION3     -5.760e-01  3.093e-01  -1.862 0.062718 .  
    ## REGION4     -5.718e-01  3.096e-01  -1.847 0.064877 .  
    ## REGION5     -6.110e-01  3.092e-01  -1.976 0.048256 *  
    ## REGION6     -5.364e-01  3.096e-01  -1.733 0.083299 .  
    ## REGION7     -5.756e-01  3.104e-01  -1.855 0.063767 .  
    ## REGION8     -5.174e-01  3.094e-01  -1.672 0.094655 .  
    ## REGION9     -1.226e+00  3.130e-01  -3.918 9.15e-05 ***
    ## PPTUG_EF     3.085e-05  8.426e-06   3.661 0.000256 ***
    ## TUITFTE     -1.026e-05  3.791e-06  -2.707 0.006839 ** 
    ## LOAN_EVER   -5.179e-05  1.027e-05  -5.043 4.90e-07 ***
    ## FAMINC       3.956e-06  5.320e-06   0.744 0.457233    
    ## CONTROL2    -2.471e-02  2.026e-02  -1.220 0.222713    
    ## CONTROL3    -1.074e-02  3.045e-02  -0.353 0.724366    
    ## PELL_EVER   -3.670e-04  1.473e-05 -24.912  < 2e-16 ***
    ## HIGHDEG1    -4.887e-01  1.682e-01  -2.906 0.003693 ** 
    ## HIGHDEG2    -2.964e-01  1.090e-01  -2.719 0.006583 ** 
    ## HIGHDEG3    -2.765e-01  1.047e-01  -2.641 0.008328 ** 
    ## HIGHDEG4    -2.602e-01  1.047e-01  -2.485 0.013024 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.3007 on 2568 degrees of freedom
    ## Multiple R-squared:  0.6134, Adjusted R-squared:  0.6036 
    ## F-statistic: 62.68 on 65 and 2568 DF,  p-value: < 2.2e-16

    #Run model diagnostics

    #Residual plot
    plot(linear.model.3, 1)

![](technical-report_files/figure-markdown_strict/Linear%20model%20with%20missForest%20imputation-1.png)

    #QQ [;pt
    plot(linear.model.3, 2)

    ## Warning: not plotting observations with leverage one:
    ##   1236

![](technical-report_files/figure-markdown_strict/Linear%20model%20with%20missForest%20imputation-2.png)

    #Scale location plot
    plot(linear.model.3, 3)

    ## Warning: not plotting observations with leverage one:
    ##   1236

![](technical-report_files/figure-markdown_strict/Linear%20model%20with%20missForest%20imputation-3.png)

    #Residual 2 
    plot(linear.model.3, 4)

![](technical-report_files/figure-markdown_strict/Linear%20model%20with%20missForest%20imputation-4.png)

    #Quartet
    par(mfrow = c(2, 2))
    plot(linear.model.3)

    ## Warning: not plotting observations with leverage one:
    ##   1236

    ## Warning: not plotting observations with leverage one:
    ##   1236

![](technical-report_files/figure-markdown_strict/Linear%20model%20with%20missForest%20imputation-5.png)

### Lasso models

To supplement these linear regression models, we ran lasso models on
each of these datasets to see if the variables which survived
penalization are variables which conform to our theoretical
expectations.

Note that when we do not impute any observations, we must use a
subsection of the dataset to run a lasso model--only the observations
without N/A values.

#### Without imputation

    # Data setup
    lasso.data <- cbind(numerics.section,
                        categoricals[1:2634,])
    lasso.data <- lasso.data %>%
      select(-MD_EARN_WNE_P6)
    lasso.data.nona <- drop_na(lasso.data)
    X <- model.matrix(log_md_wage ~ ., data = lasso.data.nona)[, -1]
    Y <- lasso.data.nona$log_md_wage 
    grid <- seq(from = 1e4, to = 1e-2, length.out = 100)

    # Cross validation to find optimal penalty term
    set.seed(1)
    cv.out1 = cv.glmnet(X, Y, alpha = 1)
    bestlam1 = cv.out1$lambda.min

    #Run lasso and get non-zero coefficients of most important variables
    lasso.1 = glmnet(x = X, y = Y, alpha = 1, lambda = grid)
    lasso.coef = predict(lasso.1, type = "coefficients", s = bestlam1)[1:nrow(coef(lasso.1)),]
    nonzero.lasso.coef <- lasso.coef[lasso.coef != 0]
    length(nonzero.lasso.coef)

    ## [1] 30

Here, the lasso regression resulted in 30 non-zero coefficients. We'll
talk more about this later.

#### Mice imputation

    # Data setup
    lasso.mice.data <- df.final.mice %>%
      select(-INSTNM, -MD_EARN_WNE_P6)
    lasso.mice.nona <- drop_na(lasso.mice.data)
    Xmice <- model.matrix(log_md_wage ~ ., data = lasso.mice.nona)
    Ymice <- lasso.mice.nona$log_md_wage 
    grid <- seq(from = 1e4, to = 1e-2, length.out = 100)

    # Cross validation to find optimal penalty term
    set.seed(1)
    cv.out.mice = cv.glmnet(Xmice, Ymice, alpha = 1)
    bestlam.mice = cv.out.mice$lambda.min

    #Run lasso and get non-zero coefficients of most important variables
    lasso.2 = glmnet(x = Xmice, y = Ymice, alpha = 1, lambda = grid)
    lasso.coef.mice = predict(lasso.2, type = "coefficients", s = bestlam.mice)[1:nrow(coef(lasso.2)),]
    nonzero.lasso.mice.coef <- lasso.coef.mice[lasso.coef.mice != 0]
    length(nonzero.lasso.mice.coef)

    ## [1] 40

#### missForest imputation

    # Data setup
    lasso.missForest.data <- missForest_df %>%
      select(-INSTNM, -MD_EARN_WNE_P6)
    lasso.missForest.nona <- drop_na(lasso.missForest.data)
    XmissForest <- model.matrix(log_md_wage ~ ., data = lasso.missForest.nona)
    YmissForest <- lasso.missForest.nona$log_md_wage 
    grid <- seq(from = 1e4, to = 1e-2, length.out = 100)

    # Cross validation to find optimal penalty term
    set.seed(1)
    cv.out.missForest = cv.glmnet(XmissForest, YmissForest, alpha = 1)
    bestlam.missForest = cv.out.missForest$lambda.min

    #Run lasso and get non-zero coefficients of most important variables
    lasso.3 = glmnet(x = XmissForest, y = YmissForest, alpha = 1, lambda = grid)
    lasso.coef.missForest = predict(lasso.3, type = "coefficients", s = bestlam.missForest)[1:nrow(coef(lasso.3)),]
    nonzero.lasso.missForest.coef <- lasso.coef.missForest[lasso.coef.missForest != 0]
    length(nonzero.lasso.missForest.coef)

    ## [1] 37

### Random forest modeling

In order to get a sense of what variables are deemed most important by a
more data-oriented approach, we decided to run a random forest on our
data set and measure variable importance.

    df_forest <- df_final %>%
      select(-INSTNM, -MD_EARN_WNE_P6) %>%
      drop_na()

    set.seed(1)
    rf.college = randomForest(log_md_wage  ~., data = df_forest, importance = TRUE)
    importance(rf.college)

    ##                         %IncMSE IncNodePurity
    ## NUMBRANCH            12.9151534   2.250039532
    ## PCIP01                1.1626507   0.086389012
    ## PCIP03                6.8113739   0.671383418
    ## PCIP04                2.7847854   0.168462337
    ## PCIP05                3.3211559   0.293310470
    ## PCIP09                8.5109040   0.771416148
    ## PCIP10                2.7823752   0.220739004
    ## PCIP11                9.0173501   1.970057022
    ## PCIP12                3.4996217   0.143908430
    ## PCIP13               15.2735452   2.293084790
    ## PCIP14                8.1472699   4.440687137
    ## PCIP15                6.1678957   0.439064340
    ## PCIP16                3.1867737   0.646280072
    ## PCIP19                1.9043939   0.184691413
    ## PCIP22                2.6833444   0.227381525
    ## PCIP23                9.6733019   1.173189880
    ## PCIP24                7.6917508   0.928366786
    ## PCIP25                0.0000000   0.001526895
    ## PCIP26                8.8047954   0.738064684
    ## PCIP27                2.4593470   0.856181422
    ## PCIP29                1.0010015   0.016754438
    ## PCIP30                4.2034309   0.374921767
    ## PCIP31                4.6355165   0.510693493
    ## PCIP38                4.3319414   0.722060490
    ## PCIP39                9.6830204   0.717926928
    ## PCIP40                6.2526334   0.426043261
    ## PCIP41               -0.3717069   0.008041613
    ## PCIP42               10.8495132   1.780359772
    ## PCIP43                6.0223494   0.464466940
    ## PCIP44                3.7370549   0.303673127
    ## PCIP45                3.8117767   1.002993835
    ## PCIP46                0.4712401   0.060025841
    ## PCIP47                0.4150117   0.088583843
    ## PCIP48                1.6560039   0.040995704
    ## PCIP49                1.6743952   0.104817031
    ## PCIP50               28.8407064   6.829965769
    ## PCIP51               17.0343255   7.038840342
    ## PCIP52               12.6691561   3.258585894
    ## PCIP54                4.3669936   0.883851525
    ## UGDS                  5.0826635   1.000062232
    ## UGDS_WHITE           12.4200049   6.436721665
    ## UGDS_BLACK           11.8713411   2.937557896
    ## UGDS_HISP            15.7517187   4.662749699
    ## UGDS_ASIAN           13.8170088  16.711571207
    ## UGDS_AIAN             6.3446573   1.151909626
    ## UGDS_NHPI             2.0638777   0.907456315
    ## UGDS_2MOR             1.4962810   2.212279214
    ## UGDS_NRA              4.7189311   6.054343388
    ## UGDS_UNKN            10.7677021   1.750638402
    ## PPTUG_EF              8.6406770   2.784580667
    ## PCTPELL              15.5789725  38.722920055
    ## INEXPFTE              7.5690021   1.131969001
    ## TUITFTE               9.5672368   1.419724726
    ## PCTFLOAN              2.8133673   2.334388315
    ## WDRAW_ORIG_YR4_RT     9.9776540  13.945665954
    ## DEBT_MDN              8.4362763   1.420147549
    ## LO_INC_DEBT_MDN      16.0330291   1.815577817
    ## MD_INC_DEBT_MDN      11.7454782   1.533083867
    ## HI_INC_DEBT_MDN      11.4247952   2.222878739
    ## PELL_DEBT_MDN         7.9784061   1.448494244
    ## NOPELL_DEBT_MDN       9.0712639   2.977143278
    ## FEMALE_DEBT_MDN       9.0083482   1.297835301
    ## MALE_DEBT_MDN        10.7926655   9.194767866
    ## FIRSTGEN_DEBT_MDN    10.0421169   1.293222914
    ## NOTFIRSTGEN_DEBT_MDN  8.0610658   1.590145231
    ## FEMALE                8.6510061   5.698413950
    ## LOAN_EVER             8.2671814   4.859176883
    ## PELL_EVER            27.8894122  94.710761855
    ## AGE_ENTRY             3.9517193   8.571120915
    ## DEPENDENT             5.6012429   7.066397340
    ## FIRST_GEN             4.2873535   6.557551448
    ## FAMINC               21.3930814  29.063229452
    ## REGION               17.3339750   5.840687278
    ## CONTROL               9.2130631   0.982643748
    ## HIGHDEG              10.4970785   1.273835053
    ## logdebt               8.1345585   1.329627234

    varImpPlot(rf.college, cex = 0.6)

![](technical-report_files/figure-markdown_strict/Random%20Forest%20with%20non-imputed%20data-1.png)

    print(rf.college)

    ## 
    ## Call:
    ##  randomForest(formula = log_md_wage ~ ., data = df_forest, importance = TRUE) 
    ##                Type of random forest: regression
    ##                      Number of trees: 500
    ## No. of variables tried at each split: 25
    ## 
    ##           Mean of squared residuals: 0.03825167
    ##                     % Var explained: 74.81

    df.forest.mice <- df.final.mice %>%
      select(-INSTNM, -MD_EARN_WNE_P6)

    set.seed(1)
    rf.college.mice = randomForest(log_md_wage  ~., data = df.forest.mice, importance = TRUE)
    importance(rf.college.mice)

    ##                         %IncMSE IncNodePurity
    ## NUMBRANCH            15.0509168  3.521637e+00
    ## PCIP01                2.9441787  3.076599e-01
    ## PCIP03                2.1511203  1.035469e+00
    ## PCIP04                3.3475750  2.573470e-01
    ## PCIP05                2.1859380  1.136819e+00
    ## PCIP09                6.4297450  1.001820e+00
    ## PCIP10                5.3543551  2.354358e-01
    ## PCIP11                9.5720428  2.856102e+00
    ## PCIP12                1.8842844  3.435360e-01
    ## PCIP13               10.2477639  2.392807e+00
    ## PCIP14               10.9158830  8.253154e+00
    ## PCIP15                3.9770702  1.173957e+00
    ## PCIP16                6.2350939  1.025409e+00
    ## PCIP19                2.3065285  3.227539e-01
    ## PCIP22                3.8119084  2.725187e-01
    ## PCIP23                7.9772568  2.193357e+00
    ## PCIP24               10.3041042  2.002091e+00
    ## PCIP25               -1.0010015  4.498959e-04
    ## PCIP26                3.4377068  1.795904e+00
    ## PCIP27                3.9113568  2.373348e+00
    ## PCIP29                1.4026537  2.414389e+00
    ## PCIP30                2.8651445  7.923003e-01
    ## PCIP31                4.8872410  5.893708e-01
    ## PCIP38                3.6839840  9.583053e-01
    ## PCIP39                8.0308742  1.390177e+00
    ## PCIP40                2.9760262  1.456141e+00
    ## PCIP41               -1.4290551  3.755363e-01
    ## PCIP42               10.0614549  2.517562e+00
    ## PCIP43                6.4161466  6.197864e-01
    ## PCIP44                3.7049452  4.122534e-01
    ## PCIP45                3.8577351  1.879647e+00
    ## PCIP46                2.3599294  9.655365e-01
    ## PCIP47               -1.7821194  1.697224e-01
    ## PCIP48                0.1125534  6.279780e-01
    ## PCIP49               -1.2777871  3.262260e-01
    ## PCIP50               28.9614317  1.456135e+01
    ## PCIP51                8.3506568  1.175334e+01
    ## PCIP52               12.7159385  5.062972e+00
    ## PCIP54                6.2546842  1.465342e+00
    ## UGDS                  1.2212599  3.155271e+00
    ## UGDS_WHITE            7.4202223  9.116295e+00
    ## UGDS_BLACK           11.4994431  3.697507e+01
    ## UGDS_HISP            10.2038407  7.038060e+00
    ## UGDS_ASIAN           14.6056399  3.811294e+01
    ## UGDS_AIAN             3.7458239  2.225210e+00
    ## UGDS_NHPI             3.7926704  2.195297e+00
    ## UGDS_2MOR             2.1818901  6.070463e+00
    ## UGDS_NRA              3.2573650  3.913228e+00
    ## UGDS_UNKN             3.4200719  4.947748e+00
    ## PPTUG_EF             11.5454268  4.385411e+00
    ## PCTPELL              16.3723262  7.214661e+01
    ## INEXPFTE              5.8469944  2.308874e+00
    ## TUITFTE               5.0039922  2.214305e+00
    ## PCTFLOAN              5.4734318  1.080721e+01
    ## WDRAW_ORIG_YR4_RT    11.3642937  2.239374e+01
    ## DEBT_MDN              1.9175842  2.608939e+00
    ## LO_INC_DEBT_MDN       9.1795467  2.820126e+00
    ## MD_INC_DEBT_MDN       5.6224794  2.519728e+00
    ## HI_INC_DEBT_MDN      10.2474406  4.550364e+00
    ## PELL_DEBT_MDN         8.7518804  4.115841e+00
    ## NOPELL_DEBT_MDN      11.3003748  4.244003e+00
    ## FEMALE_DEBT_MDN       7.4667636  2.238165e+00
    ## MALE_DEBT_MDN         6.8786995  1.019248e+01
    ## FIRSTGEN_DEBT_MDN     6.6095777  3.705674e+00
    ## NOTFIRSTGEN_DEBT_MDN  4.7231097  3.235028e+00
    ## FEMALE                9.9505242  1.120119e+01
    ## LOAN_EVER            12.2839950  1.587669e+01
    ## PELL_EVER            22.1049217  1.310617e+02
    ## AGE_ENTRY            10.6574489  1.130428e+01
    ## DEPENDENT            17.5867885  1.064481e+01
    ## FIRST_GEN             9.5513023  1.061223e+01
    ## FAMINC               15.8668828  3.195897e+01
    ## logdebt               1.7968690  1.831271e+00
    ## REGION               10.5390167  9.487966e+00
    ## CONTROL               6.0604808  1.471738e+00
    ## HIGHDEG              12.1580488  2.238086e+00

    varImpPlot(rf.college.mice, cex = 0.6)

![](technical-report_files/figure-markdown_strict/Random%20Forest%20mice-1.png)

    print(rf.college.mice)

    ## 
    ## Call:
    ##  randomForest(formula = log_md_wage ~ ., data = df.forest.mice,      importance = TRUE) 
    ##                Type of random forest: regression
    ##                      Number of trees: 500
    ## No. of variables tried at each split: 25
    ## 
    ##           Mean of squared residuals: 0.06318595
    ##                     % Var explained: 72.29

    missForest_df_forest <- missForest_df %>%
      select(-INSTNM, -MD_EARN_WNE_P6)

    set.seed(1)
    rf.college.missForest = randomForest(log_md_wage  ~., data = missForest_df_forest, importance = TRUE)
    importance(rf.college.missForest)

    ##                          %IncMSE IncNodePurity
    ## NUMBRANCH            12.83383581     2.5539096
    ## PCIP01                2.00242947     0.5411452
    ## PCIP03                3.27006878     1.1376897
    ## PCIP04                3.42857743     0.9378864
    ## PCIP05                2.50214714     0.8844720
    ## PCIP09                3.61513657     1.6104118
    ## PCIP10                2.36061730     0.4173582
    ## PCIP11                6.90200166     2.8046607
    ## PCIP12                3.15733670     0.7501418
    ## PCIP13                9.26291427     3.0259792
    ## PCIP14               12.28769617    10.4589702
    ## PCIP15                6.62249045     0.6328094
    ## PCIP16                4.08707572     1.4801384
    ## PCIP19                1.87068975     0.4500148
    ## PCIP22                0.78231138     0.3967635
    ## PCIP23                3.86445184     1.8581261
    ## PCIP24                6.06739878     1.8252229
    ## PCIP25                2.65608822     3.5575839
    ## PCIP26                5.23756501     2.0966853
    ## PCIP27                5.44986550     3.4631859
    ## PCIP29                7.23955289     2.2304425
    ## PCIP30                2.23897914     1.2048340
    ## PCIP31                5.71759797     0.6508958
    ## PCIP38                4.62767422     1.3959541
    ## PCIP39                4.89452635     3.4288221
    ## PCIP40                2.47923847     1.1984842
    ## PCIP41                0.87987746     1.4637188
    ## PCIP42                7.12772797     3.5189204
    ## PCIP43                3.66218283     0.8162163
    ## PCIP44                0.08682472     0.5815653
    ## PCIP45                5.12904231     2.6692069
    ## PCIP46                2.20597020     1.6024966
    ## PCIP47                2.75341466     0.7893054
    ## PCIP48                2.74038155     2.5836877
    ## PCIP49                1.32281281     0.7511337
    ## PCIP50               30.47367791    14.7122405
    ## PCIP51               11.91910095    10.4484339
    ## PCIP52               15.91063628     4.6592142
    ## PCIP54                4.41651299     2.1344135
    ## UGDS                  1.80367893     2.8987858
    ## UGDS_WHITE           10.48566929    13.9785121
    ## UGDS_BLACK           16.20999478    32.4781231
    ## UGDS_HISP            12.32137040     9.6876163
    ## UGDS_ASIAN           15.34537726    41.0696968
    ## UGDS_AIAN             1.39863882     2.5636180
    ## UGDS_NHPI             0.36612894     1.2574352
    ## UGDS_2MOR             1.55614715     5.6572286
    ## UGDS_NRA              6.76035457     3.9277940
    ## UGDS_UNKN             5.03574276     4.7163488
    ## PPTUG_EF              7.04934976     4.3755871
    ## PCTPELL              12.22745570    60.1001346
    ## INEXPFTE              4.45396084     2.5451292
    ## TUITFTE               4.33402509     2.5795685
    ## PCTFLOAN              9.57514845    12.2826549
    ## WDRAW_ORIG_YR4_RT     8.83850125    14.8403858
    ## DEBT_MDN              6.22255235     2.2688827
    ## LO_INC_DEBT_MDN       7.79169009     2.8728472
    ## MD_INC_DEBT_MDN       9.24624369     3.4359833
    ## HI_INC_DEBT_MDN      10.30901764     4.7784511
    ## PELL_DEBT_MDN         9.13580005     3.1690008
    ## NOPELL_DEBT_MDN       6.16173491     5.5743132
    ## FEMALE_DEBT_MDN       8.41998509     2.7182529
    ## MALE_DEBT_MDN         6.96639321     7.0491778
    ## FIRSTGEN_DEBT_MDN     7.00354225     4.0178988
    ## NOTFIRSTGEN_DEBT_MDN  8.93336531     1.9974076
    ## FEMALE               10.96604912    10.8789877
    ## LOAN_EVER            10.05259945     8.2067554
    ## PELL_EVER            22.12497569   120.0814959
    ## AGE_ENTRY             9.82839255    13.4892889
    ## DEPENDENT             6.19187607     9.8016887
    ## FIRST_GEN             6.98784966    10.2282622
    ## FAMINC               20.89167241    44.5924745
    ## logdebt               7.19802985     1.7570387
    ## REGION               12.17540598    12.3954161
    ## CONTROL               6.61497847     1.6006969
    ## HIGHDEG               9.14936194     1.7283981

    varImpPlot(rf.college.missForest, cex = 0.6)

![](technical-report_files/figure-markdown_strict/Random%20Forest%20missForest-1.png)

    print(rf.college.missForest)

    ## 
    ## Call:
    ##  randomForest(formula = log_md_wage ~ ., data = missForest_df_forest,      importance = TRUE) 
    ##                Type of random forest: regression
    ##                      Number of trees: 500
    ## No. of variables tried at each split: 25
    ## 
    ##           Mean of squared residuals: 0.06611106
    ##                     % Var explained: 71.01

### Discussion

### Sources
