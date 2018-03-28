# US Job Satisfaction Analysis 2011

Use the SOCR 2011 US Job Satisfaction data** to construct an R protocol to examine the job-stress level and hiring-potential using the job description (JD) text.

Implemtation Steps:

- Split the data 90:10 training:testing (randomly).
- Convert the textual JD meta-data into a corpus object.
- Triage some of the irrelevant punctuation and other symbols in the corpus document, change all text to lower case, etc.
- Tokenize the job descriptions into words. 
- Examine the distributions of Stress_Category and Hiring_Potential.
- Binarize the Job Stress into two categories (low/high stress levels), separately for training and testing data.
- Generate a word cloud to visualize the job descriptions (training data).
- Graphically visualize the difference between low and high stress categories.
- Transform the word count features into categorical data.
- Ignore low frequency words and report the sparsity of your categorical data matrix.
- Apply the Naive Bayes classifier on the high frequency terms.
- Fit an LDA prediction model for job stress level and compare to the Naive Bayes classifier (stress-level), report the error rates, specificity and sensitivity (on testing data).
- Use C5.0 and rpart to train a decision tree and compare their job-stress predictions to their Naive Bayes counterparts.
- Fit a multivariate linear model to predict Overall job ranking (smaller is better). Generate some informative pairs plots. Use backward step-wise feature selection to simplify the model, report the AIC.


















**  (http://wiki.socr.umich.edu/index.php/SOCR_Data_2011_US_JobsRanking#2011_Ranking_of_the_200_most_common_Jobs_in_the_US)

