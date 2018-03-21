# LLSMF2013 project

Project done in Data Aanalytics applied in business in **Université Catholique de Louvain-la-Neuve**. The objective is to put into practice quantitative data analysis techniques through a case study requiring a data analysis software. All the project has been programmed in **R**. The code is available in this [link](code.R).

This data set is divided into two subsets, one training set (Available historical data : [Train.csv](Train.csv)) and one test set (New data to be predicted : [TestStudent.csv](TestStudent.csv)). They both contain 15,000 records of loans. The training set contains 23 explanatory variables (features) and one binary target, dependent, categorical variable representing whether the customer defaulted the payment or not (coded as a binary variable). These features include continuous as well as categorical variables such as, among others, the gender of the customer, age, education, history of payments, etc. 

The second set is the test set, it contains the same set of explanatory variables for 15.000 other records, but not the target variable. The objective was to predict which individuals will default the payment, as accurately as possible, using a **supervised classification algorithm**.

In this project the team developed several algorithms to reach the highest accuracy on the predictions. The Machine Learning algorithms implemented are:

- Logistic Regression
- Stepwize Logistic Regression
- K-nearest neighbours
- Decision Trees
- Artificial Neural Network
- Random Forest

In conclusion, after test several methods to classify data the **Random Forest algorithm** was chosen to make the predictions on the new data. Furthermore, the feature selection version of Random Forest was selected for the final model. The Random Forest was chosen because it had the best accuracy studied, used less features, avoids over-fitting, and is intuitive. Therefore, a tradeoff between complexity and performance is reached. It is estimated the model will reach an accuracy of around 78% on the new dataset. Further explanation of the project is in this [file](FINAL_REPORT.pdf)

