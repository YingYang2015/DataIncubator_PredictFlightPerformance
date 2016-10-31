
Pobably most of you have experienced being stucked at the airport because of the flight delay. You may be going to have a really relaxed weekend runaway, instead the weekend is half ruined because of the flight delay. In the U.S., there are over 20% flights delayed every year. This project will build a user interactive product, in which you can see the probability of your intended booking flight being delayed. Moreover, it will also recommend you a few alternative flights with better chance leaving the airport smoothly.  
In this project, I use historical government transportation data in the past 30 years. The exploratory data suggests that 
1.	some low cost airlines actually have less delay than big airline companies. 
2.	Some smaller hubs have higher delay frequency. 
3.	Some flights departure at certain hubs will for sure be delayed. Such as UA in Augusta, GA
4.	It is better to avoid flights in the morning, because most of the flight delays happened in the morning, and the waiting time is longer.
See the plotly interactive graphs: 
Plot 1:  https://plot.ly/~yangying/1.embed
Plot 2:  https://plot.ly/~yangying/3.embed

I plan to select many more features in the dataset to predict the flight status. Feature reduction will be performed using principle component analysis or linear regression. I will compare machine learning models including logistic regression, support vector machine, random forest, and gradient boosting machines with cross validation. Models will also be fitted with subgroups, which share more similar features. Groups will be clustered using k-means clustering. 
