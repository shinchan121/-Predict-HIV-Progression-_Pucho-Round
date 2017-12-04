# -Predict-HIV-Progression-_Pucho-Round
 to predict the likelihood that an HIV patient's infection will become less severe, given a small dataset and limited clinical information.


# Feature extraction 
1. the length of the neuclotide
2. the count of various A,C,T,G terms in the sequence
3. the gc value of the sequence which according to the biologist studies indicate very huge importance.

# Model 
 
 Initially due to lack of knowledge of the biological predicaments it took some time to evaluate the correct model for it
 using analogy and hit and trial beilif neural net approach was thrown away to try for svm but than as the hyperplane resolution was very     hard to achieve here due to the limited data , Random forests was used to train the training set over a 70:30 ration .
 after proper tuning the model was deployed over the TEST data to which the SOLUTION csv file has been given with a accuracy of 84%
 
 
 # LEADERboard cannot be availed here as there is no submission option for this challange
 
