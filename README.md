# Statistical Analysis on heart failures clinical records

We consider a heart failure clinical record [dataset](https://archive.ics.uci.edu/ml/datasets/Heart+failure+clinical+records) including the medical records of 299 patients who had left ventricular systolic dysfunction and
had previous heart failures. Data was collected during the follow-up period and each patient profile has
the following clinical features: 
- Age and Time are discrete quantitative variable taking values in $\mathbb{N}^+$;
- Creatinine Phosphokinase, Ejection Fraction, Platelets, Serum Creatinine and Serum Sodium are
continuous quantitative variables taking values in $\mathbb{R}^+$.
- Anaemia, Diabetes, High Blood Pressure, Sex and Smoking are nominal categorical binary variables.

Death Event is the target in our binary classification study and resulted to be unbalanced: the proportion of survived pationts is about 32%.

# Data exploration

The strenght of the linear association between two variables X and Y can be expressed in terms of the
Covariance $S_{XY}$ and the Pearson Correlation $r_{XY} = S_{XY}/(S_X S_Y)$.

In the following plot, the shape of the ellipses represents the Pearson coefficient for the corresponding
variables: a flattened ellipse indicates that $r_{XY}$ is near 1 or -1, while an almost circular ellipse indicates that
$r_{XY}$ is near 0.

<img align="left" width="35%" src="https://github.com/silviapoletti/Statistical-Analysis-on-Heart-failures-clinical-records/blob/a0db1c4d9a4df7d62b5196154425decc473775ea/report/correlation.png">

This plot highlights that Age and Serum Creatinine have a just hinted positive correlation with the
response, while Serum Sodium and Ejection Fraction have a just hinted negative linear correlation with
the response. This means that old age, high Serum Creatinine, low Serum Sodium and low Ejection
Fraction are all factors that may increase the risk of death.

For what concerns the variable Time, the analysis is different. In fact the response variable is pretty
correlated with Time (actual follow-up period), since it indicates whether the patient died or not before
the end of the planned follow-up period. Therefore, a death often corresponds to a short value of Time. However, in this work we prefer to
focus more on the other clinical features and their influence on the response variable.

In our dataset the highest correlation (in absolute value) between features
is $Corr(sex, smoking) = 0.446$ while the others are lower than 0.230. Therefore, our models
will reasonably not suffer from collinearity of the features.

<br/>
<br/>

# How gender influences the medical parameters?

<p align="center">
  <img src="https://github.com/silviapoletti/Statistical-Analysis-on-Heart-failures-clinical-records/blob/c1d4dbb2cc02559784bdce31f1f507a02f36e1e6/report/gender_influence_1.png" width="45%"/>
    <img src="https://github.com/silviapoletti/Statistical-Analysis-on-Heart-failures-clinical-records/blob/c1d4dbb2cc02559784bdce31f1f507a02f36e1e6/report/gender_influence_2.png" width="45%"/>
    <img src="https://github.com/silviapoletti/Statistical-Analysis-on-Heart-failures-clinical-records/blob/c1d4dbb2cc02559784bdce31f1f507a02f36e1e6/report/gender_influence_3.png" width="45%"/>
</p>

# What increments the risk of dying after an heart attack?

<p align="center">
  <img src="https://github.com/silviapoletti/Statistical-Analysis-on-Heart-failures-clinical-records/blob/8083a21b7cdb00d2bc6848ba65caaf0c0328fd62/report/risk_1.png" width="45%"/>
    <img src="https://github.com/silviapoletti/Statistical-Analysis-on-Heart-failures-clinical-records/blob/8083a21b7cdb00d2bc6848ba65caaf0c0328fd62/report/risk_2.png" width="45%"/>
    <img src="https://github.com/silviapoletti/Statistical-Analysis-on-Heart-failures-clinical-records/blob/8083a21b7cdb00d2bc6848ba65caaf0c0328fd62/report/risk_3.png" width="45%"/>
  <img src="https://github.com/silviapoletti/Statistical-Analysis-on-Heart-failures-clinical-records/blob/8083a21b7cdb00d2bc6848ba65caaf0c0328fd62/report/risk_4.png" width="45%"/>
  <img src="https://github.com/silviapoletti/Statistical-Analysis-on-Heart-failures-clinical-records/blob/8083a21b7cdb00d2bc6848ba65caaf0c0328fd62/report/risk_5.png" width="45%"/>
</p>


Data exploration: descriptive statistics and 3D data visualization to identify significant patterns, trends and interaction effects in the data.
Data modelling: K Nearest Neighbours, Linear and Quadratic Discriminant Analysis and dimensionality reduction by using Best Subset Selection and Shrinkage methods.

![alt text](https://github.com/silviapoletti/Statistical-Analysis-on-Heart-failures-clinical-records/blob/main/images/3dplot_serumcreatinine_ejectionfraction.png?raw=true)

Low Ejection Fraction and high Serum Creatinine could reasonably lead to death. In fact the (blue) regression
surface cut the (grey) horizontal plane - representing the default threshold 0.5 - forming an oblique
line. However, in the second and third plot, we can observe some misclassified points, both for class 0 and
class 1. Therefore we can’t expect an interaction effect between Ejection Fraction and Serum Creatinine.

![alt text](https://github.com/silviapoletti/Statistical-Analysis-on-Heart-failures-clinical-records/blob/main/images/3dplot_serumsodium_ejectionfraction.png?raw=true)

Low Ejection Fraction and low Serum Sodium: the regression surface cut the horizontal plane forming
an oblique line. Moreover, all the six points in the portion of space described by Ejection Fraction lower
than 30 and Serum Sodium lower than 130, belong to class 1. However, the other points seem more or
less randomly dispersed among the two classes and therefore we can’t expect that the interaction effect
between Ejection Fraction and Serum Creatinine would lead to a better fit of the data.

![alt text](https://github.com/silviapoletti/Statistical-Analysis-on-Heart-failures-clinical-records/blob/main/images/3dplot_age_ejectionfraction.png?raw=true)

Low Ejection Fraction and old Age: the regression surface cut the horizontal plane forming an oblique line.
Even if the majority of points in the portion of space described by Ejection Fraction lower than 40 and Age
greater than 70, belong to class 1, there’s also a small amount of points in that region belonging to class
0, as shown in the third plot. In addition, there’s a relevant amount of misclassified points in both classes
and therefore we can exclude an interaction effect between Ejection Fraction and Age.
