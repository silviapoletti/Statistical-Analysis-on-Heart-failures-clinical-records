# Statistical-Analysis-on-Heart-failures-clinical-records

Data exploration: descriptive statistics and 3D data visualization to identify significant patterns , trends and interaction
effects in the data . Data modelling: K Nearest Neighbours, Linear and Quadratic Discriminant Analysis and
dimensionality reduction by usi ng Best Subset Selection and Shrinkage methods.

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
