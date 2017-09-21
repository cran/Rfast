<h1 align="center"> _Rfast_ </h1>

> ####New version 
Date release: **22/08/2017**

***

> ###**_Statistical functions_**  
>
>>1. <u> **Improved** </u>(_**by speed or correctness**_) 
>>
>>  |	      Function	    |     What's new!     |
>>  | ------------------	| ------------------- |
>>  | allbetas 			      |  Time improvement.  |
>>  | ancovas 			      |     Bug fix.        |
>>  | colgeom.mle 		    |  Time improvement.  |
>>  | colpois.mle 		    |       Bug fix.    	|
>>  | diri.nr2 			      |     Added a Newton-Raphson implementation.                    |
>>  | ftest 				      |  Time improvement.                                            |
>>  | ftests 				      |  Time improvement.                                            |
>>  | fs.reg 			        |  	Added some more options for regression.                     |
>>  | lomax.mle 			    |     Time improvement.                                         |
>>  | negbin.mle 			    |     Time improvement.                                         |
>>  | prop.reg 			      |      Time improvement and better estimative procedure.        |
>>  | glm_logistic 			  |     Time improvement and better estimative procedure.         |
>>  | normlog.mle 		    |         Time improvement.                                     |
>>  | rint.reg 				    |     Time improvement.                                         |
>>  | rm.anova 			      |       Bug fix.                                                |
>>  | rmdp 				        |     Time improvement.                                         |
>>  | score.betaregs 		  |       Time improvement.                                       |
>>  | score.gammaregs 	  |      	  Time improvement.	                                    |
>>  | score.geomregs 		  |       Time improvement.	                                      |
>>  | score.glms 			    |     Time improvement.                                         |
>>  | score.multinomregs  |   		  Time improvement.                                     |
>>  | score.weibregs 		  |       Time improvement.	                                      |
>>  | spml.mle 			      |       Time improvement.                                       |
>>  | spml.reg 				    |     Time improvement.                                         |
>>  | tobit.mle 			    |     Time improvement.                                         |
>>  | ttests 				      |   Time improvement.                                           |
>>  | univglms 			      |       Time improvement.                                       |
>>  | colhameans 		      |       Added a parallel option.                                |
>>  | rowhameans 		      |       Added a parallel option.                                |
>>  | edist               |  Added the option to create a distance matrix.                |
>
***
>
>>2. <u> **New** </u>
>>
>>  | 	   Function		    |                           What's new!                           |
>>  | ------------------	| --------------------------------------------------------------- |
>>  | knn.cv              |   k-fold cross-valdiation for the k-NN algorithm.               |
>>  | anova_qpois.reg     |   Wald test for quasi poisson regression.                       |
>>  | circlin.cor         |   Correlation between a circular and a linear variable.         |
>>  | colexp2.mle         |  Column-wise MLE of the 2-parameter exponential distribution.   |
>>  | colexpmle           |  Column-wise MLE of the exponential distribution.        	      |
>>  | colgammamle         |  Column-wise MLE of the gamma distribution.                     |
>>  | colgeometric.mle    |  Column-wise MLE of the geometric distribution.                 |
>>  | colinvgauss.mle	    |  Column-wise MLE of the inverse Gaussian distribution.          |
>>  | colkurtosis	        |  Column-wise kurtosis.                                          |
>>  | collaplace.mle      |   Column-wise MLE of the Laplace distribution.                  |
>>  | collindley.mle	    |  Column-wise MLE of the Lindley distribution.                   |
>>  | colmaxboltz.mle	    |  Column-wise MLE of the Maxwell-Boltzman distribution.          |
>>  | colnormal.mle       |   Column-wise MLE of the normal distribution.                   |
>>  | colpareto.mle       |  Column-wise MLE of the Pareto distribution.                    |
>>  | colrayleigh.mle 	  |  Column-wise MLE of the Rayleigh distribution.                  |
>>  | colskewness         |  Column-wise skewness.                                          |
>>  | colvm.mle           |  Column-wise MLE of the von-Mises distribution distribution.    |
>>  | colwatsons	        | Column-wise Watson tests of uniformity.                         |
>>  | exp.regs	          |  Many simple exponential regressions.     	                    |
>>  | exp2.mle	          |  MLE of the 2-parameter exponential distribution.               |
>>  | fish.kent           |  Hypothesis test for the von-Mises Fisher versus the Kent distribution.   |
>>  | geom.nb             |  Naive Bayes with the geoemtric distribution.                   |
>>  | james	              |  The James test for 2 mean vectors.                             |
>>  | kuiper	            | The Kuiper test of uniformity                                   |
>>  | kurt                |   Kurtosis for a variable.                                      |
>>  | kurt.test2	        | Hypothesis test for 2 kurtosis coefficients.                    |
>>  | list.ftests	        | F-tests for big matrices using lists.                           |
>>  | mad2	              |  Median/Mean absolute deviation of a vector.                         |
>>  | matrnorm	          |  Matrix with random data generation from a normal distribution. |
>>  | multinom.reg	      |  Multinomial regression                                         |
>>  | mv.eeltest1	        | Exponential empirical likelihood one mean vector test.          |
>>  | mv.eeltest2	        | Exponential empirical likelihood 2 mean vectors test.           |
>>  | mvkurtosis	        | Multivariate kurtosis.                                          |
>>  | percent.ttest	      | Quasi binomial test for two percentage means.                   |
>>  | percent.ttests	    | Many quasi binomial tests for two percentage means.             |
>>  | rel.risk	          | Relative risk.                                                  |
>>  | Rnorm	              |  Nornal distribution random values generation.                  |
>>  | skew	              | Skewness coefficient.                                           |
>>  | skew.test2	        | Hypothesis test for 2 skewness coefficients.                    |
>>  | spdinv	            | Inverse of a symeetric positive definite matrix.                |
>>  | watson	            | Watson test of uniformity                                       |
>>  | normlog.reg 				| Gaussian regression with a log link function.                   |
>>  | normlog.regs 				| Many simple Gaussian regressions with a log link function.      |
>>  | rint.regs 				  | Many simple linear mixed models.                                |
>>  | geom.regs 				  | Many simple geometric regressions.                              |
>>  | multinom.regs 			| Many simple multinomial regressions.                            |
>>  | bcdcor 				      | Bias corrected distance correlation.                            |
>>  | dcor.ttest 				  | Hypothesis test for the bias corrected distance correlation.    |
>>  | group.all           | Summary statistics by group using function all                                        |
>>  | group.any           | Summary statistics by group using function any                                        |
>>  | group.med           | Summary statistics by group using function med (median)                               |
>>  | group.min           | Summary statistics by group using function min (minimum)                              |
>>  | group.max           | Summary statistics by group using function max (maximum)                              |
>>  | group.min_max       | Summary statistics by group using function min_max (minimum and maximum)              |
>>  | group.mean          | Summary statistics by group using function mean                                       |
>>  | group.var           | Summary statistics by group using function var (variance)                             |
>>  | group.mad 	        | Summary statistics by group using function mad (median/mean absolute deviation)       |


***

> ###**_Utility functions_**  
>
>>1. <u> **Improved** </u>(_**by speed or correctness**_) 
>>
>>  | 	    Function	      |                What's new!                  |
>>  | --------------------- | ------------------------------------------- |
>>  | rowMins, rowMaxs      | Fix bug about the argument "value".         |
>>  | floyd                 | From now floyd accepts numeric and integers.|
>>  | group.sum             | Time improvement - Faster version. Add extra argument.   |
>>  | upper_tri, lower_tri  | Add extra logical argument "suma=FALSE".    |
>>  | dista                 | Add extra logical argument "square=FALSE".  |
>>  | Dist                  | Add extra logical argument "vector=FALSE", and new method "itakura_saito". |
>>  | total.dist            | Add every method from Dist.                 |
>>  | Diag.matrix,Diag.fill | the argument for the initial value to fill the matrix can be vector. Also fix bug in Diag.fill. |
>>  | colmeans,colsums,colMedians, colnth, rownth | add logical argument for parallel version, "parallel=FALSE". |
>>  | colMaxs,colMins       | add logical argument for parallel version, "parallel=FALSE", but only for the values. |
>>  | sort_mat              | add logical argument for parallel version, "parallel=FALSE". Also fix bug about argument "stable". |
>>  | read.examples         | returns one more item in the list, the field "long_line".    |
>>  | Sort                  | Fix a bug with argument partial                   |
>>  | colOrder, rowOrder, Order   |   Time improvement - Really Faster version using less memory. From now on Order accepts character vector |
>>  | sort_cor_vectors      |   Faster version using less memory. From now on accepts character vector |
>>  | is_element            |   From now on accepts character vector |
>>  | rep_col,regression    |   Fix bug                               |
***
>
>>2. <u> **New** </u>
>>
>>  | 	   Function		    |     What's new!                           |
>>  | ------------------	| ----------------------------------------- |
>>  | bincomb             | binary combinations.                      |
>>  | rep_row             | repeat a vector by row.                   |
>>  | submatrix           | get a submatrix.                          |

