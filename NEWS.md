<h1 align="center"> _Rfast_ </h1>

> ####New version 
Date release: **10/07/2018**

***

> ###**_Statistical functions_**  
>
>>1. <u> **Improved** </u>(_**by speed, correctness or options**_) 
>>
>>  |	      Function	    |     What's new!     |
>>  | --------------------- | ---------------------- |
>>  |     ompr           	| Time improvement.    |
>>  |     omp           	| Time improvement .   |
>>  |   cholesky            | Time improvement. |
***
>
>>2. <u> **New** </u>
>>
>>  | 	   Function		      |                           What's new!                           |
>>  | ------------------	  | --------------------------------------------------------------- |
>>  |                         |                                                                 |
 

***

> ###**_Utility functions_**  
>
>>1. <u> **Improved** </u>(_**by speed, correctness or options**_) 
>>
>>  | 	    Function	    |                What's new!                  |
>>  | --------------------- | ------------------------------------------- |
>>  |   colnth, rownth      | Add extra options, "na.rm" and "descending" order and "index.return". |
>>  |   colrow.zero         | deprecate it and replaced by "colrow.value". |
***
>
>>2. <u> **New** </u>
>>
>>  | 	   Function		    |                What's new!                |
>>  | ------------------	| ----------------------------------------- |
>>  |     which.is          | The same with which_isFactor but general. Use this instead of which_isFactor. |
>>  |     colrow.value      | Search if a column and row is filled with a specific value. |


> ####version 1.9.0
Date release: **15/05/2018**

***

> ###**_Statistical functions_**  
>
>>1. <u> **Improved** </u>(_**by speed, correctness or options**_) 
>>
>>  |	      Function	    |     What's new!     |
>>  | ------------------	| ------------------- |
>>  | topological_sort      |  Time improvement.  |
>>  | permcor               |  Fix a bug.         |
>>  | omp                   |  Time improvement.  |
>>  | normlog.regs          |  Fix a bug.                                    |
>>  | cova                  |  Added an extra argument.                      |
>>  | ompr                  |  Time improvement.                             |
***
>
>>2. <u> **New** </u>
>>
>>  | 	   Function		      |                           What's new!                           |
>>  | ------------------	  | --------------------------------------------------------------- |
>>  | betabinom.mle           |  MLE of the beta binomial distribution.                         |
>>  | betageom.mle            |  MLE of the beta geometric distribution.                        |
>>  | multivt.mle             |  MLE of the multivariate t distribution.                        |
>>  | colpoisson.anovas       |  Column-wise ANOVA with Poisson distribution.                   |
>>  | colquasipoisson.anovas  |  Column-wise ANOVA with quasi Poisson.                          |
>>  | exact.ttest2            |  Exact permutations 2-sample t-test.                            |
>>  | chi2Test                |  Chi-squared test of independence.                              |
>>  | gchi2Test               |  G-square and Chi-square tests of indepdence.                   |
>>  | chi2tests               |  Many chi-squared tests of independence.                        |
>>  | chi2Test_univariate     |  Matrix with chi-square tests of indepedence.                   |
>>  | mvlnorm.mle             |  MLE of the multivariate lognormal distribution.                |
>>  | poly.cor                |  Polychoric correlation.                                        |
>>  | pooled.cov              |  Pooled covariance matrix.                                      |
>>  | spatmed.reg             |  Spatial median (multivariate) regression.                      |
>>  | sscov                   |  Spatial sign covariance matrix.                                |
>>  | trim.colmeans           |  Trimmed column-wise means.                                     |
>>  | trim.rowmeans           |  Trimmed row-wise means.                                        |
>>  | eigs.sym                |  Extract some principal components from a symmetric matrix.     |
>>  | invgauss.regs           |  Many simple inverse Gaussian regressions with a log link.      |
>>  | invgauss.reg            |  Inverse Gaussian regression with a log link.                   |
>>  | gammaregs               |  Many simple Gamma regressions with a log link.                 |
>>  | gammareg                |  Gamma regression with a log link.                              |
>>  | gammacon                |  Gamma regression with a constant term only.                    |
 

***

> ###**_Utility functions_**  
>
>>1. <u> **Improved** </u>(_**by speed, correctness or options**_) 
>>
>>  | 	    Function	    |                What's new!                  |
>>  | --------------------- | ------------------------------------------- |
>>  |   as_integer          | Fix a bug.                                  |
>>  |   Round               | Fix a bug. digit argument can be up to 15.  |
>>  |   matrix.sum          | Deprecate it.                               |
>>  |   countNA             | Deprecate it.                               |
>>  |   sort_unique.length  | Deprecate it for numeric numbers.           |
>>  |   Rank                | Deprecate method="first".                   |
>>  |   Match               | Improved.                                   |
>>  |   nth                 | Improved method for integers.               |
>>  |   colshuffle          | Fix a bug.                                  |
>>  |   transpose           | Now can handle generic matrix using parallel                                  |
>>  |   colshuffle          | Fix a bug.                                  |
***
>
>>2. <u> **New** </u>
>>
>>  | 	   Function		    |                What's new!                |
>>  | ------------------	| ----------------------------------------- |
>>  |   Outer               | like R's outer.                           |
>>  |   RemoveFromNamespace | remove exported functions.                |
>>  |   Sort.int            | fast sorting integer.                     |
>>  |   colCumMaxs          | apply cummax to column.                   |
>>  |   colCumSums          | apply cumsum to column.                   |
>>  |   colCumMins          | apply cummin to column.                   |
>>  |   colCumProds         | apply cumprod to column.                  |
>>  |   positive            | apply method to each positive value.      |
>>  |   positive.negative   | apply method to each positive and negative value.   |
>>  |   negative            | apply method to each negative value.                |
>>  |   as.Rfast.function   | convert an R function to Rfast's equivalent         |
>>  |   mat.mult            | Generic matrix multiplication using parallel.       |
>>  |   checkUsage          | checking usage section in Rd files.                 |
>>  |   Hash                | Create Hash object.                                 |
>>  |   Hash.key            | Search key or multi key.                            |
>>  |   apply.condition     | Apply method to each column using a condition. Only integers.    |
>>  |   Stack               | Stack object. See man page.                               |
>>  |   iterator            | iterator object. See man page.                            |
>>  |   Elem                | access element of an iterator object.                     |
>>  |   print.environment   | S3 method for printing environment.                       |
>>  |   env.copy            | deep copy environment.                                    |
>>  |   ufactor             | Untyped factor object. See man page.                      |



> ####version 1.8.8
Date release: **10/03/2018**

***

> ###**_Statistical functions_**  
>
>>1. <u> **Improved** </u>(_**by speed, correctness or options**_) 
>>
>>  |	      Function	    |     What's new!     |
>>  | ------------------	| ------------------- |
>>  | rowMedians            | Fix a bug  |
>>  | mad2                  | Add option handling NAs and fix a bug  |
>>  | prop.regs             | Made it more stable.                   |
>>  | logistic_only         | Made it more stable.                   |
>>  | multinom.regs         | Fix a bug and removed parallel for safety reasons.   |
>>  | rint.regs             | Fix a bug.                    |
>>  | univglms              | Add the option for quasi Poisson regression     |
>>  | hd.eigen              | Now returns eigen-vectors as well.   |
>>  | ompr                  | Fix a bug.   |
>>  | tobit.mle             | Made it faster.   |
>>  | standardise           | Made it faster.   |
>>  | auc                   | Made it faster.   |
>>  | colaucs               | Made it faster.   |
>>  | pc.skel               | Made it faster and added more utilities.  |
>>  | cor.fsreg             | Made it faster and fix a bug.  |
>>  | allttests             | Made it faster and fix a bug.  |
***
>
>>2. <u> **New** </u>
>>
>>  | 	   Function		    |                           What's new!                           |
>>  | ------------------	| --------------------------------------------------------------- |
>>  | colanovas           | ANOVA with many categorical variables.                            |
>>  | quasi.poison_only   | Many quasi Poisson univariate regressions.                        |
>>  | permcor             | Permutation based hypothesis test for zero correlation.   |
>>  | boot.ttest2         | Bootstrap t-sample independent t-test.   |
>>  | univglms2           | Univariate GLMs for data.frames with continuous and categorical predictor variables.  |
>>  | bc                  | Estimate the optimal lambda in the Box-Cox transformation.  |
>>  | ar1                 | AR(1) model.  |
>>  | colar1              | Many column-wise AR(1) models.  |
>>  | bc                  | Estimate the optimal lambda in the Box-Cox transformation.  |
>>  | rbing               | Random values generation from a special case of the Bingham distribution.   |
>>  | rbingham            | Random values generation from the Bingham distribution.   |
>>  | omp                 | Orthogonal Matching Pursuit allowing many regression models.   |
>>  | yule                | Yule's coefficient of colligation.   |
>>  | col.yule            | Many column-wise Yule's coefficients of colligation.   |
>>  | cox.poisrat         | Test for the ratio of two Poisson means.   |
>>  | col.coxpoisrat      | Many column-wise tests for the ratio of two Poisson means.   |



***

> ###**_Utility functions_**  
>
>>1. <u> **Improved** </u>(_**by speed, correctness or options**_) 
>>
>>  | 	    Function	      |                What's new!                  |
>>  | --------------------- | ------------------------------------------- |
>>  | data.frame.to_matrix  | Add option setting colnames and rownames. Fix a bug   |
>>  | nth                   | Add option for handling NAs.  |
>>  | Pmax                  | Add option for handling NAs.  |
>>  | Pmin                  | Add option for handling NAs.  |
>>  | Sort                  | Add option for handling NAs.  |
>>  | Table                 | Add option for handling NAs, add option for second argument, deprecate argument as.vector and replaced from "names". time improvement/quite efficient version.  |
>>  | Round                 | Fix a bug.  |
>>  | Norm                  | Fix a bug.  |
>>  | colsums               | Add option for sum using specific indices.  |
>>  | rowsums               | Add option for sum using specific indices and option for parallel.  |
>>  | is_element            | Fix a bug.  |
>>  | eachrow               | Deprecate argument suma and replaced from argument method.  |
>>  | permutation           | Deprecate argument all and replaced from nperm.  |
>>  | permutation.next      | Deprecate argument all.next and replaced from nperm.  |
>>  | permutation.prev      | Deprecate argument all.prev and replaced from nperm.  |
>>  | data.frame.to_matrix  | Fix a bug and time improvement.  |
>>  | Rank                  | Fix a bug for method="first".    |
>>  | Match                 | fix bug and time improvement.     |
>>  | CheckExmaples         | change option "print.errors".     |
***
>
>>2. <u> **New** </u>
>>
>>  | 	   Function		    |                What's new!                |
>>  | ------------------	| ----------------------------------------- |
>>  | colPmax             | column-wise parallel maxima  |
>>  | colPmin             | column-wise parallel minima |
>>  | freq.max            | maximum frequency of a number |
>>  | freq.min            | minimum frequency of a number |
>>  | Pmin_Pmax           | parallel minima-maxima values |
>>  | Table.sign          | counting the positive, negative, zeros and NA values. |
>>  | topological_sort    | Topological sort of a Directed Acyclic Graph (DAG)    |
>>  | countNA             | count the NAs  |
>>  | columns             | get specific columns from a matrix  |
>>  | rows                | get specific rows from a matrix  |
>>  | eachcol.apply       | apply a function to each col after the operation |
>>  | checkTF             | checking man files for missing TRUE/FALSE values in examples |
