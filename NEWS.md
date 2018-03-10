<h1 align="center"> _Rfast_ </h1>

> ####New version 
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

