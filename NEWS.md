<h1 align="center"> _Rfast_ </h1>

> ####New version 
Date release: **15/11/2017**

***

> ###**_Statistical functions_**  
>
>>1. <u> **Improved** </u>(_**by speed, correctness or options**_) 
>>
>>  |	      Function	    |     What's new!     |
>>  | ------------------	| ------------------- |
>>  |                       |                     |
>>  |  beta.mle             |     Made Newton-Raphson more robust     |
>>  |  betaprime.mle        |     Made Newton-Raphson more robust     |
>>  |  cauchy.mle           |     Made Newton-Raphson more robust     |
>>  |  chisq.mle            |     Made Newton-Raphson more robust     |
>>  |  colvarcomps.mom      |     Time improvement                    |
>>  |  colvarcomps.mle      |     Now workd for unbalanced designs    |
>>  |  colVars              |     Option for parallel computations    |
>>  |  corpairs             |     Option for parallel computations    |
>>  |  ct.mle               |     Made Newton-Raphson more robust     |
>>  |  fish.kent            |     Time improvement                    |
>>  |  logseries.mle        |     Time improvement                    |
>>  |  multinom.reg         |     Made Newton-Raphson more robust     |
>>  |  mv.eeltest2          |     Time improvement                    |
>>  |  mvnorm.mle           |     Time improvement                    |
>>  |  prop.reg             |     Bug fix                             |
>>  |  qpois.reg            |     Bug fix                             |
>>  |  regression           |     Bug fix                             |
>>  |  score.betaregs       |     Time improvement                    |
>>  |  ttests               |     Option for parallel computations    |
>
***
>
>>2. <u> **New** </u>
>>
>>  | 	   Function		    |                           What's new!                           |
>>  | ------------------	| --------------------------------------------------------------- |
>>  |  cat.goftests       |     Goodness of fit tests for categorical data                  |
>>  |  colcvs             |     Column-wise coefficients of variation                       |
>>  |  rowcvs             |     Column-wise coefficients of variation                       |
>>  |  dirimultinom.mle   |     MLE of the Dirichelt-multinomial distribution               |
>>  |  cor.fbed           |     FBED (without the backwardphase) variable selection method using the correlation        |
>>  |  match.coefs        |     Many matching coefficients                                  |
>>  |  spml.regs          |     Many spml regressions                                       |
>>  |  weib.reg           |     Weibull regression                                          |
>>  |  dirknn             |     k-NN algorithm with unit vectors as predictor variables     |
>>  |  ompr               |     Orthogonal matching pursuit with Pearson correlation coefficient   |
>>  |  bs.reg             |     Backward selection for logistic and Poisson regression      |


***

> ###**_Utility functions_**  
>
>>1. <u> **Improved** </u>(_**by speed, correctness or options**_) 
>>
>>  | 	    Function	      |                What's new!                  |
>>  | --------------------- | ------------------------------------------- |
>>  | colMads, rowMads      | add option for parallel  |
>>  | rowMedians            | add option for parallel and fix bug.  |
>>  | group.sum             | time improvement, Use less memory and add argument for minimum group value,  |
>>  | nth                   | add argument for descending order and index.return |
>>  | sort_mat              | fix bug about agument by.row |
>>  | lower_tri, upper_tri  | add option for including diagonal in the proccess. |
>>  | AdToNamespace         | time improvement. Add argument for sorting the names. |
>>  | colnth, rownth        | fix bug about argument parallel |
>>  | dista                 | fix bug about argument square and add extra arguments for the firtst "k" index/values. |
>>  | Order                 | add option for partial order. |
>>  | checkAliases          | fix bug. |
>>  | is_element            | fix bug. |
>>  | comb_n                | fix bug. |
>>  | Dist                  | fix bug. |
>>  | data.frame.to_matrix  | fix bug. |
>>  | Sort                  | fix bug. |
>>  | Var, med              | add option for remove NAs. |
>>  | Round                 | time improvement and add option for remove NAs. |
***
>
>>2. <u> **New** </u>
>>
>>  | 	   Function		    |                What's new!                |
>>  | ------------------	| ----------------------------------------- |
>>  | colAll, rowAll                  | call function all for each column/row of a matrix |
>>  | colCountValues, rowCountValues  | count value for each column/row of a matrix |
>>  | colRanks, rowRanks              | column and row-wise ranks of values    |
>>  | Rank                            | ranks of the values of a vector       |
>>  | transpose                       | transpose a square matrix     |


