<h1 align="center"> _Rfast_ </h1>

> ####New version 
Date release: **22/08/2017**

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
>>  |  colVars              |     Option for parallel computations    |
>>  |  corpairs             |     Option for parallel computations    |
>>  |  ct.mle               |     Made Newton-Raphson more robust     |
>>  |  fish.kent            |     Time imporvement                    |
>>  |  logseries.mle        |     Time imporvement                    |
>>  |  multinom.reg         |     Made Newton-Raphson more robust     |
>>  |  mv.eeltest2          |     Time improvement                    |
>>  |  mvnorm.mle           |     Time improvement                    |
>>  |  prop.reg             |     Bug fix                             |
>>  |  qpois.reg            |     Bug fix                             |
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
>>  |  cor.fbed           |     FBED variable selection method using the correlation        |
>>  |  match.coefs        |     Many matching coefficients                                  |


***

> ###**_Utility functions_**  
>
>>1. <u> **Improved** </u>(_**by speed, correctness or options**_) 
>>
>>  | 	    Function	      |                What's new!                  |
>>  | --------------------- | ------------------------------------------- |
>>  | colMads, rowMads      | add option for parallel. By default "parallel=FALSE"  |
>>  | rowMedians            | add option for parallel. By default "parallel=FALSE" and fix bug.  |
>>  | group.sum             | time improvement. Use less memory.  |
>>  | nth                   | add argument for descending order and index.return |
>>  | sort_mat              | fix bug about agument by.row |
>>  | lower_tri, upper_tri  | add option for including diagonal in the proccess. |
>>  | AdToNamespace         | time improvement. Add argument "sort=FALSE" for sorting the names. |
>>  | colnth, rownth        | fix bug about argument parallel |
>>  | dista                 | fix bug about argument square and add extra arguments for the firtst "k" index/values. |
>>  | Order                 | add option for partial order. |
>>  | checkAliases, is_element, comb_n, Dist  | fix bug. |
>>  | Var, med              | add option for remove NAs. |
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

