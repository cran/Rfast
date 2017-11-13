#include <algorithm>

#ifndef TEMPLATES_H
#define TEMPLATES_H

template<typename f,typename s>
struct pr{
  f first;
  s second;
  bool is_good;
  pr():first(0),second(0),is_good(false){}
  pr(f first,s second):first(first),second(second),is_good(false){}
};

typedef double (*Func_d)(double);

template<Func_d F>
void fill_with(double *start,double *end,double *startf){
  for(;start!=end;++start,++startf)
    *startf=F(*start);
}

template<typename F1,typename F2,typename F3 >
int count_value_helper(F1 x,F2 value){
  int s=0;
  F3 start=x.begin(),end=start+x.size();
  for(;start!=end;++start)
    if(*start==value)
      s++;
  return s;
}

template<typename T>
double med_helper(typename T::iterator start,typename T::iterator last){
  double F;
  const int sz=last-start,middle=sz/2-1;
  if(sz%2==0){
    std::nth_element(start,start+middle,last);
    F=(start[middle]+*(std::min_element(start+middle+1,last)))/2.0;
  }else{
    std::nth_element(start,start+middle+1,last);
    F=start[middle+1];
  }
  return F;
}

#endif
