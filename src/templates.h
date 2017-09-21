

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

#endif
