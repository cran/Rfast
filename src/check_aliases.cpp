//Author: Manos Papadakis

#include <RcppArmadillo.h>
#include "system_files.h"
#include <vector>
#include <string>
#include <algorithm>

using namespace Rcpp;
using namespace std;

//[[Rcpp::export]]
List check_aliases(const string path_man,const string path_rf,vector<string> no_read){
  ifstream file;
  string tmp2;
  vector<string> aliases,all_r_files=readDirectory(path_rf,2),all_rd_files=readDirectory(path_man,3),tmp;
  dont_read_man(all_rd_files,no_read);
  for(unsigned int i=0;i<all_rd_files.size();++i){
    file.open(path_man+all_rd_files[i]+".Rd");
    if(!file.is_open()){
      stop("Can't open file \"%s\".",all_rd_files[i]);
    }
    getline(file,tmp2);
    tmp=read_aliases(file);
    file.close();
    for(unsigned int j=0;j<tmp.size();++j){
      aliases.push_back(tmp[j]);
    }
  }
  sort(aliases.begin(),aliases.end());
  sort(all_r_files.begin(),all_r_files.end());
  List ls;
  ls["Missing Man files"]=find_which(all_r_files,aliases);
  ls["Missing R files"]=find_which(aliases,all_r_files);
  ls["Duplicate alias"]=find_duplis(aliases);
  return ls;
}

RcppExport SEXP Rfast_check_aliases(SEXP dir_to_manSEXP,SEXP dir_to_fileSEXP,SEXP no_readSEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< const string >::type dir_to_man(dir_to_manSEXP);
    traits::input_parameter< const string >::type dir_to_file(dir_to_fileSEXP);
	traits::input_parameter< vector<string> >::type no_read(no_readSEXP);
    __result = wrap(check_aliases(dir_to_man,dir_to_file,no_read));
    return __result;
END_RCPP
}
