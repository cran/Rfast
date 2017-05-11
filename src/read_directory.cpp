
//Author: Manos Papadakis
#include <RcppArmadillo.h>
#include <dirent.h>

using namespace Rcpp;
using namespace std;


//[[Rcpp::export]]
vector<string> read_directory(string path){
  DIR *dir=NULL;
  struct dirent *ent;
  vector<string> files;
  string textf;
  if((dir = opendir(path.c_str())) != NULL) {
    readdir(dir);
    readdir(dir);
    while((ent = readdir(dir)) != NULL) {
      textf=ent->d_name;
      files.push_back(textf);
    }
    closedir(dir);
  }else{
    stop("Error: Could not open directory with path \""+path+"\"");
  }
  return files;
}


RcppExport SEXP Rfast_read_directory(SEXP pathSEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< const string >::type path(pathSEXP);
    __result = wrap(read_directory(path));
    return __result;
END_RCPP
}
