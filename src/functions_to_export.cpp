//Author: Manos Papadakis

#include <RcppArmadillo.h>
#include "functions_to_export.h"

using namespace arma;
using namespace std;

vector<string> split_words(string x){
  x.erase(remove(x.begin(),x.end(), ' '),x.end());
  int n=std::count(x.begin(),x.end(),',')+1;
  vector<string> y(n);
  x+=",";
  int i=0;
  const char *split=",";
  char *token = strtok(&x[0], split);
  while (token != NULL) {
    y[i++]=token;
    token = strtok(NULL, split);
  }
  return y;
}

void writeFile(vector<string> f,string path){
  ofstream oput(path.c_str());
  if(!oput.is_open()){
    Rcout<<"can't open file\n";
  }
  for(unsigned int i=0;i<f.size();++i){
    oput<<f[i]<<endl;
  }
}

vector<string> readFile(string path,int& which_string_has_export){
  std::ifstream input(path.c_str());
  string s,export_word="export";
  vector<string> f;
  which_string_has_export=-1;
  while(getline(input,s)){
    if(find_export(s,export_word)){
      which_string_has_export=f.size();
    }
    f.push_back(s);
  }  
  return f;
}

bool find_export(string x,string y){
  unsigned int leny=y.size();
  if(x.size()<leny){
    return false;
  }
  unsigned int i=0;
  for(i=0;i<y.size();++i){
    if(x[i]!=y[i])
      return false;
  }
  return true;
}

vector<string> readDirectory(const string path){
  DIR *dir=NULL;
  struct dirent *ent;
  vector<string> files;
  string textf;
  if((dir = opendir(path.c_str())) != NULL) {
    readdir(dir);
    readdir(dir);
    while((ent = readdir(dir)) != NULL) {
      textf=ent->d_name;
      textf.erase(textf.end()-2,textf.end());
      files.push_back(textf);
    }
    closedir(dir);
  }else{
    stop("Error: Could not open directory with path \""+path+"\"");
  }
  return files;
}
