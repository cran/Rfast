//Author: Manos Papadakis

#include <RcppArmadillo.h>
#include "system_files.h"
#include <algorithm>

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
    stop("can't open file\n");
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

vector<string> readDirectory(const string path,const int n){
  DIR *dir=NULL;
  struct dirent *ent;
  vector<string> files;
  string textf;
  if((dir = opendir(path.c_str())) != NULL) {
    readdir(dir);
    readdir(dir);
    while((ent = readdir(dir)) != NULL) {
      textf=ent->d_name;
      textf.erase(textf.end()-n,textf.end());
      files.push_back(textf);
    }
    closedir(dir);
  }else{
    stop("Error: Could not open directory with path \""+path+"\"");
  }
  return files;
}

bool is_alias(const char *s,int len){
  return (len>5 && s[0]=='\\' && s[1]=='a' && s[2]=='l' 
            && s[3]=='i' && s[4]=='a' && s[5]=='s');
}

bool next_alias(ifstream &file,string &res){
  string s;
  getline(file,s);
  bool ok_alias=is_alias(s.c_str(),s.size());
  if(ok_alias){
    s.erase(s.end()-1);
    s.erase(s.begin(),s.begin()+7);
    res=s;
  }
  return ok_alias;
}

vector<string> read_aliases(ifstream &file){
  vector<string> als;
  string s;
  while(next_alias(file,s)){
    s.erase(remove(s.begin(),s.end(),' '),s.end());
    als.push_back(s);
  }
  return als;
}

vector<string> find_which(vector<string> big,vector<string> small){
  vector<string> f;
  for(unsigned int i=0;i<big.size();++i)
    if(binary_search(small.begin(),small.end(),big[i])==false)
      f.push_back(big[i]);
    return f;
}

vector<string> find_duplis(vector<string> x){
  x.push_back("@");
  vector<string>::iterator a=x.begin(),b=a+1;
  vector<string> f;
  int s=0;
  for(;b!=x.end();++b)
    if(*a!=*b){
      if(s)
        f.push_back(*a);
      a=b;
      s=0;
    }else
      ++s;
    return f;
}

bool is_example(const char *s,int len){
  return (len>7 && s[0]=='\\' && s[1]=='e' && s[2]=='x' 
            && s[3]=='a' && s[4]=='m' && s[5]=='p' 
            && s[6]=='l' && s[7]=='e' && s[8]=='s');
}

bool get_example(ifstream &file,string &res){
  string s;
  getline(file,s);
  bool is_e=is_example(s.c_str(),s.size());
  res = is_e ? s : "";
  return is_e;
}

string read_example(ifstream &file,int& long_lines){
  string als;
  string s;
  while(!get_example(file,s));
  getline(file,s);
  while(!file.eof() && s[0]!='}'){
    if(s.size()>99){ // 100 max lines
      ++long_lines;
    }
    s+="\n";
    als+=s;
    getline(file,s);
  }
  return als;
}

bool binary_help(vector<string>::iterator first,vector<string>::iterator last,string& val,vector<string>::iterator& res){
  vector<string>::iterator t=lower_bound(first,last,val);
  int tt=t-first+1;
  bool found=false;
  if(tt!=last-first && val>=*first){
    res=t;
    found=true;
  }
  return  found;
}

void dont_read_man(vector<string>& all_rd_files,vector<string>& no_read){
  if(no_read[0]!=""){
    sort(all_rd_files.begin(),all_rd_files.end());
    vector<string>::iterator fv;
    for(unsigned int i=0;i<no_read.size();++i){
      if(binary_help(all_rd_files.begin(),all_rd_files.end(),no_read[i],fv)){
        all_rd_files.erase(fv);
      }
    }
  }
}
