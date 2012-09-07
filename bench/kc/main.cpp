#include <kcpolydb.h>
#include <iostream>
#include <sstream>

using namespace std;
using namespace kyotocabinet;

// main routine
int main(int argc, char** argv) {

  // create the database object
  PolyDB db;

  // open the database
  if (!db.open("casket.kch", PolyDB::OWRITER | PolyDB::OCREATE)) {
    cerr << "open error: " << db.error().name() << endl;
    return 0;
  }

  ifstream ifs(argv[1]);
  if (!ifs) {
      cerr << "cannot open " << argv[1] << endl;
      return 0;
  }

  // word count
  for (string word; ifs >> word; ) {
      string value = "0";
      db.get(word, &value);
      istringstream iss(value);
      int cnt; iss >> cnt;
      ostringstream oss; oss << (cnt + 1);
      if (!db.set(word, oss.str())) {
	  cerr << "set error: " << db.error().name() << endl;
      }
  }

  // close the database
  if (!db.close()) {
    cerr << "close error: " << db.error().name() << endl;
  }

  return 0;
}
