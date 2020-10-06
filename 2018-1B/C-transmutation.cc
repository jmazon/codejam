#include <vector>
#include <iostream>
using namespace std;

static vector<int> ri1, ri2, stock;

static void produce(int m, uint64_t sl, uint64_t sh, vector<int>& recipe)
{
  if (((m < 64 && (sl & (1 << m))) ||
       (m >= 64 && (sh & (1 << (m-64))))))
    throw 42;
  else if (stock[m] > 0) {
    stock[m]--;
    recipe[m]++;
  }
  else {
    if (m < 64) sl |= (1 << m);
    else sh |= (1 << (m - 64));
    produce(ri1[m], sl, sh, recipe);
    produce(ri2[m], sl, sh, recipe);
  }
}

int main()
{
  int t; cin >> t;
  for (int tt = 1; tt <= t; tt++) {

    int m; cin >> m;
    ri1.clear();   ri1.reserve(m);
    ri2.clear();   ri2.reserve(m);
    stock.clear(); stock.reserve(m);
    for (int j = 0; j < m; j++) {
      int i1, i2; cin >> i1 >> i2;
      ri1.push_back(i1-1);
      ri2.push_back(i2-1);
    }
    for (int j = 0; j < m; j++) {
      int q; cin >> q;
      stock.push_back(q);
    }

    int amount = 0;
    try {
      for (;;) {
        vector<int> recipe(m);
        produce(0, 0, 0, recipe);
        amount++;
continue;
        int repeat = 1000000000;
        for (int j = 0; j < m; j++)
          if (recipe[j] > 0) {
            int r = stock[j] / recipe[j];
            if (r < repeat) repeat = r;
          }
        for (int j = 0; j < m; j++)
          stock[j] -= repeat * recipe[j];
        amount += repeat;        
      }
    }
    catch (...) {}

    cout << "Case #" << tt << ": " << amount << endl;
  }
  return 0;
}
