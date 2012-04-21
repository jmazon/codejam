#include <iostream>
#include <string>
#include <deque>
#include <iterator>
#include <algorithm>
using namespace std;
int main ()
{
  int t;
  cin >> t;
  for (int i = 1; i <= t; i++) {
    
    string n;
    cin >> n;

    deque<int> v;
    copy(n.begin(), n.end(), back_inserter(v));
    for (deque<int>::iterator it = v.begin(); it != v.end(); ++it) *it -= '0';
    
    if (!next_permutation(v.begin(), v.end())) {
      prev_permutation(v.begin(), v.end());
      v.push_front(0);
      next_permutation(v.begin(), v.end());
    }

    cout << "Case #" << i << ": ";
    copy(v.begin(), v.end(), ostream_iterator<int>(cout));
    cout << endl;
  }
}
