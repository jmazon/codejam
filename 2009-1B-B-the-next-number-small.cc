#include <iostream>
#include <deque>
#include <iterator>
#include <algorithm>
using namespace std;
int main ()
{
  int t;
  cin >> t;
  for (int i = 1; i <= t; i++) {
    
    unsigned long n;
    cin >> n;

    deque<int> v;
    while (n != 0) {
      v.push_front(n % 10);
      n /= 10;
    }
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
