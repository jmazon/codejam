#include <iostream>
using namespace std;

int c[500][500] = {0};
int f(int n, int m)
{
  if (n<0) return 0;
  if (n==0) return 1;
  if (m==0) return 0;
  if (c[n][m]) return c[n][m];

  int r = 0;
  for (int i = 1; i <= m; i++) {
    r += f(n-i, m);
    while (r >= 100003) r -= 100003;
  }
  c[n][m] = r;
  return r;
}

int solve(int n)
{
  int r = 0;
  for (int i = 0; i <= n; i++) {
    r += f(n-1-i, i);
    while (r >= 100003) r -= 100003;
  }
  return r;
}

main()
{
  int c;
  cin >> c;
  for (int i = 1; i <= c; i++) {
    int n;
    cin >> n;
    cout << "Case #" << i << ": " << solve(n) << endl;
  }
}
