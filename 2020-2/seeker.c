#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <inttypes.h>

static int64_t mathf128(int64_t i0, int64_t step, int64_t stock)
{
  _Float128 d = (_Float128) i0 / step - 0.5f128;
  int64_t m = floorf128(sqrtf128(2/step*stock + d*d) - d);
  *n += m;
  *pile -= m * (2*i0 + step*(m-1)) / 2;
}

static void mathd(int64_t i0, int64_t step, int64_t stock,
                  int64_t *pile, int64_t *n)
{
  double d = (double) i0 / step - 0.5;
  int64_t m = floor(sqrt(2/step*stock + d*d) - d);
  *n += m;
  *pile -= m * (2*i0 + step*(m-1)) / 2;
}

solver solvers[] = { bs_int, math };

int main()
{
  int t; scanf("%d", &t);
  for (int tn = 1; tn <= t; tn++) {
    int64_t n = 0, l, r; scanf("%" SCNd64 "%" SCNd64, &l, &r);
    phase1(solvers[rand() % (sizeof solvers / sizeof(solver))], &n, &l, &r);
    printf("Case #%d: %" PRId64 " %" PRId64 " %" PRId64 " \n", tn, n, l, r);
  }
}
