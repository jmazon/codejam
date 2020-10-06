#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <inttypes.h>

typedef void (*solver)(int64_t, int64_t, int64_t, int64_t *, int64_t *);

/* Phase 2: customers alternate from both piles. */
static void phase2(solver s, int64_t *n, int64_t *l, int64_t *r)
{
  int64_t n0 = *n;
  if (*l >= *r) {
    s(n0+1, 2, *l, l, n);
    s(n0+2, 2, *r, r, n);
  }
  else {
    s(n0+1, 2, *r, r, n);
    s(n0+2, 2, *l, l, n);
  }
}

/* Phase 1: all customers are served from the same largest pile. */
static void phase1(solver s, int64_t *n, int64_t *l, int64_t *r)
{
  if (*l >= *r) {
    s(1, 1, *l-*r, l, n);
    phase2(s, n, l, r);
  }
  else {
    s(1, 1, *r-*l, r, n);
    phase2(s, n, l, r);
  }
}

static void bs_int(int64_t i0, int64_t step, int64_t stock,
                   int64_t *pile, int64_t *n)
{
  int64_t a = 0, b = 1414213562l, m, v;
  for (;;) {
    m = (a + b) / 2;
    v = m * (2*i0 + step*(m-1)) / 2;
    if (b-a <= 1) break;
    if (v <= stock) a = m;
    else b = m;
  }
  *n += m;
  *pile -= v;
}

static void math(int64_t i0, int64_t step, int64_t stock,
                 int64_t *pile, int64_t *n)
{
  long double d = (double) i0 / step - 0.5;
  int64_t m = floorl(sqrtl(2/step*stock + d*d) - d);
  *n += m;
  *pile -= m * (2*i0 + step*(m-1)) / 2;
}

static void mathf128(int64_t i0, int64_t step, int64_t stock,
                     int64_t *pile, int64_t *n)
{
  _Float128 d = (_Float128) i0 / step - 0.5f128;
  int64_t m = floor(sqrt(2/step*stock + d*d) - d);
  *n += m;
  *pile -= m * (2*i0 + step*(m-1)) / 2;
}

solver solvers[] = { bs_int, math, mathf128 };

int main()
{
  int t; scanf("%d", &t);
  for (int tn = 1; tn <= t; tn++) {
    int64_t n = 0, l, r; scanf("%" SCNd64 "%" SCNd64, &l, &r);
    phase1(solvers[rand() % (sizeof solvers / sizeof(solver))], &n, &l, &r);
    printf("Case #%d: %" PRId64 " %" PRId64 " %" PRId64 " \n", tn, n, l, r);
  }
}

