#include <ctype.h>
#include <stdio.h>
int main ()
{
  int i,t;
  scanf("%d", &t);

  for (i = 0; i < t; i++) {
    int n;
    scanf("%d", &n);
    int j,k,o;
    char m[n][n];
    double wp[n];
    double owp[n];
    double oowp[n];
    char buf[101];
    gets(buf); /* don't try this at home */
    for (j = 0; j < n; j++) {
      gets(buf);
      for (k = 0; k < n; k++) {
	m[j][k] = buf[k];
      }
    }
    for (j = 0; j < n; j++) {
      int g=0,w=0;
      for (k = 0; k < n; k++) {
	if (isdigit(m[j][k])) g++;
	if (m[j][k]=='1') w++;
      }
      wp[j] = (double)w/(double)g;
    }
    for (j = 0; j < n; j++) {
      owp[j] = 0;
      int os = 0;
      for (k = 0; k < n; k++) {
	if (!isdigit(m[j][k])) continue;
	int og=0, ow=0;
	for (o = 0; o < n; o++) {
	  if (o==j) continue;
	  if (isdigit(m[k][o])) og++;
	  if (m[k][o] == '1') ow++;
	}
	owp[j] += (double)ow/(double)og;
	os++;
      }
      owp[j] /= (double)os;
    }
    for (j = 0; j < n; j++) {
      oowp[j] = 0;
      int os = 0;
      for (k = 0; k < n; k++) {
	if (!isdigit(m[j][k])) continue;
	oowp[j] += owp[k];
	os++;
      }
      oowp[j] /= os;
    }
    printf("Case #%d:\n", i+1);
    for (j = 0; j < n; j++)
      printf("%.12g\n", 0.25*wp[j] + 0.5*owp[j] + 0.25*oowp[j]);
  }
}
