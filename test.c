#include <stdio.h>

int main(void) {
  int a[2] = {2, 3};
  int i = 0;

  printf("%d\n", a[i++]);
  printf("%d\n", a[i]);
  return(0);
}
