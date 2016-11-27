#include "mapinit.h"
#include <stdio.h>

int main () {
  printf("available maps:\ntest\n");
  char input = getchar();
  mapinit(&input);
  printf("all done!\n");
  return 0;
}
