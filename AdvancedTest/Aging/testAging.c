// testing aging.cb

#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>

#include "aging.h"

void intprinter(uint32_t x)
{
  printf("%u",x);
}

void strprinter(char *x)
{
  printf("%s",x);
}

uint32_t myrandom()
{
  return random();
}

int main()
{
  CBANG_NS(aging, test)(intprinter,strprinter,myrandom);
  return 0;
}
