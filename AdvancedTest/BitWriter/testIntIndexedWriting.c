// Ok testing BIT access produced code

#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>

#include "intIndexedWriting.h"

int main()
{
  if (CBANG_NS(intIndexedWriting, firstTest)())
    printf("firstTest:\tOK\n");
  else
    {
      printf("firstTest:\tKO\n");
      return 1;
    }

  if (CBANG_NS(intIndexedWriting, secondTest)())
    printf("secondTest:\tOK\n");
  else
    {
      printf("secondTest:\tKO\n");
      return 1;
    }

  return 0;
}

