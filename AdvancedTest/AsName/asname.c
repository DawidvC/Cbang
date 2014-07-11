#include "cbind.h"

int main()
{
  size_t* array = CBANG_NS(cbind, AllocArrayOfSize_t)(10, 0);
  FreeArrayOfSize_t(array);

  return 0;
}
