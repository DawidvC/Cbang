// Ok testing Mask adding for assignation

#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>

#include "meaninglessMaskTest.h"

#define MASK(Size,Value) ( ((2 << ((Size) - 1)) - 1) & (Value) )

int main()
{
    int result = 0;

    if (CBANG_NS(meaninglessMaskTest, test1)() == 1)
        // If mask was added, this tests cannot detect it...
        printf("test1:\tOK\n");
    else
    {
        printf("test1:\tKO\n");
        result = 1;
    }

    struct
    {
        const char *name;
        int8_t (*func)(void);
    } resultTest[] =
    {
        {"test2", CBANG_NS(meaninglessMaskTest, test2)},
        {"test3", CBANG_NS(meaninglessMaskTest, test3)},
        {"test4", CBANG_NS(meaninglessMaskTest, test4)},
        {"test5", CBANG_NS(meaninglessMaskTest, test5)},
    };

    for (unsigned i = 0;
          i < sizeof(resultTest) / sizeof(resultTest[0]);
          ++i)
      if (resultTest[i].func() == MASK(4, 127))
          printf("%s:\tOK\n", resultTest[i].name);
      else
      {
          printf("%s:\tKO (%d)\n", resultTest[i].name, resultTest[i].func());
          result = 1;
      }

  return result;
}
