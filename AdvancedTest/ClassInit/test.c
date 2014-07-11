// Ok testing Mask adding for assignation

#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>

#include "basicObj.h"

int main()
{

# define SIZE 100
    void *object_data[SIZE];
    for (int i = 0; i < SIZE; ++i)
        object_data[i] = (void *) 0;

    CBANG_NS(basicObj, myClass) object =
        CBANG_NS(basicObj, myClass_init)(object_data);
    (void) object;

    if (object_data[1] == (void *) 0)
    {
        puts("KO: the constructor hasn't been called.");
        return 1;
    }
    else
        puts("OK: the constructor has been called.");
    return 0;
}
