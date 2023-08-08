#include <stdio.h>
#include <stdint.h>

uint32_t* image_load(const char* filename, FILE *f,
                     unsigned int *width, unsigned int *height);

void image_save(const char* filename, FILE* f, const uint32_t* image,
                unsigned int width, unsigned int height);
