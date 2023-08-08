#ifndef IMAGEFILES_NO_FREEIMAGE
#include <FreeImage.h>
#include "imagefiles_freeimage.h"

bool freeimage_initialized = false;

void initialize() {
  if (!freeimage_initialized) {
    FreeImage_Initialise(false);
    freeimage_initialized = true;
  }
}

uint32_t* image_load(const char* filename, FILE *f, unsigned int *width, unsigned int *height) {
  initialize();
  return (uint32_t*)freeimage_load(filename, f, width, height);
}

void image_save(const char* filename, FILE* f, const uint32_t* image,
                unsigned int width, unsigned int height) {
  initialize();
  freeimage_save(filename, f, (int32_t*)image, width, height);
}
#else
#include "imagefiles_pam.h"

uint32_t* image_load(const char* filename, FILE *f,
                     unsigned int *width, unsigned int *height) {
  (void) filename;
  return pam_load(f, width, height);
}

void image_save(const char* filename, FILE* f, const uint32_t* image,
                unsigned int width, unsigned int height) {
  (void) filename;
  pam_save(f, image, width, height);
}
#endif
