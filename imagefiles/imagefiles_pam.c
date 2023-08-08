#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <assert.h>

/* Backup image library: Netpbm PAM graphics format loading and saving without
   alpha channel (subset of actual format). */

uint32_t* pam_load(FILE *f, unsigned int *width, unsigned int *height) {
  assert(0 == fscanf(f, "P7\n"));
  int c = fgetc(f);
  if (c == '#') {
    while (fgetc(f) != '\n');
  } else {
    ungetc(c, f);
  }
  assert(1 == fscanf(f, "WIDTH %u\n", width));
  assert(1 == fscanf(f, "HEIGHT %u\n", height));
  unsigned int depth;
  assert(1 == fscanf(f, "DEPTH %u\n", &depth));
  assert(3 == depth);
  assert(0 == fscanf(f, "MAXVAL 255\n"));
  assert(0 == fscanf(f, "TUPLTYPE RGB\n"));
  assert(0 == fscanf(f, "ENDHDR\n"));
  uint32_t* image = (uint32_t*) malloc(*width * *height * sizeof(int32_t));
  if (image == NULL) {
    return NULL;
  }
  for (unsigned int i = 0; i < *width * *height; i++) {
    int r, g, b;
    r = fgetc(f);
    g = fgetc(f);
    b = fgetc(f);

    image[i] = 0xff000000 | (r << 16) | (g << 8) | b;
  }
  return image;
}

void pam_save(FILE* f, const uint32_t* image,
              unsigned int width, unsigned int height) {
  fprintf(f, "P7\n");
  fprintf(f, "WIDTH %u\n", width);
  fprintf(f, "HEIGHT %u\n", height);
  fprintf(f, "DEPTH 3\n");
  fprintf(f, "MAXVAL 255\n");
  fprintf(f, "TUPLTYPE RGB\n");
  fprintf(f, "ENDHDR\n");
  for (unsigned int i = 0; i < width * height; i++) {
    int32_t color = image[i];
    int r, g, b;
    r = (color & 0xff0000) >> 16;
    g = (color & 0x00ff00) >> 8;
    b = color & 0x0000ff;
    fputc(r, f);
    fputc(g, f);
    fputc(b, f);
  }
}

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
