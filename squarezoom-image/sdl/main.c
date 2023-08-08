#include "../lib/github.com/diku-dk/lys/sdl/liblys.h"
#include "font_data.h"
#include PRINTFHEADER

#include "../../imagefiles/imagefiles.h"

#define _XOPEN_SOURCE
#include <unistd.h>
#include <getopt.h>

#define INITIAL_WIDTH 1024
#define INITIAL_HEIGHT 1024

void loop_start(struct lys_context *ctx, struct lys_text *text) {
  prepare_text(ctx->fut, text);
}

void loop_iteration(struct lys_context *ctx, struct lys_text *text) {
  if (!text->show_text) {
    return;
  }

  build_text(ctx, text->text_buffer, text->text_buffer_len, text->text_format,
             ctx->fps, text->sum_names);
  if (*(text->text_buffer) != '\0') {
    int32_t text_colour;
    FUT_CHECK(ctx->fut,
              futhark_entry_text_colour(ctx->fut, (uint32_t*) &text_colour,
                                        ctx->state));
    draw_text(ctx, ctx->font, ctx->font_size, text->text_buffer, text_colour, 10, 10);
  }
}

void loop_end(struct lys_text *text) {
  free(text->text_format);
  free(text->text_buffer);

  for (size_t i = 0; i < n_printf_arguments(); i++) {
    if (text->sum_names[i] != NULL) {
      size_t j = 0;
      while (text->sum_names[i][j] != NULL) {
        free(text->sum_names[i][j]);
        j++;
      }
      free(text->sum_names[i]);
    }
  }
  free(text->sum_names);
}

int font_size_from_dimensions(int width, int height) {
  int size, font_size;
  if (height < width) {
    size = height;
  } else {
    size = width;
  }
  font_size = size / 45;
  if (font_size < 14) {
    font_size = 14;
  } else if (font_size > 32) {
    font_size = 32;
  }
  return font_size;
}

TTF_Font* open_font(int font_size) {
  return TTF_OpenFontRW(SDL_RWFromMem(font_data, sizeof(font_data)), true, font_size);
}

void window_size_updated(struct lys_context *ctx) {
  ctx->font_size = font_size_from_dimensions(ctx->width, ctx->height);
  TTF_CloseFont(ctx->font);
  ctx->font = open_font(ctx->font_size);
  SDL_ASSERT(ctx->font != NULL);
}

void f1(struct lys_text *text) {
  text->show_text = !text->show_text;
}

void handle_event(struct lys_context *ctx, enum lys_event event) {
  struct lys_text *text = (struct lys_text *) ctx->event_handler_data;
  switch (event) {
  case LYS_LOOP_START:
    loop_start(ctx, text);
    break;
  case LYS_LOOP_ITERATION:
    loop_iteration(ctx, text);
    break;
  case LYS_LOOP_END:
    loop_end(text);
    break;
  case LYS_WINDOW_SIZE_UPDATED:
    window_size_updated(ctx);
    break;
  case LYS_F1:
    f1(text);
  }
}

void usage(char **argv) {
  printf("Usage: %s options... input-file\n", argv[0]);
  puts("Options:");
  puts("  -?      Print this help and exit.");
  puts("  -w INT  Set the initial width of the window.");
  puts("  -h INT  Set the initial height of the window.");
  puts("  -R      Disallow resizing the window.");
  puts("  -d DEV  Set the computation device.");
  puts("  -r INT  Maximum frames per second.");
  puts("  -i      Select execution device interactively.");
}

int main(int argc, char** argv) {
  int width = INITIAL_WIDTH, height = INITIAL_HEIGHT, max_fps = 60;
  bool allow_resize = true;
  char *deviceopt = NULL;
  bool device_interactive = false;
  char* input_image_path;

  int c;
  while ( (c = getopt(argc, argv, "w:h:r:Rd:b:i")) != -1) {
    switch (c) {
    case 'w':
      width = atoi(optarg);
      if (width <= 0) {
        fprintf(stderr, "'%s' is not a valid width.\n", optarg);
        exit(EXIT_FAILURE);
      }
      break;
    case 'h':
      height = atoi(optarg);
      if (height <= 0) {
        fprintf(stderr, "'%s' is not a valid width.\n", optarg);
        exit(EXIT_FAILURE);
      }
      break;
    case 'r':
      max_fps = atoi(optarg);
      if (max_fps <= 0) {
        fprintf(stderr, "'%s' is not a valid framerate.\n", optarg);
        exit(EXIT_FAILURE);
      }
      break;
    case 'R':
      allow_resize = false;
      break;
    case 'd':
      deviceopt = optarg;
      break;
    case 'i':
      device_interactive = true;
      break;
    case '?':
      usage(argv);
      return EXIT_SUCCESS;
    default:
      fprintf(stderr, "unknown option: %c\n", c);
      usage(argv);
      return EXIT_FAILURE;
    }
  }

  if (optind < argc) {
    input_image_path = argv[optind];
  } else {
    fprintf(stderr, "error: missing output image\n\n");
    usage(argv);
    return EXIT_FAILURE;
  }

  optind++;
  if (optind < argc) {
    fprintf(stderr, "Excess non-options: ");
    while (optind < argc)
      fprintf(stderr, "%s ", argv[optind++]);
    fprintf(stderr, "\n");
    exit(EXIT_FAILURE);
  }

  int sdl_flags = 0;
  if (allow_resize) {
    sdl_flags |= SDL_WINDOW_RESIZABLE;
  }

  struct lys_context ctx;
  struct futhark_context_config *futcfg;
  lys_setup(&ctx, width, height, max_fps, sdl_flags);

  char* opencl_device_name = NULL;
  lys_setup_futhark_context(deviceopt, device_interactive,
                            &futcfg, &ctx.fut, &opencl_device_name);
  if (opencl_device_name != NULL) {
    printf("Using OpenCL device: %s\n", opencl_device_name);
    printf("Use -d or -i to change this.\n");
    free(opencl_device_name);
  }

  FUT_CHECK(ctx.fut, futhark_entry_grab_mouse(ctx.fut, &ctx.grab_mouse));

  struct lys_text text;
  ctx.event_handler_data = (void*) &text;
  ctx.event_handler = handle_event;

  SDL_ASSERT(TTF_Init() == 0);

  ctx.font_size = font_size_from_dimensions(ctx.width, ctx.height);
  ctx.font = open_font(ctx.font_size);
  SDL_ASSERT(ctx.font != NULL);

  FILE *input_image;
  int input_image_width, input_image_height;
  if (strcmp(input_image_path, "-") == 0) {
    input_image = fdopen(0, "r");
  } else {
    input_image = fopen(input_image_path, "r");
  }
  assert(input_image != NULL);
  uint32_t* input_image_data = image_load(input_image_path, input_image,
                                          (unsigned int*) &input_image_width,
                                          (unsigned int*) &input_image_height);
  assert(input_image_data != NULL);
  fprintf(stderr, "Image dimensions: %dx%d\n", input_image_width, input_image_height);
  assert(fclose(input_image) != EOF);
  struct futhark_u32_2d *input_image_fut = futhark_new_u32_2d(ctx.fut, input_image_data, height, width);
  free(input_image_data);

  int32_t seed = (int32_t) lys_wall_time();
  futhark_entry_init(ctx.fut, &ctx.state,
                     seed, ctx.height, ctx.width, input_image_fut);
  lys_run_sdl(&ctx);
  free(ctx.data);

  TTF_CloseFont(ctx.font);

  futhark_context_free(ctx.fut);
  futhark_context_config_free(futcfg);

  return EXIT_SUCCESS;
}
