/*
 * PicoDrive
 * (C) notaz, 2013
 *
 * This work is licensed under the terms of MAME license.
 * See COPYING file in the top-level directory.
 */

#include <stdio.h>
#include <math.h>
#include <SDL/SDL_ttf.h>

#include "../libpicofe/input.h"
#include "../libpicofe/plat_sdl.h"
#include "../libpicofe/in_sdl.h"
#include "../libpicofe/gl.h"
#include "emu.h"
#include "menu_pico.h"
#include "input_pico.h"
#include "version.h"

#include <pico/pico.h>

#define RES_HW_SCREEN_HORIZONTAL  240
#define RES_HW_SCREEN_VERTICAL    240

#define MAX(x, y) (((x) > (y)) ? (x) : (y))
#define MIN(x, y) (((x) < (y)) ? (x) : (y))
#define ABS(x) (((x) < 0) ? (-x) : (x))

#define BLACKER_BLACKS

static void *shadow_fb;

const struct in_default_bind in_sdl_defbinds[] __attribute__((weak)) = {
	{ SDLK_u,     	IN_BINDTYPE_PLAYER12, GBTN_UP },
	{ SDLK_d,		    IN_BINDTYPE_PLAYER12, GBTN_DOWN },
	{ SDLK_l,   	  IN_BINDTYPE_PLAYER12, GBTN_LEFT },
	{ SDLK_r,  		  IN_BINDTYPE_PLAYER12, GBTN_RIGHT },
	{ SDLK_x,       IN_BINDTYPE_PLAYER12, GBTN_B },
	{ SDLK_a,      	IN_BINDTYPE_PLAYER12, GBTN_C },
	{ SDLK_b,      	IN_BINDTYPE_PLAYER12, GBTN_A },
	{ SDLK_y,      	IN_BINDTYPE_PLAYER12, GBTN_Y },
	{ SDLK_m,      	IN_BINDTYPE_PLAYER12, GBTN_X },
	{ SDLK_n,      	IN_BINDTYPE_PLAYER12, GBTN_Z },
	{ SDLK_s, 		 IN_BINDTYPE_PLAYER12, GBTN_START },
	{ SDLK_k,      	IN_BINDTYPE_PLAYER12, GBTN_MODE },
	{ SDLK_q, 		 IN_BINDTYPE_EMU, PEVB_MENU },
	{ SDLK_TAB,    	IN_BINDTYPE_EMU, PEVB_RESET },
	{ SDLK_p,     	IN_BINDTYPE_EMU, PEVB_STATE_SAVE },
	{ SDLK_F2,     	IN_BINDTYPE_EMU, PEVB_STATE_LOAD },

	{ SDLK_e,     	IN_BINDTYPE_EMU, PEVB_VOL_DOWN },
	{ SDLK_c,     	IN_BINDTYPE_EMU, PEVB_VOL_UP },
	{ SDLK_w,     	IN_BINDTYPE_EMU, PEVB_BRIGHT_DOWN },
	{ SDLK_g,     	IN_BINDTYPE_EMU, PEVB_BRIGHT_UP },
	{ SDLK_j,     	IN_BINDTYPE_EMU, PEVB_AR_FACT_DOWN },
	{ SDLK_i,     	IN_BINDTYPE_EMU, PEVB_AR_FACT_UP },
	{ SDLK_h,     	IN_BINDTYPE_EMU, PEVB_DISPMODE },

	{ SDLK_F3,     	IN_BINDTYPE_EMU, PEVB_SSLOT_PREV },
	{ SDLK_F4,     	IN_BINDTYPE_EMU, PEVB_SSLOT_NEXT },
	{ SDLK_F5,     	IN_BINDTYPE_EMU, PEVB_SWITCH_RND },
	{ SDLK_F6,     	IN_BINDTYPE_EMU, PEVB_PICO_PPREV },
	{ SDLK_F7,     	IN_BINDTYPE_EMU, PEVB_PICO_PNEXT },
	{ SDLK_F8,     	IN_BINDTYPE_EMU, PEVB_PICO_SWINP },
	{ SDLK_BACKSPACE, IN_BINDTYPE_EMU, PEVB_FF },
	{ 0, 0, 0 }
};

const struct menu_keymap in_sdl_key_map[] __attribute__((weak)) =
{
	{ SDLK_u,	PBTN_UP },
	{ SDLK_d,	PBTN_DOWN },
	{ SDLK_l,	PBTN_LEFT },
	{ SDLK_r,	PBTN_RIGHT },
	{ SDLK_a,	PBTN_MOK },
	{ SDLK_b,	PBTN_MBACK },
	{ SDLK_x,	PBTN_MA2 },
	{ SDLK_y,	PBTN_MA3 },
	{ SDLK_m,  	PBTN_L },
	{ SDLK_n, 	PBTN_R },
};

const struct menu_keymap in_sdl_joy_map[] __attribute__((weak)) =
{
	{ SDLK_UP,	PBTN_UP },
	{ SDLK_DOWN,	PBTN_DOWN },
	{ SDLK_LEFT,	PBTN_LEFT },
	{ SDLK_RIGHT,	PBTN_RIGHT },
	/* joystick */
	{ SDLK_WORLD_0,	PBTN_MOK },
	{ SDLK_WORLD_1,	PBTN_MBACK },
	{ SDLK_WORLD_2,	PBTN_MA2 },
	{ SDLK_WORLD_3,	PBTN_MA3 },
};

extern const char * const in_sdl_key_names[] __attribute__((weak));

static const struct in_pdata in_sdl_platform_data = {
	.defbinds = in_sdl_defbinds,
	.key_map = in_sdl_key_map,
	.kmap_size = sizeof(in_sdl_key_map) / sizeof(in_sdl_key_map[0]),
	.joy_map = in_sdl_joy_map,
	.jmap_size = sizeof(in_sdl_joy_map) / sizeof(in_sdl_joy_map[0]),
	.key_names = in_sdl_key_names,
};

/* YUV stuff */
static int yuv_ry[32], yuv_gy[32], yuv_by[32];
static unsigned char yuv_u[32 * 2], yuv_v[32 * 2];

SDL_Surface * hw_screen = NULL;
SDL_Surface * virtual_hw_screen = NULL;

void clear_screen(SDL_Surface *surface, uint16_t color)
{
  if(surface){
    uint16_t *dest_ptr = (uint16_t *)surface->pixels;
    uint32_t x, y;

    for(y = 0; y < surface->h; y++)
    {
      for(x = 0; x < surface->w; x++, dest_ptr++)
      {
        *dest_ptr = color;
      }
    }
  }
}

void bgr_to_uyvy_init(void)
{
  int i, v;

  /* init yuv converter:
    y0 = (int)((0.299f * r0) + (0.587f * g0) + (0.114f * b0));
    y1 = (int)((0.299f * r1) + (0.587f * g1) + (0.114f * b1));
    u = (int)(8 * 0.565f * (b0 - y0)) + 128;
    v = (int)(8 * 0.713f * (r0 - y0)) + 128;
  */
  for (i = 0; i < 32; i++) {
    yuv_ry[i] = (int)(0.299f * i * 65536.0f + 0.5f);
    yuv_gy[i] = (int)(0.587f * i * 65536.0f + 0.5f);
    yuv_by[i] = (int)(0.114f * i * 65536.0f + 0.5f);
  }
  for (i = -32; i < 32; i++) {
    v = (int)(8 * 0.565f * i) + 128;
    if (v < 0)
      v = 0;
    if (v > 255)
      v = 255;
    yuv_u[i + 32] = v;
    v = (int)(8 * 0.713f * i) + 128;
    if (v < 0)
      v = 0;
    if (v > 255)
      v = 255;
    yuv_v[i + 32] = v;
  }
}

void rgb565_to_uyvy(void *d, const void *s, int pixels)
{
  unsigned int *dst = d;
  const unsigned short *src = s;
  const unsigned char *yu = yuv_u + 32;
  const unsigned char *yv = yuv_v + 32;
  int r0, g0, b0, r1, g1, b1;
  int y0, y1, u, v;

  for (; pixels > 0; src += 2, dst++, pixels -= 2)
  {
    r0 = (src[0] >> 11) & 0x1f;
    g0 = (src[0] >> 6) & 0x1f;
    b0 =  src[0] & 0x1f;
    r1 = (src[1] >> 11) & 0x1f;
    g1 = (src[1] >> 6) & 0x1f;
    b1 =  src[1] & 0x1f;
    y0 = (yuv_ry[r0] + yuv_gy[g0] + yuv_by[b0]) >> 16;
    y1 = (yuv_ry[r1] + yuv_gy[g1] + yuv_by[b1]) >> 16;
    u = yu[b0 - y0];
    v = yv[r0 - y0];
    // valid Y range seems to be 16..235
    y0 = 16 + 219 * y0 / 31;
    y1 = 16 + 219 * y1 / 31;

    *dst = (y1 << 24) | (v << 16) | (y0 << 8) | u;
  }
}



// Nearest neighboor
void flip_NN(SDL_Surface *virtual_screen, SDL_Surface *hardware_screen, int new_w, int new_h){
  int w2=new_w;
  int h2=new_h;
  int x_ratio = (int)((virtual_screen->w<<16)/w2) +1;
  int y_ratio = (int)((virtual_screen->h<<16)/h2) +1;
  //int x_ratio = (int)((w1<<16)/w2) ;
  //int y_ratio = (int)((h1<<16)/h2) ;
  //printf("virtual_screen->w=%d, virtual_screen->h=%d\n", virtual_screen->w, virtual_screen->h);
  int x2, y2 ;
  for (int i=0;i<h2;i++) {
    if(i>=RES_HW_SCREEN_VERTICAL){
      continue;
    }
    //printf("\n\ny=%d\n", i);
    for (int j=0;j<w2;j++) {
      if(j>=RES_HW_SCREEN_HORIZONTAL){
        continue;
      }
      //printf("x=%d, ",j);
      x2 = ((j*x_ratio)>>16) ;
      y2 = ((i*y_ratio)>>16) ;

      //printf("y=%d, x=%d, y2=%d, x2=%d, (y2*virtual_screen->w)+x2=%d\n", i, j, y2, x2, (y2*virtual_screen->w)+x2);
      *(uint16_t*)(hardware_screen->pixels+(i* ((w2>RES_HW_SCREEN_HORIZONTAL)?RES_HW_SCREEN_HORIZONTAL:w2 ) +j)*sizeof(uint16_t)) =
      *(uint16_t*)(virtual_screen->pixels + ((y2*virtual_screen->w)+x2) *sizeof(uint16_t)) ;
    }
  }
}

// Nearest neighboor with possible out of screen coordinates (for cropping)
void flip_NN_AllowOutOfScreen(SDL_Surface *virtual_screen, SDL_Surface *hardware_screen, int new_w, int new_h){
  int w2=new_w;
  int h2=new_h;
  int x_ratio = (int)((virtual_screen->w<<16)/w2) +1;
  int y_ratio = (int)((virtual_screen->h<<16)/h2) +1;
  //int x_ratio = (int)((w1<<16)/w2) ;
  //int y_ratio = (int)((h1<<16)/h2) ;
  //printf("virtual_screen->w=%d, virtual_screen->h=%d\n", virtual_screen->w, virtual_screen->h);
  int x2, y2 ;

  /// --- Compute padding for centering when out of bounds ---
  int x_padding = 0;
  if(w2>RES_HW_SCREEN_HORIZONTAL){
    x_padding = (w2-RES_HW_SCREEN_HORIZONTAL)/2 + 1;
  }

  for (int i=0;i<h2;i++) {
    if(i>=RES_HW_SCREEN_VERTICAL){
      continue;
    }
    //printf("\n\ny=%d\n", i);
    for (int j=0;j<w2;j++) {
      if(j>=RES_HW_SCREEN_HORIZONTAL){
        continue;
      }
      //printf("x=%d, ",j);
      x2 = ((j*x_ratio)>>16) ;
      y2 = ((i*y_ratio)>>16) ;

      //printf("y=%d, x=%d, y2=%d, x2=%d, (y2*virtual_screen->w)+x2=%d\n", i, j, y2, x2, (y2*virtual_screen->w)+x2);
      *(uint16_t*)(hardware_screen->pixels+(i* ((w2>RES_HW_SCREEN_HORIZONTAL)?RES_HW_SCREEN_HORIZONTAL:w2 ) +j)*sizeof(uint16_t)) =
      *(uint16_t*)(virtual_screen->pixels + ((y2*virtual_screen->w)+x2 + x_padding) *sizeof(uint16_t)) ;
    }
  }
}

/// Nearest neighboor optimized with possible out of screen coordinates (for cropping)
void flip_NNOptimized_AllowOutOfScreen(SDL_Surface *virtual_screen, SDL_Surface *hardware_screen, int new_w, int new_h){
  int w1=virtual_screen->w;
  //int h1=virtual_screen->h;
  int w2=new_w;
  int h2=new_h;
  int x_ratio = (int)((virtual_screen->w<<16)/w2);
  int y_ratio = (int)((virtual_screen->h<<16)/h2);

  int y_padding = (RES_HW_SCREEN_VERTICAL-new_h)/2;
  //int x_ratio = (int)((virtual_screen->w<<16)/w2);
  //int y_ratio = (int)((virtual_screen->h<<16)/h2);
  int x2, y2 ;

  /// --- Compute padding for centering when out of bounds ---
  int x_padding = 0;
  if(w2>RES_HW_SCREEN_HORIZONTAL){
    x_padding = (w2-RES_HW_SCREEN_HORIZONTAL)/2 + 1;
  }
  int x_padding_ratio = x_padding*w1/w2;
  //printf("virtual_screen->h=%d, h2=%d\n", virtual_screen->h, h2);

  for (int i=0;i<h2;i++)
  {
    if(i>=RES_HW_SCREEN_VERTICAL){
      continue;
    }

    uint16_t* t = (uint16_t*)(hardware_screen->pixels+((i+y_padding)* ((w2>RES_HW_SCREEN_HORIZONTAL)?RES_HW_SCREEN_HORIZONTAL:w2) )*sizeof(uint16_t));
    y2 = ((i*y_ratio)>>16);
    uint16_t* p = (uint16_t*)(virtual_screen->pixels + (y2*w1 + x_padding_ratio) *sizeof(uint16_t));
    int rat = 0;
    for (int j=0;j<w2;j++)
    {
      if(j>=RES_HW_SCREEN_HORIZONTAL){
        continue;
      }
      x2 = (rat>>16);
#ifdef BLACKER_BLACKS
      *t++ = p[x2] & 0xFFDF; /// Optimization for blacker blacks
#else
      *t++ = p[x2]; /// Optimization for blacker blacks
#endif
      rat += x_ratio;
      //printf("y=%d, x=%d, y2=%d, x2=%d, (y2*virtual_screen->w)+x2=%d\n", i, j, y2, x2, (y2*virtual_screen->w)+x2);
    }
  }
}


/// Nearest neighboor with 2D bilinear and interp by the number of pixel diff, not 2
void flip_NNOptimized_MissingPixelsBilinear(SDL_Surface *virtual_screen, SDL_Surface *hardware_screen, int new_w, int new_h){
  int w1=virtual_screen->w;
  int h1=virtual_screen->h;
  int w2=new_w;
  int h2=new_h;
  int y_padding = (RES_HW_SCREEN_VERTICAL-new_h)/2;
  //int x_ratio = (int)((w1<<16)/w2) +1;
  //int y_ratio = (int)((h1<<16)/h2) +1;
  int x_ratio = (int)((w1<<16)/w2);
  int y_ratio = (int)((h1<<16)/h2);
  int x1, y1;
  /*int cnt_yes_x_yes_y, cnt_yes_x_no_y, cnt_no_x_yes_y, cnt_no_x_no_y;
  cnt_yes_x_yes_y= cnt_yes_x_no_y= cnt_no_x_yes_y= cnt_no_x_no_y = 0;*/
  for (int i=0;i<h2;i++)
  {
    uint16_t* t = (uint16_t*)(hardware_screen->pixels+((i+y_padding)*w2)*sizeof(uint16_t));
    y1 = ((i*y_ratio)>>16);
    int px_diff_next_y = MAX( (((i+1)*y_ratio)>>16) - y1, 1);
    //printf("px_diff_next_y:%d\n", px_diff_next_y);
    uint16_t* p = (uint16_t*)(virtual_screen->pixels + (y1*w1) *sizeof(uint16_t));
    int rat = 0;
    for (int j=0;j<w2;j++)
    {
      // ------ current x value ------
      x1 = (rat>>16);
      int px_diff_next_x = MAX( ((rat+x_ratio)>>16) - x1, 1);

      // ------ optimized bilinear (to put in function) -------
      uint16_t * cur_p;
      int cur_y_offset;
      uint32_t red_comp = 0;
      uint32_t green_comp = 0;
      uint32_t blue_comp = 0;
      for(int cur_px_diff_y=0; cur_px_diff_y<px_diff_next_y; cur_px_diff_y++){
        cur_y_offset = (y1+cur_px_diff_y<h1)?(w1*cur_px_diff_y):0;
        for(int cur_px_diff_x=0; cur_px_diff_x<px_diff_next_x; cur_px_diff_x++){
          cur_p = (x1+cur_px_diff_x<w1)?(p+x1+cur_px_diff_x+cur_y_offset):(p+x1+cur_y_offset);
          red_comp += (*cur_p)&0xF800;
          green_comp += (*cur_p)&0x07E0;
          blue_comp += (*cur_p)&0x001F;
        }
      }
      red_comp = (red_comp / (px_diff_next_x*px_diff_next_y) )&0xF800;
#ifdef BLACKER_BLACKS
      /// Optimization for blacker blacks (our screen do not handle green value of 1 very well)
      green_comp = (green_comp / (px_diff_next_x*px_diff_next_y) )&0x07C0;
#else
      green_comp = (green_comp / (px_diff_next_x*px_diff_next_y) )&0x07E0;
#endif
      blue_comp = (blue_comp / (px_diff_next_x*px_diff_next_y) )&0x001F;
      *t++ = red_comp+green_comp+blue_comp;

      // ------ next pixel ------
      rat += x_ratio;
    }
  }
}


/// Nearest neighbor with 2D bilinear and interpolation with left and right pixels, pseudo gaussian weighting
void flip_NNOptimized_LeftAndRightBilinear(SDL_Surface *virtual_screen, SDL_Surface *hardware_screen, int new_w, int new_h){
  int w1=virtual_screen->w;
  int h1=virtual_screen->h;
  int w2=new_w;
  int h2=new_h;
  int y_padding = (RES_HW_SCREEN_VERTICAL-new_h)/2;
  //int x_ratio = (int)((w1<<16)/w2) +1;
  //int y_ratio = (int)((h1<<16)/h2) +1;
  int x_ratio = (int)((w1<<16)/w2);
  int y_ratio = (int)((h1<<16)/h2);
  int x1, y1;

#ifdef BLACKER_BLACKS
      /// Optimization for blacker blacks (our screen do not handle green value of 1 very well)
      uint16_t green_mask = 0x07C0;
#else
      uint16_t green_mask = 0x07E0;
#endif

  /// --- Compute padding for centering when out of bounds ---
  int x_padding = 0;
  if(w2>RES_HW_SCREEN_HORIZONTAL){
    x_padding = (w2-RES_HW_SCREEN_HORIZONTAL)/2 + 1;
  }
  int x_padding_ratio = x_padding*w1/w2;

  /// --- Interp params ---
  int px_diff_prev_x = 0;
  int px_diff_next_x = 0;
  uint32_t ponderation_factor;
  uint16_t * cur_p;
  uint16_t * cur_p_left;
  uint16_t * cur_p_right;
  uint32_t red_comp, green_comp, blue_comp;
  //int cnt_interp = 0; int cnt_no_interp = 0;
  //printf("virtual_screen->w=%d, virtual_screen->w=%d\n", virtual_screen->w, virtual_screen->h);

  for (int i=0;i<h2;i++)
  {
    if(i>=RES_HW_SCREEN_VERTICAL){
      continue;
    }
    uint16_t* t = (uint16_t*)(hardware_screen->pixels+( (i+y_padding)*((w2>RES_HW_SCREEN_HORIZONTAL)?RES_HW_SCREEN_HORIZONTAL:w2))*sizeof(uint16_t));
    y1 = ((i*y_ratio)>>16);
    uint16_t* p = (uint16_t*)(virtual_screen->pixels + (y1*w1 + x_padding_ratio) *sizeof(uint16_t));
    int rat = 0;
    for (int j=0;j<w2;j++)
    {
      if(j>=RES_HW_SCREEN_HORIZONTAL){
        continue;
      }
      // ------ current x value ------
      x1 = (rat>>16);
      px_diff_next_x = ((rat+x_ratio)>>16) - x1;

      // ------ adapted bilinear with 3x3 gaussian blur -------
      cur_p = p+x1;
      if(px_diff_prev_x > 1 || px_diff_next_x > 1){
        red_comp=((*cur_p)&0xF800) << 1;
        green_comp=((*cur_p)&0x07E0) << 1;
        blue_comp=((*cur_p)&0x001F) << 1;
        ponderation_factor = 2;

        // ---- Interpolate current and left ----
        if(px_diff_prev_x > 1 && x1>0){
          cur_p_left = p+x1-1;

          red_comp += ((*cur_p_left)&0xF800);
          green_comp += ((*cur_p_left)&0x07E0);
          blue_comp += ((*cur_p_left)&0x001F);
          ponderation_factor++;
        }

        // ---- Interpolate current and right ----
        if(px_diff_next_x > 1 && x1+1<w1){
          cur_p_right = p+x1+1;

          red_comp += ((*cur_p_right)&0xF800);
          green_comp += ((*cur_p_right)&0x07E0);
          blue_comp += ((*cur_p_right)&0x001F);
          ponderation_factor++;
        }

        /// --- Compute new px value ---
        if(ponderation_factor==4){
          red_comp = (red_comp >> 2)&0xF800;
          green_comp = (green_comp >> 2)&green_mask;
          blue_comp = (blue_comp >> 2)&0x001F;
        }
        else if(ponderation_factor==2){
          red_comp = (red_comp >> 1)&0xF800;
          green_comp = (green_comp >> 1)&green_mask;
          blue_comp = (blue_comp >> 1)&0x001F;
        }
        else{
          red_comp = (red_comp / ponderation_factor )&0xF800;
          green_comp = (green_comp / ponderation_factor )&green_mask;
          blue_comp = (blue_comp / ponderation_factor )&0x001F;
        }

        /// --- write pixel ---
        *t++ = red_comp+green_comp+blue_comp;
      }
      else{
        /// --- copy pixel ---
#ifdef BLACKER_BLACKS
        /// Optimization for blacker blacks (our screen do not handle green value of 1 very well)
        *t++ = (*cur_p)&0xFFDF;
#else
        *t++ = (*cur_p);
#endif
      }

      /// save number of pixels to interpolate
      px_diff_prev_x = px_diff_next_x;

      // ------ next pixel ------
      rat += x_ratio;
    }
  }
  //printf("cnt_interp = %d, int cnt_no_interp = %d\n", cnt_interp, cnt_no_interp);
}

/// Nearest neighbor with 2D bilinear and interpolation with left, right, up and down pixels, pseudo gaussian weighting
void flip_NNOptimized_LeftRightUpDownBilinear(SDL_Surface *virtual_screen, SDL_Surface *hardware_screen, int new_w, int new_h){
  int w1=virtual_screen->w;
  int h1=virtual_screen->h;
  int w2=new_w;
  int h2=new_h;
  int y_padding = (RES_HW_SCREEN_VERTICAL-new_h)/2;
  //int x_ratio = (int)((w1<<16)/w2) +1;
  //int y_ratio = (int)((h1<<16)/h2) +1;
  int x_ratio = (int)((w1<<16)/w2);
  int y_ratio = (int)((h1<<16)/h2);
  int x1, y1;

#ifdef BLACKER_BLACKS
      /// Optimization for blacker blacks (our screen do not handle green value of 1 very well)
      uint16_t green_mask = 0x07C0;
#else
      uint16_t green_mask = 0x07E0;
#endif

  /// --- Compute padding for centering when out of bounds ---
  int x_padding = 0;
  if(w2>RES_HW_SCREEN_HORIZONTAL){
    x_padding = (w2-RES_HW_SCREEN_HORIZONTAL)/2 + 1;
  }
  int x_padding_ratio = x_padding*w1/w2;

  /// --- Interp params ---
  int px_diff_prev_x = 0;
  int px_diff_next_x = 0;
  int px_diff_prev_y = 0;
  int px_diff_next_y = 0;
  uint32_t ponderation_factor;
  uint16_t * cur_p;
  uint16_t * cur_p_left;
  uint16_t * cur_p_right;
  uint16_t * cur_p_up;
  uint16_t * cur_p_down;
  uint32_t red_comp, green_comp, blue_comp;
  //int cnt_interp = 0; int cnt_no_interp = 0;
  //printf("virtual_screen->w=%d, virtual_screen->w=%d\n", virtual_screen->w, virtual_screen->h);

  ///Debug

  for (int i=0;i<h2;i++)
  {
    if(i>=RES_HW_SCREEN_VERTICAL){
      continue;
    }
    uint16_t* t = (uint16_t*)(hardware_screen->pixels+( (i+y_padding)*((w2>RES_HW_SCREEN_HORIZONTAL)?RES_HW_SCREEN_HORIZONTAL:w2))*sizeof(uint16_t));
    // ------ current and next y value ------
    y1 = ((i*y_ratio)>>16);
    px_diff_next_y = MAX( (((i+1)*y_ratio)>>16) - y1, 1);
    uint16_t* p = (uint16_t*)(virtual_screen->pixels + (y1*w1+x_padding_ratio) *sizeof(uint16_t));
    int rat = 0;
    for (int j=0;j<w2;j++)
    {
      if(j>=RES_HW_SCREEN_HORIZONTAL){
        continue;
      }
      // ------ current x value ------
      x1 = (rat>>16);
      px_diff_next_x = ((rat+x_ratio)>>16) - x1;

      // ------ adapted bilinear with 3x3 gaussian blur -------
      cur_p = p+x1;
      if(px_diff_prev_x > 1 || px_diff_next_x > 1 || px_diff_prev_y > 1 || px_diff_next_y > 1){
        red_comp=((*cur_p)&0xF800) << 1;
        green_comp=((*cur_p)&0x07E0) << 1;
        blue_comp=((*cur_p)&0x001F) << 1;
        ponderation_factor = 2;

        // ---- Interpolate current and left ----
        if(px_diff_prev_x > 1 && x1>0){
          cur_p_left = p+x1-1;

          red_comp += ((*cur_p_left)&0xF800);
          green_comp += ((*cur_p_left)&0x07E0);
          blue_comp += ((*cur_p_left)&0x001F);
          ponderation_factor++;
        }

        // ---- Interpolate current and right ----
        if(px_diff_next_x > 1 && x1+1<w1){
          cur_p_right = p+x1+1;

          red_comp += ((*cur_p_right)&0xF800);
          green_comp += ((*cur_p_right)&0x07E0);
          blue_comp += ((*cur_p_right)&0x001F);
          ponderation_factor++;
        }

        // ---- Interpolate current and up ----
        if(px_diff_prev_y > 1 && y1 > 0){
          cur_p_up = p+x1-w1;

          red_comp += ((*cur_p_up)&0xF800);
          green_comp += ((*cur_p_up)&0x07E0);
          blue_comp += ((*cur_p_up)&0x001F);
          ponderation_factor++;
        }

        // ---- Interpolate current and down ----
        if(px_diff_next_y > 1 && y1 + 1 < h1){
          cur_p_down = p+x1+w1;

          red_comp += ((*cur_p_down)&0xF800);
          green_comp += ((*cur_p_down)&0x07E0);
          blue_comp += ((*cur_p_down)&0x001F);
          ponderation_factor++;
        }

        /// --- Compute new px value ---
        if(ponderation_factor==4){
          red_comp = (red_comp >> 2)&0xF800;
          green_comp = (green_comp >> 2)&green_mask;
          blue_comp = (blue_comp >> 2)&0x001F;
        }
        else if(ponderation_factor==2){
          red_comp = (red_comp >> 1)&0xF800;
          green_comp = (green_comp >> 1)&green_mask;
          blue_comp = (blue_comp >> 1)&0x001F;
        }
        else{
          red_comp = (red_comp / ponderation_factor )&0xF800;
          green_comp = (green_comp / ponderation_factor )&green_mask;
          blue_comp = (blue_comp / ponderation_factor )&0x001F;
        }

        /// --- write pixel ---
        *t++ = red_comp+green_comp+blue_comp;
      }
      else{
        /// --- copy pixel ---
#ifdef BLACKER_BLACKS
        /// Optimization for blacker blacks (our screen do not handle green value of 1 very well)
        *t++ = (*cur_p)&0xFFDF;
#else
        *t++ = (*cur_p);
#endif
      }

      /// save number of pixels to interpolate
      px_diff_prev_x = px_diff_next_x;

      // ------ next pixel ------
      rat += x_ratio;
    }
    px_diff_prev_y = px_diff_next_y;
  }
  //printf("cnt_interp = %d, int cnt_no_interp = %d\n", cnt_interp, cnt_no_interp);
}



/// Nearest neighbor with 2D bilinear and interpolation with left, right, up and down pixels, pseudo gaussian weighting
void flip_NNOptimized_LeftRightUpDownBilinear_Optimized4(SDL_Surface *virtual_screen, SDL_Surface *hardware_screen, int new_w, int new_h){
  int w1=virtual_screen->w;
  int h1=virtual_screen->h;
  int w2=new_w;
  int h2=new_h;
  int y_padding = (RES_HW_SCREEN_VERTICAL-new_h)/2;
  int x_ratio = (int)((w1<<16)/w2);
  int y_ratio = (int)((h1<<16)/h2);
  int x1, y1;

#ifdef BLACKER_BLACKS
      /// Optimization for blacker blacks (our screen do not handle green value of 1 very well)
      uint16_t green_mask = 0x07C0;
#else
      uint16_t green_mask = 0x07E0;
#endif

  /// --- Compute padding for centering when out of bounds ---
  int x_padding = 0;
  if(w2>RES_HW_SCREEN_HORIZONTAL){
    x_padding = (w2-RES_HW_SCREEN_HORIZONTAL)/2 + 1;
  }
  int x_padding_ratio = x_padding*w1/w2;

  /// --- Interp params ---
  int px_diff_prev_x = 0;
  int px_diff_next_x = 0;
  int px_diff_prev_y = 0;
  int px_diff_next_y = 0;
  uint32_t ponderation_factor;
  uint8_t left_px_missing, right_px_missing, up_px_missing, down_px_missing;
  int supposed_pond_factor;

  uint16_t * cur_p;
  uint16_t * cur_p_left;
  uint16_t * cur_p_right;
  uint16_t * cur_p_up;
  uint16_t * cur_p_down;
  uint32_t red_comp, green_comp, blue_comp;
  //printf("virtual_screen->w=%d, virtual_screen->w=%d\n", virtual_screen->w, virtual_screen->h);

  ///Debug
  /*int occurence_pond[7];
  memset(occurence_pond, 0, 7*sizeof(int));*/

  for (int i=0;i<h2;i++)
  {
    if(i>=RES_HW_SCREEN_VERTICAL){
      continue;
    }
    uint16_t* t = (uint16_t*)(hardware_screen->pixels+( (i+y_padding)*((w2>RES_HW_SCREEN_HORIZONTAL)?RES_HW_SCREEN_HORIZONTAL:w2))*sizeof(uint16_t));
    // ------ current and next y value ------
    y1 = ((i*y_ratio)>>16);
    px_diff_next_y = MAX( (((i+1)*y_ratio)>>16) - y1, 1);
    uint16_t* p = (uint16_t*)(virtual_screen->pixels + (y1*w1+x_padding_ratio) *sizeof(uint16_t));
    int rat = 0;
    for (int j=0;j<w2;j++)
    {
      if(j>=RES_HW_SCREEN_HORIZONTAL){
        continue;
      }
      // ------ current x value ------
      x1 = (rat>>16);
      px_diff_next_x = ((rat+x_ratio)>>16) - x1;

      // ------ adapted bilinear with 3x3 gaussian blur -------
      cur_p = p+x1;
      if(px_diff_prev_x > 1 || px_diff_next_x > 1 || px_diff_prev_y > 1 || px_diff_next_y > 1){
        red_comp=((*cur_p)&0xF800) << 1;
        green_comp=((*cur_p)&0x07E0) << 1;
        blue_comp=((*cur_p)&0x001F) << 1;
        ponderation_factor = 2;
        left_px_missing = (px_diff_prev_x > 1 && x1>0);
        right_px_missing = (px_diff_next_x > 1 && x1+1<w1);
        up_px_missing = (px_diff_prev_y > 1 && y1 > 0);
        down_px_missing = (px_diff_next_y > 1 && y1 + 1 < h1);
        supposed_pond_factor = 2 + left_px_missing + right_px_missing +
                                       up_px_missing + down_px_missing;

        // ---- Interpolate current and up ----
        if(up_px_missing){
          cur_p_up = p+x1-w1;

          if(supposed_pond_factor==3){
            red_comp += ((*cur_p_up)&0xF800) << 1;
            green_comp += ((*cur_p_up)&0x07E0) << 1;
            blue_comp += ((*cur_p_up)&0x001F) << 1;
            ponderation_factor+=2;
          }
          else if(supposed_pond_factor==4 ||
                  (supposed_pond_factor==5 && !down_px_missing )){
            red_comp += ((*cur_p_up)&0xF800);
            green_comp += ((*cur_p_up)&0x07E0);
            blue_comp += ((*cur_p_up)&0x001F);
            ponderation_factor++;
          }
        }

        // ---- Interpolate current and left ----
        if(left_px_missing){
          cur_p_left = p+x1-1;

          if(supposed_pond_factor==3){
            red_comp += ((*cur_p_left)&0xF800) << 1;
            green_comp += ((*cur_p_left)&0x07E0) << 1;
            blue_comp += ((*cur_p_left)&0x001F) << 1;
            ponderation_factor+=2;
          }
          else if(supposed_pond_factor==4 ||
                  (supposed_pond_factor==5 && !right_px_missing )){
            red_comp += ((*cur_p_left)&0xF800);
            green_comp += ((*cur_p_left)&0x07E0);
            blue_comp += ((*cur_p_left)&0x001F);
            ponderation_factor++;
          }
        }

        // ---- Interpolate current and down ----
        if(down_px_missing){
          cur_p_down = p+x1+w1;

          if(supposed_pond_factor==3){
            red_comp += ((*cur_p_down)&0xF800) << 1;
            green_comp += ((*cur_p_down)&0x07E0) << 1;
            blue_comp += ((*cur_p_down)&0x001F) << 1;
            ponderation_factor+=2;
          }
          else if(supposed_pond_factor>=4){
            red_comp += ((*cur_p_down)&0xF800);
            green_comp += ((*cur_p_down)&0x07E0);
            blue_comp += ((*cur_p_down)&0x001F);
            ponderation_factor++;
          }
        }

        // ---- Interpolate current and right ----
        if(right_px_missing){
          cur_p_right = p+x1+1;

          if(supposed_pond_factor==3){
            red_comp += ((*cur_p_right)&0xF800) << 1;
            green_comp += ((*cur_p_right)&0x07E0) << 1;
            blue_comp += ((*cur_p_right)&0x001F) << 1;
            ponderation_factor+=2;
          }
          else if(supposed_pond_factor>=4){
            red_comp += ((*cur_p_right)&0xF800);
            green_comp += ((*cur_p_right)&0x07E0);
            blue_comp += ((*cur_p_right)&0x001F);
            ponderation_factor++;
          }
        }

        /// --- Compute new px value ---
        if(ponderation_factor==4){
          red_comp = (red_comp >> 2)&0xF800;
          green_comp = (green_comp >> 2)&green_mask;
          blue_comp = (blue_comp >> 2)&0x001F;
        }
        else if(ponderation_factor==2){
          red_comp = (red_comp >> 1)&0xF800;
          green_comp = (green_comp >> 1)&green_mask;
          blue_comp = (blue_comp >> 1)&0x001F;
        }
        else{
          red_comp = (red_comp / ponderation_factor )&0xF800;
          green_comp = (green_comp / ponderation_factor )&green_mask;
          blue_comp = (blue_comp / ponderation_factor )&0x001F;
        }

        /// Debug
        //occurence_pond[ponderation_factor] += 1;

        /// --- write pixel ---
        *t++ = red_comp+green_comp+blue_comp;
      }
      else{
        /// --- copy pixel ---
#ifdef BLACKER_BLACKS
        /// Optimization for blacker blacks (our screen do not handle green value of 1 very well)
        *t++ = (*cur_p)&0xFFDF;
#else
        *t++ = (*cur_p);
#endif

        /// Debug
        //occurence_pond[1] += 1;
      }

      /// save number of pixels to interpolate
      px_diff_prev_x = px_diff_next_x;

      // ------ next pixel ------
      rat += x_ratio;
    }
    px_diff_prev_y = px_diff_next_y;
  }
  /// Debug
  /*printf("pond: [%d, %d, %d, %d, %d, %d]\n", occurence_pond[1], occurence_pond[2], occurence_pond[3],
                                              occurence_pond[4], occurence_pond[5], occurence_pond[6]);*/
}



/// Nearest neighbor with 2D bilinear and interpolation with left, right, up and down pixels, pseudo gaussian weighting
void flip_NNOptimized_LeftRightUpDownBilinear_Optimized8(SDL_Surface *virtual_screen, SDL_Surface *hardware_screen, int new_w, int new_h){
  int w1=virtual_screen->w;
  int h1=virtual_screen->h;
  int w2=new_w;
  int h2=new_h;
  int y_padding = (RES_HW_SCREEN_VERTICAL-new_h)/2;
  //int x_ratio = (int)((w1<<16)/w2) +1;
  //int y_ratio = (int)((h1<<16)/h2) +1;
  int x_ratio = (int)((w1<<16)/w2);
  int y_ratio = (int)((h1<<16)/h2);
  int x1, y1;

#ifdef BLACKER_BLACKS
      /// Optimization for blacker blacks (our screen do not handle green value of 1 very well)
      uint16_t green_mask = 0x07C0;
#else
      uint16_t green_mask = 0x07E0;
#endif

  /// --- Compute padding for centering when out of bounds ---
  int x_padding = 0;
  if(w2>RES_HW_SCREEN_HORIZONTAL){
    x_padding = (w2-RES_HW_SCREEN_HORIZONTAL)/2 + 1;
  }
  int x_padding_ratio = x_padding*w1/w2;

  /// --- Interp params ---
  int px_diff_prev_x = 0;
  int px_diff_next_x = 0;
  int px_diff_prev_y = 0;
  int px_diff_next_y = 0;
  uint32_t ponderation_factor;
  uint8_t left_px_missing, right_px_missing, up_px_missing, down_px_missing;
  int supposed_pond_factor;

  uint16_t * cur_p;
  uint16_t * cur_p_left;
  uint16_t * cur_p_right;
  uint16_t * cur_p_up;
  uint16_t * cur_p_down;
  uint32_t red_comp, green_comp, blue_comp;
  //printf("virtual_screen->w=%d, virtual_screen->w=%d\n", virtual_screen->w, virtual_screen->h);

  ///Debug
  /*int occurence_pond[9];
  memset(occurence_pond, 0, 9*sizeof(int));*/

  for (int i=0;i<h2;i++)
  {
    if(i>=RES_HW_SCREEN_VERTICAL){
      continue;
    }
    uint16_t* t = (uint16_t*)(hardware_screen->pixels+( (i+y_padding)*((w2>RES_HW_SCREEN_HORIZONTAL)?RES_HW_SCREEN_HORIZONTAL:w2))*sizeof(uint16_t));
    // ------ current and next y value ------
    y1 = ((i*y_ratio)>>16);
    px_diff_next_y = MAX( (((i+1)*y_ratio)>>16) - y1, 1);
    uint16_t* p = (uint16_t*)(virtual_screen->pixels + (y1*w1+x_padding_ratio) *sizeof(uint16_t));
    int rat = 0;
    for (int j=0;j<w2;j++)
    {
      if(j>=RES_HW_SCREEN_HORIZONTAL){
        continue;
      }
      // ------ current x value ------
      x1 = (rat>>16);
      px_diff_next_x = ((rat+x_ratio)>>16) - x1;

      // ------ adapted bilinear with 3x3 gaussian blur -------
      cur_p = p+x1;
      if(px_diff_prev_x > 1 || px_diff_next_x > 1 || px_diff_prev_y > 1 || px_diff_next_y > 1){
        red_comp=((*cur_p)&0xF800) << 1;
        green_comp=((*cur_p)&0x07E0) << 1;
        blue_comp=((*cur_p)&0x001F) << 1;
        ponderation_factor = 2;
        left_px_missing = (px_diff_prev_x > 1 && x1>0);
        right_px_missing = (px_diff_next_x > 1 && x1+1<w1);
        up_px_missing = (px_diff_prev_y > 1 && y1 > 0);
        down_px_missing = (px_diff_next_y > 1 && y1 + 1 < h1);
        supposed_pond_factor = 2 + left_px_missing + right_px_missing +
                                       up_px_missing + down_px_missing;

        // ---- Interpolate current and up ----
        if(up_px_missing){
          cur_p_up = p+x1-w1;

          if(supposed_pond_factor==3){
            red_comp += ((*cur_p_up)&0xF800) << 1;
            green_comp += ((*cur_p_up)&0x07E0) << 1;
            blue_comp += ((*cur_p_up)&0x001F) << 1;
            ponderation_factor+=2;
          }
          else if(supposed_pond_factor == 4 ||
                  (supposed_pond_factor == 5 && !down_px_missing) ||
                  supposed_pond_factor == 6 ){
            red_comp += ((*cur_p_up)&0xF800);
            green_comp += ((*cur_p_up)&0x07E0);
            blue_comp += ((*cur_p_up)&0x001F);
            ponderation_factor++;
          }
        }

        // ---- Interpolate current and left ----
        if(left_px_missing){
          cur_p_left = p+x1-1;

          if(supposed_pond_factor==3){
            red_comp += ((*cur_p_left)&0xF800) << 1;
            green_comp += ((*cur_p_left)&0x07E0) << 1;
            blue_comp += ((*cur_p_left)&0x001F) << 1;
            ponderation_factor+=2;
          }
          else if(supposed_pond_factor == 4 ||
                  (supposed_pond_factor == 5 && !right_px_missing) ||
                  supposed_pond_factor == 6 ){
            red_comp += ((*cur_p_left)&0xF800);
            green_comp += ((*cur_p_left)&0x07E0);
            blue_comp += ((*cur_p_left)&0x001F);
            ponderation_factor++;
          }
        }

        // ---- Interpolate current and down ----
        if(down_px_missing){
          cur_p_down = p+x1+w1;

          if(supposed_pond_factor==3 || supposed_pond_factor==6){
            red_comp += ((*cur_p_down)&0xF800) << 1;
            green_comp += ((*cur_p_down)&0x07E0) << 1;
            blue_comp += ((*cur_p_down)&0x001F) << 1;
            ponderation_factor+=2;
          }
          else if(supposed_pond_factor >= 4 && supposed_pond_factor != 6){
            red_comp += ((*cur_p_down)&0xF800);
            green_comp += ((*cur_p_down)&0x07E0);
            blue_comp += ((*cur_p_down)&0x001F);
            ponderation_factor++;
          }
        }

        // ---- Interpolate current and right ----
        if(right_px_missing){
          cur_p_right = p+x1+1;

          if(supposed_pond_factor==3 || supposed_pond_factor==6){
            red_comp += ((*cur_p_right)&0xF800) << 1;
            green_comp += ((*cur_p_right)&0x07E0) << 1;
            blue_comp += ((*cur_p_right)&0x001F) << 1;
            ponderation_factor+=2;
          }
          else if(supposed_pond_factor >= 4 && supposed_pond_factor != 6){
            red_comp += ((*cur_p_right)&0xF800);
            green_comp += ((*cur_p_right)&0x07E0);
            blue_comp += ((*cur_p_right)&0x001F);
            ponderation_factor++;
          }
        }

        /// --- Compute new px value ---
        if(ponderation_factor==8){
          red_comp = (red_comp >> 3)&0xF800;
          green_comp = (green_comp >> 3)&green_mask;
          blue_comp = (blue_comp >> 3)&0x001F;
        }
        else if(ponderation_factor==4){
          red_comp = (red_comp >> 2)&0xF800;
          green_comp = (green_comp >> 2)&green_mask;
          blue_comp = (blue_comp >> 2)&0x001F;
        }
        else if(ponderation_factor==2){
          red_comp = (red_comp >> 1)&0xF800;
          green_comp = (green_comp >> 1)&green_mask;
          blue_comp = (blue_comp >> 1)&0x001F;
        }
        else{
          red_comp = (red_comp / ponderation_factor )&0xF800;
          green_comp = (green_comp / ponderation_factor )&green_mask;
          blue_comp = (blue_comp / ponderation_factor )&0x001F;
        }

        /// Debug
        //occurence_pond[ponderation_factor] += 1;

        /// --- write pixel ---
        *t++ = red_comp+green_comp+blue_comp;
      }
      else{
        /// --- copy pixel ---
#ifdef BLACKER_BLACKS
        /// Optimization for blacker blacks (our screen do not handle green value of 1 very well)
        *t++ = (*cur_p)&0xFFDF;
#else
        *t++ = (*cur_p);
#endif

        /// Debug
        //occurence_pond[1] += 1;
      }

      /// save number of pixels to interpolate
      px_diff_prev_x = px_diff_next_x;

      // ------ next pixel ------
      rat += x_ratio;
    }
    px_diff_prev_y = px_diff_next_y;
  }
  /// Debug
  /*printf("pond: [%d, %d, %d, %d, %d, %d, %d, %d]\n", occurence_pond[1], occurence_pond[2], occurence_pond[3],
                                              occurence_pond[4], occurence_pond[5], occurence_pond[6],
                                              occurence_pond[7], occurence_pond[8]);*/
}


/// Nearest neighbor with full 2D uniform bilinear  (interpolation with missing left, right, up and down pixels)
void flip_NNOptimized_FullBilinear_Uniform(SDL_Surface *virtual_screen, SDL_Surface *hardware_screen, int new_w, int new_h){
  int w1=virtual_screen->w;
  int h1=virtual_screen->h;
  int w2=new_w;
  int h2=new_h;
  int y_padding = (RES_HW_SCREEN_VERTICAL-new_h)/2;
  //int x_ratio = (int)((w1<<16)/w2) +1;
  //int y_ratio = (int)((h1<<16)/h2) +1;
  int x_ratio = (int)((w1<<16)/w2);
  int y_ratio = (int)((h1<<16)/h2);
  int x1, y1;
  int px_diff_prev_x = 1;
  int px_diff_prev_y = 1;
  //int cnt_interp = 0; int cnt_no_interp = 0;
  //printf("virtual_screen->w=%d, virtual_screen->w=%d\n", virtual_screen->w, virtual_screen->h);

  /// ---- Compute padding for centering when out of bounds ----
  int x_padding = 0;
  if(w2>RES_HW_SCREEN_HORIZONTAL){
    x_padding = (w2-RES_HW_SCREEN_HORIZONTAL)/2 + 1;
  }
  int x_padding_ratio = x_padding*w1/w2;

  /// ---- Copy and interpolate pixels ----
  for (int i=0;i<h2;i++)
  {
    if(i>=RES_HW_SCREEN_VERTICAL){
      continue;
    }

    uint16_t* t = (uint16_t*)(hardware_screen->pixels+( (i+y_padding)*((w2>RES_HW_SCREEN_HORIZONTAL)?RES_HW_SCREEN_HORIZONTAL:w2))*sizeof(uint16_t));

    // ------ current and next y value ------
    y1 = ((i*y_ratio)>>16);
    int px_diff_next_y = MAX( (((i+1)*y_ratio)>>16) - y1, 1);

    uint16_t* p = (uint16_t*)(virtual_screen->pixels + (y1*w1 + x_padding_ratio) *sizeof(uint16_t));
    int rat = 0;
    for (int j=0;j<w2;j++)
    {
      if(j>=RES_HW_SCREEN_HORIZONTAL){
        continue;
      }

      // ------ current and next x value ------
      x1 = (rat>>16);
      int px_diff_next_x = MAX( ((rat+x_ratio)>>16) - x1, 1);

      // ------ bilinear uniformly weighted --------
      uint32_t red_comp=0, green_comp=0, blue_comp=0, ponderation_factor=0;
      uint16_t * cur_p;
      int cur_y_offset;

      //printf("\npx_diff_prev_y=%d, px_diff_prev_x=%d, px_diff_next_y=%d, px_diff_next_x=%d, interp_px=", px_diff_prev_y, px_diff_prev_x, px_diff_next_y, px_diff_next_x);

      for(int cur_px_diff_y=-(px_diff_prev_y-1); cur_px_diff_y<px_diff_next_y; cur_px_diff_y++){
        if(y1 + cur_px_diff_y >= h1 || y1 < -cur_px_diff_y){
          continue;
        }
        cur_y_offset = w1*cur_px_diff_y;
        //printf("cur_diff_y=%d-> ", cur_px_diff_y);

        for(int cur_px_diff_x=-(px_diff_prev_x-1); cur_px_diff_x<px_diff_next_x; cur_px_diff_x++){
          if(x1 + cur_px_diff_x >= w1 || x1 < -cur_px_diff_x){
            continue;
          }
          cur_p = (p+cur_y_offset+x1+cur_px_diff_x);
          //printf("{y=%d,x=%d}, ", y1+cur_px_diff_y, x1+cur_px_diff_x);
          red_comp += ((*cur_p)&0xF800);
          green_comp += ((*cur_p)&0x07E0);
          blue_comp += ((*cur_p)&0x001F);
          ponderation_factor++;
        }
      }
      //printf("\n");

      /// ------ Ponderation -------
      red_comp = (red_comp / ponderation_factor )&0xF800;
#ifdef BLACKER_BLACKS
      /// Optimization for blacker blacks (our screen do not handle green value of 1 very well)
      green_comp = (green_comp / ponderation_factor )&0x07C0;
#else
      green_comp = (green_comp / ponderation_factor )&0x07E0;
#endif
      blue_comp = (blue_comp / ponderation_factor )&0x001F;
      *t++ = red_comp+green_comp+blue_comp;

      /// ------ x Interpolation values -------
      px_diff_prev_x = px_diff_next_x;

      // ------ next pixel ------
      rat += x_ratio;
    }

    /// ------ y Interpolation values -------
    px_diff_prev_y = px_diff_next_y;
  }
  //printf("cnt_interp = %d, int cnt_no_interp = %d\n", cnt_interp, cnt_no_interp);
}


/// Nearest neighbor with full 2D uniform bilinear  (interpolation with missing left, right, up and down pixels)
void flip_NNOptimized_FullBilinear_GaussianWeighted(SDL_Surface *virtual_screen, SDL_Surface *hardware_screen, int new_w, int new_h){
  int w1=virtual_screen->w;
  int h1=virtual_screen->h;
  int w2=new_w;
  int h2=new_h;
  //printf("virtual_screen->w=%d, virtual_screen->w=%d\n", virtual_screen->w, virtual_screen->h);
  int y_padding = (RES_HW_SCREEN_VERTICAL-new_h)/2;
  int x_ratio = (int)((w1<<16)/w2);
  int y_ratio = (int)((h1<<16)/h2);
  int x1, y1;
  int px_diff_prev_x = 1;
  int px_diff_prev_y = 1;
  //int cnt_interp = 0; int cnt_no_interp = 0;

  /// ---- Compute padding for centering when out of bounds ----
  int x_padding = 0;
  if(w2>RES_HW_SCREEN_HORIZONTAL){
    x_padding = (w2-RES_HW_SCREEN_HORIZONTAL)/2 + 1;
  }
  int x_padding_ratio = x_padding*w1/w2;

  /// ---- Interpolation params ----
  uint32_t max_pix_interpolate = 3;
  if(max_pix_interpolate > 3 || max_pix_interpolate<1){
    printf("ERROR cannot interpolate more than 3x3 px in flip_NNOptimized_FullBilinear_GaussianWeighted\n");
    return;
  }

  /// ---- Convolutional mask ----
  int mask_weight_5x5[] = {36, 24, 6,   24, 16, 4,    6, 4, 1};
  int mask_weight_3x3[] = {4, 2,  2, 1};
  int mask_weight_1x1[] = {1};
  int *mask_weight;
  if(max_pix_interpolate==3){
    mask_weight = mask_weight_5x5;
  }
  else if(max_pix_interpolate==2){
    mask_weight = mask_weight_3x3;
  }
  else{
    mask_weight = mask_weight_1x1;
  }

  /// ---- Copy and interpolate pixels ----
  for (int i=0;i<h2;i++)
  {
    if(i>=RES_HW_SCREEN_VERTICAL){
      continue;
    }

    uint16_t* t = (uint16_t*)(hardware_screen->pixels+( (i+y_padding)*((w2>RES_HW_SCREEN_HORIZONTAL)?RES_HW_SCREEN_HORIZONTAL:w2))*sizeof(uint16_t));

    // ------ current and next y value ------
    y1 = ((i*y_ratio)>>16);
    int px_diff_next_y = MIN( MAX( (((i+1)*y_ratio)>>16) - y1, 1), max_pix_interpolate);

    uint16_t* p = (uint16_t*)(virtual_screen->pixels + (y1*w1 + x_padding_ratio) *sizeof(uint16_t));
    int rat = 0;
    for (int j=0;j<w2;j++)
    {
      if(j>=RES_HW_SCREEN_HORIZONTAL){
        continue;
      }

      // ------ current and next x value ------
      x1 = (rat>>16);
      int px_diff_next_x = MIN( MAX( ((rat+x_ratio)>>16) - x1, 1), max_pix_interpolate); //we interpolate max "max_pix_interpolate" pix in each dim

      // ------ bilinear uniformly weighted --------
      uint32_t red_comp=0, green_comp=0, blue_comp=0;
      int ponderation_factor=0;
      uint16_t * cur_p;
      int cur_y_offset;

      //printf("\npx_diff_prev_y=%d, px_diff_prev_x=%d, px_diff_next_y=%d, px_diff_next_x=%d, interp_px=", px_diff_prev_y, px_diff_prev_x, px_diff_next_y, px_diff_next_x);

      for(int cur_px_diff_y=-(px_diff_prev_y-1); cur_px_diff_y<px_diff_next_y; cur_px_diff_y++){
        if(y1 + cur_px_diff_y >= h1 || y1 < -cur_px_diff_y){
          continue;
        }
        cur_y_offset = w1*cur_px_diff_y;
        //printf("cur_diff_y=%d-> ", cur_px_diff_y);

        for(int cur_px_diff_x=-(px_diff_prev_x-1); cur_px_diff_x<px_diff_next_x; cur_px_diff_x++){
          if(x1 + cur_px_diff_x >= w1 || x1 < -cur_px_diff_x){
            continue;
          }
          cur_p = (p+cur_y_offset+x1+cur_px_diff_x);
          int weight = mask_weight[ABS(cur_px_diff_y)*max_pix_interpolate+ABS(cur_px_diff_x)];

          red_comp += ((*cur_p)&0xF800) * weight;
          green_comp += ((*cur_p)&0x07E0) * weight;
          blue_comp += ((*cur_p)&0x001F) * weight;
          ponderation_factor += weight;
        }
      }
      //printf("\n");

      /// ------ Ponderation -------
      red_comp = (red_comp / ponderation_factor) & 0xF800;
#ifdef BLACKER_BLACKS
      /// Optimization for blacker blacks (our screen do not handle green value of 1 very well)
      green_comp = (green_comp / ponderation_factor )&0x07C0;
#else
      green_comp = (green_comp / ponderation_factor )&0x07E0;
#endif
      blue_comp = (blue_comp / ponderation_factor) & 0x001F;
      *t++ = red_comp+green_comp+blue_comp;

      /// ------ x Interpolation values -------
      px_diff_prev_x = px_diff_next_x;

      // ------ next pixel ------
      rat += x_ratio;
    }

    /// ------ y Interpolation values -------
    px_diff_prev_y = px_diff_next_y;
  }
  //printf("cnt_interp = %d, int cnt_no_interp = %d\n", cnt_interp, cnt_no_interp);
}

void SDL_Rotate_270(SDL_Surface * hw_surface, SDL_Surface * virtual_hw_surface){
    int i, j;
    uint16_t *source_pixels = (uint16_t*) virtual_hw_surface->pixels;
    uint16_t *dest_pixels = (uint16_t*) hw_surface->pixels;

    /// --- Checking for right pixel format ---
    //printf("Source bpb = %d, Dest bpb = %d\n", virtual_hw_surface->format->BitsPerPixel, hw_surface->format->BitsPerPixel);
    if(virtual_hw_surface->format->BitsPerPixel != 16){
	printf("Error in SDL_FastBlit, Wrong virtual_hw_surface pixel format: %d bpb, expected: 16 bpb\n", virtual_hw_surface->format->BitsPerPixel);
	return;
    }
    if(hw_surface->format->BitsPerPixel != 16){
	printf("Error in SDL_FastBlit, Wrong hw_surface pixel format: %d bpb, expected: 16 bpb\n", hw_surface->format->BitsPerPixel);
	return;
    }

    /// --- Checking if same dimensions ---
    if(hw_surface->w != virtual_hw_surface->w || hw_surface->h != virtual_hw_surface->h){
	printf("Error in SDL_FastBlit, hw_surface (%dx%d) and virtual_hw_surface (%dx%d) have different dimensions\n",
	       hw_surface->w, hw_surface->h, virtual_hw_surface->w, virtual_hw_surface->h);
	return;
    }

	/// --- Pixel copy and rotation (270) ---
	uint16_t *cur_p_src, *cur_p_dst;
	for(i=0; i<virtual_hw_surface->h; i++){
		for(j=0; j<virtual_hw_surface->w; j++){
			cur_p_src = source_pixels + i*virtual_hw_surface->w + j;
			cur_p_dst = dest_pixels + (hw_surface->h-1-j)*hw_surface->w + i;
			*cur_p_dst = *cur_p_src;
		}
	}
}

void plat_video_flip(void)
{
	if (plat_sdl_overlay != NULL) {
		SDL_Rect dstrect =
			{ 0, 0, plat_sdl_screen->w, plat_sdl_screen->h };

		SDL_LockYUVOverlay(plat_sdl_overlay);
		rgb565_to_uyvy(plat_sdl_overlay->pixels[0], shadow_fb,
				g_screen_ppitch * g_screen_height);
		SDL_UnlockYUVOverlay(plat_sdl_overlay);
		SDL_DisplayYUVOverlay(plat_sdl_overlay, &dstrect);
	}
	else if (plat_sdl_gl_active) {
		gl_flip(shadow_fb, g_screen_ppitch, g_screen_height);
	}
	else {
		if (SDL_MUSTLOCK(plat_sdl_screen))
			SDL_UnlockSurface(plat_sdl_screen);

		/// --------------Optimized Flip depending on aspect ratio -------------
		static int prev_aspect_ratio;
		if(prev_aspect_ratio != aspect_ratio || need_screen_cleared){
			//printf("aspect ratio changed: %d\n", aspect_ratio);
			clear_screen(virtual_hw_screen, 0);
			prev_aspect_ratio = aspect_ratio;
			need_screen_cleared = 0;
		}

		switch(aspect_ratio){
		case ASPECT_RATIOS_TYPE_STRECHED:
			flip_NNOptimized_LeftAndRightBilinear(plat_sdl_screen, virtual_hw_screen,
							      RES_HW_SCREEN_HORIZONTAL, RES_HW_SCREEN_VERTICAL);
			break;
		case ASPECT_RATIOS_TYPE_MANUAL:
			;uint32_t h_scaled = MIN(plat_sdl_screen->h*RES_HW_SCREEN_HORIZONTAL/plat_sdl_screen->w,
						 RES_HW_SCREEN_VERTICAL);
			uint32_t h_zoomed = MIN(h_scaled + aspect_ratio_factor_percent*(RES_HW_SCREEN_VERTICAL - h_scaled)/100,
						RES_HW_SCREEN_VERTICAL);
			flip_NNOptimized_LeftRightUpDownBilinear_Optimized8(plat_sdl_screen, virtual_hw_screen,
									    MAX(plat_sdl_screen->w*h_zoomed/plat_sdl_screen->h, RES_HW_SCREEN_HORIZONTAL),
									    MIN(h_zoomed, RES_HW_SCREEN_VERTICAL));
			break;
	    	case ASPECT_RATIOS_TYPE_CROPPED:
			flip_NNOptimized_AllowOutOfScreen(plat_sdl_screen, virtual_hw_screen,
							  MAX(plat_sdl_screen->w*RES_HW_SCREEN_VERTICAL/plat_sdl_screen->h, RES_HW_SCREEN_HORIZONTAL),
							  RES_HW_SCREEN_VERTICAL);
			break;
		case ASPECT_RATIOS_TYPE_SCALED:
			flip_NNOptimized_LeftRightUpDownBilinear_Optimized8(plat_sdl_screen, virtual_hw_screen,
									    RES_HW_SCREEN_HORIZONTAL,
									    MIN(plat_sdl_screen->h*RES_HW_SCREEN_HORIZONTAL/plat_sdl_screen->w, RES_HW_SCREEN_VERTICAL));
			break;
		default:
			printf("Wrong aspect ratio value: %d\n", aspect_ratio);
			aspect_ratio = ASPECT_RATIOS_TYPE_STRECHED;
			flip_NNOptimized_LeftRightUpDownBilinear_Optimized8(plat_sdl_screen, virtual_hw_screen,
									    RES_HW_SCREEN_HORIZONTAL, RES_HW_SCREEN_VERTICAL);
			break;
		}

		// Rotate
		//SDL_Rotate_270(hw_screen, virtual_hw_screen);
		//SDL_BlitSurface(virtual_hw_screen, NULL, hw_screen, NULL);
		memcpy(hw_screen->pixels, virtual_hw_screen->pixels, hw_screen->w*hw_screen->h*sizeof(uint16_t));

		/// --- Real Flip ---
		SDL_Flip(hw_screen);


		g_screen_ptr = plat_sdl_screen->pixels;
		PicoDrawSetOutBuf(g_screen_ptr, g_screen_ppitch * 2);
	}
}

void plat_video_wait_vsync(void)
{
}

void plat_video_menu_enter(int is_rom_loaded)
{
	plat_sdl_change_video_mode(g_menuscreen_w, g_menuscreen_h, 0);
	g_screen_ptr = shadow_fb;
}

void plat_video_menu_begin(void)
{
	if (plat_sdl_overlay != NULL || plat_sdl_gl_active) {
		g_menuscreen_ptr = shadow_fb;
	}
	else {
		if (SDL_MUSTLOCK(plat_sdl_screen))
			SDL_LockSurface(plat_sdl_screen);
		g_menuscreen_ptr = plat_sdl_screen->pixels;
	}
}

void plat_video_menu_end(void)
{
	if (plat_sdl_overlay != NULL) {
		SDL_Rect dstrect =
			{ 0, 0, plat_sdl_screen->w, plat_sdl_screen->h };

		SDL_LockYUVOverlay(plat_sdl_overlay);
		rgb565_to_uyvy(plat_sdl_overlay->pixels[0], shadow_fb,
				g_menuscreen_pp * g_menuscreen_h);
		SDL_UnlockYUVOverlay(plat_sdl_overlay);

		SDL_DisplayYUVOverlay(plat_sdl_overlay, &dstrect);
	}
	else if (plat_sdl_gl_active) {
		gl_flip(g_menuscreen_ptr, g_menuscreen_pp, g_menuscreen_h);
	}
	else {
		if (SDL_MUSTLOCK(plat_sdl_screen))
			SDL_UnlockSurface(plat_sdl_screen);
		flip_NNOptimized_LeftAndRightBilinear(plat_sdl_screen, virtual_hw_screen, RES_HW_SCREEN_HORIZONTAL, RES_HW_SCREEN_VERTICAL);
		//SDL_Flip(hw_screen);
		SDL_Rotate_270(hw_screen, virtual_hw_screen);
		//SDL_Flip(plat_sdl_screen);
	}
	g_menuscreen_ptr = NULL;

}

void plat_video_menu_leave(void)
{
}

void plat_video_loop_prepare(void)
{
	plat_sdl_change_video_mode(g_screen_width, g_screen_height, 0);

	if (plat_sdl_overlay != NULL || plat_sdl_gl_active) {
		g_screen_ptr = shadow_fb;
	}
	else {
		if (SDL_MUSTLOCK(plat_sdl_screen))
			SDL_LockSurface(plat_sdl_screen);
		g_screen_ptr = plat_sdl_screen->pixels;
	}
	PicoDrawSetOutBuf(g_screen_ptr, g_screen_ppitch * 2);
}

void plat_early_init(void)
{
}

static void plat_sdl_quit(void)
{
	// for now..
	engineState = PGS_Quit;
	//exit(1);
}

void plat_init(void)
{
	int shadow_size;
	int ret;

	ret = plat_sdl_init();
	if (ret != 0)
		exit(1);

	if(TTF_Init())
	{
		fprintf(stderr, "Error TTF_Init: %s\n", TTF_GetError());
		exit(EXIT_FAILURE);
	}

	hw_screen = SDL_SetVideoMode(RES_HW_SCREEN_HORIZONTAL, RES_HW_SCREEN_VERTICAL, 16, SDL_FULLSCREEN | SDL_HWSURFACE | SDL_DOUBLEBUF);
	if(hw_screen == NULL)
	{
		fprintf(stderr, "Error SDL_SetVideoMode: %s\n", SDL_GetError());
		exit(EXIT_FAILURE);
	}

	plat_sdl_quit_cb = plat_sdl_quit;

	SDL_WM_SetCaption("PicoDrive " VERSION, NULL);

	virtual_hw_screen = SDL_CreateRGBSurface(SDL_SWSURFACE,
      RES_HW_SCREEN_HORIZONTAL, RES_HW_SCREEN_VERTICAL, 16, 0xFFFF, 0xFFFF, 0xFFFF, 0);
    if (virtual_hw_screen == NULL) {
      fprintf(stderr, "virtual_hw_screen failed: %s\n", SDL_GetError());
    }

	g_menuscreen_w = plat_sdl_screen->w;
	g_menuscreen_h = plat_sdl_screen->h;
	g_menuscreen_pp = g_menuscreen_w;
	g_menuscreen_ptr = NULL;

	shadow_size = g_menuscreen_w * g_menuscreen_h * 2;
	if (shadow_size < 320 * 480 * 2)
		shadow_size = 320 * 480 * 2;

	shadow_fb = malloc(shadow_size);
	g_menubg_ptr = calloc(1, shadow_size);
	if (shadow_fb == NULL || g_menubg_ptr == NULL) {
		fprintf(stderr, "OOM\n");
		exit(1);
	}

	g_screen_width = 320;
	g_screen_height = 240;
	g_screen_ppitch = 320;
	g_screen_ptr = shadow_fb;

	in_sdl_init(&in_sdl_platform_data, plat_sdl_event_handler);
	in_probe();


	init_menu_SDL();
	init_menu_zones();
	init_menu_system_values();
	bgr_to_uyvy_init();
}

void plat_finish(void)
{
	SDL_FreeSurface(virtual_hw_screen);
	deinit_menu_SDL();
	free(shadow_fb);
	shadow_fb = NULL;
	free(g_menubg_ptr);
	g_menubg_ptr = NULL;
	TTF_Quit();
	plat_sdl_finish();
}
