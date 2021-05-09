/*
 * PicoDrive
 * (C) notaz, 2010,2011
 *
 * This work is licensed under the terms of MAME license.
 * See COPYING file in the top-level directory.
 */
#include <stdio.h>
#include <string.h>
#include <time.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <SDL/SDL_ttf.h>
#include <SDL/SDL_image.h>

#include "emu.h"
#include "menu_pico.h"
#include "configfile_fk.h"
#include "input_pico.h"
#include "../libpicofe/input.h"
#include "version.h"

#include <pico/pico_int.h>
#include <pico/patch.h>

#ifdef PANDORA
#define MENU_X2 1
#else
#define MENU_X2 0
#endif

// FIXME
#define REVISION "0"

static const char *rom_exts[] = {
	"zip",
	"bin", "smd", "gen", "md",
	"iso", "cso", "cue",
	"32x",
	"sms",
	NULL
};

// rrrr rggg gggb bbbb
static unsigned short fname2color(const char *fname)
{
	static const char *other_exts[] = { "gmv", "pat" };
	const char *ext;
	int i;

	ext = strrchr(fname, '.');
	if (ext++ == NULL) {
		ext = fname + strlen(fname) - 3;
		if (ext < fname) ext = fname;
	}

	for (i = 0; rom_exts[i] != NULL; i++)
		if (strcasecmp(ext, rom_exts[i]) == 0) return 0xbdff; // FIXME: mk defines
	for (i = 0; i < array_size(other_exts); i++)
		if (strcasecmp(ext, other_exts[i]) == 0) return 0xaff5;
	return 0xffff;
}

#include "../libpicofe/menu.c"

static const char *men_dummy[] = { NULL };

/* platform specific options and handlers */
#if   defined(__GP2X__)
#include "../gp2x/menu.c"
#elif defined(PANDORA)
#include "../pandora/menu.c"
#else
#define MENU_OPTIONS_GFX
#define MENU_OPTIONS_ADV
#endif











/// -------------- DEFINES --------------
#define MIN(a,b) (((a)<(b))?(a):(b))
#define MAX(a,b) (((a)>(b))?(a):(b))
#define ABS(x) (((x) < 0) ? (-x) : (x))

//#define MENU_DEBUG
#define MENU_ERROR

#ifdef MENU_DEBUG
#define MENU_DEBUG_PRINTF(...)   printf(__VA_ARGS__);
#else
#define MENU_DEBUG_PRINTF(...)
#endif //MENU_DEBUG

#ifdef MENU_ERROR
#define MENU_ERROR_PRINTF(...)   printf(__VA_ARGS__);
#else
#define MENU_ERROR_PRINTF(...)
#endif //MENU_ERROR


#define RES_HW_SCREEN_HORIZONTAL    240
#define RES_HW_SCREEN_VERTICAL      240
#define SCREEN_HORIZONTAL_SIZE      240
#define SCREEN_VERTICAL_SIZE        240

#define SCROLL_SPEED_PX             30
#define FPS_MENU                    50
#define ARROWS_PADDING              8

#define MENU_ZONE_WIDTH             SCREEN_HORIZONTAL_SIZE
#define MENU_ZONE_HEIGHT            SCREEN_VERTICAL_SIZE
#define MENU_BG_SQUARE_WIDTH        180
#define MENU_BG_SQUARE_HEIGHT       140

#define MENU_FONT_NAME_TITLE        "/usr/games/menu_resources/OpenSans-Bold.ttf"
#define MENU_FONT_SIZE_TITLE        22
#define MENU_FONT_NAME_INFO         "/usr/games/menu_resources/OpenSans-Bold.ttf"
#define MENU_FONT_SIZE_INFO         16
#define MENU_FONT_NAME_SMALL_INFO   "/usr/games/menu_resources/OpenSans-Regular.ttf"
#define MENU_FONT_SIZE_SMALL_INFO   13
#define MENU_PNG_BG_PATH            "/usr/games/menu_resources/zone_bg.png"
#define MENU_PNG_ARROW_TOP_PATH     "/usr/games/menu_resources/arrow_top.png"
#define MENU_PNG_ARROW_BOTTOM_PATH  "/usr/games/menu_resources/arrow_bottom.png"

#define GRAY_MAIN_R                 85
#define GRAY_MAIN_G                 85
#define GRAY_MAIN_B                 85
#define WHITE_MAIN_R                236
#define WHITE_MAIN_G                236
#define WHITE_MAIN_B                236

#define MAX_SAVE_SLOTS              9

#define MAXPATHLEN                  512



/// -------------- STATIC VARIABLES --------------
extern SDL_Surface * hw_screen;
extern SDL_Surface * virtual_hw_screen;
SDL_Surface * draw_screen;

static int backup_key_repeat_delay, backup_key_repeat_interval;
static SDL_Surface * backup_hw_screen = NULL;

static TTF_Font *menu_title_font = NULL;
static TTF_Font *menu_info_font = NULL;
static TTF_Font *menu_small_info_font = NULL;
static SDL_Surface *img_arrow_top = NULL;
static SDL_Surface *img_arrow_bottom = NULL;
static SDL_Surface ** menu_zone_surfaces = NULL;
static int * idx_menus = NULL;
static int nb_menu_zones = 0;
static int menuItem = 0;
int stop_menu_loop = 0;

static SDL_Color text_color = {GRAY_MAIN_R, GRAY_MAIN_G, GRAY_MAIN_B};
static int padding_y_from_center_menu_zone = 18;
static uint16_t width_progress_bar = 100;
static uint16_t height_progress_bar = 20;
static uint16_t x_volume_bar = 0;
static uint16_t y_volume_bar = 0;
static uint16_t x_brightness_bar = 0;
static uint16_t y_brightness_bar = 0;

int volume_percentage = 0;
int brightness_percentage = 0;

static int quick_load_slot_chosen = 0;

#undef X
#define X(a, b) b,
const char *resume_options_str[] = {RESUME_OPTIONS};


/// -------------- FUNCTIONS IMPLEMENTATION --------------
void init_menu_SDL(){
    /// ----- Loading the fonts -----
    menu_title_font = TTF_OpenFont(MENU_FONT_NAME_TITLE, MENU_FONT_SIZE_TITLE);
    if(!menu_title_font){
        MENU_ERROR_PRINTF("ERROR in init_menu_SDL: Could not open menu font %s, %s\n", MENU_FONT_NAME_TITLE, SDL_GetError());
    }
    menu_info_font = TTF_OpenFont(MENU_FONT_NAME_INFO, MENU_FONT_SIZE_INFO);
    if(!menu_info_font){
        MENU_ERROR_PRINTF("ERROR in init_menu_SDL: Could not open menu font %s, %s\n", MENU_FONT_NAME_INFO, SDL_GetError());
    }
    menu_small_info_font = TTF_OpenFont(MENU_FONT_NAME_SMALL_INFO, MENU_FONT_SIZE_SMALL_INFO);
    if(!menu_small_info_font){
        MENU_ERROR_PRINTF("ERROR in init_menu_SDL: Could not open menu font %s, %s\n", MENU_FONT_NAME_SMALL_INFO, SDL_GetError());
    }

    /// ----- Copy draw_screen at init ------
    backup_hw_screen = SDL_CreateRGBSurface(SDL_SWSURFACE,
        virtual_hw_screen->w, virtual_hw_screen->h, 16, 0, 0, 0, 0);
    if(backup_hw_screen == NULL){
        MENU_ERROR_PRINTF("ERROR in init_menu_SDL: Could not create backup_hw_screen: %s\n", SDL_GetError());
    }

    draw_screen = SDL_CreateRGBSurface(SDL_SWSURFACE,
        virtual_hw_screen->w, virtual_hw_screen->h, 16, 0, 0, 0, 0);
    if(draw_screen == NULL){
        MENU_ERROR_PRINTF("ERROR in init_menu_SDL: Could not create draw_screen: %s\n", SDL_GetError());
    }

    /// ------ Load arrows imgs -------
    img_arrow_top = IMG_Load(MENU_PNG_ARROW_TOP_PATH);
    if(!img_arrow_top) {
        MENU_ERROR_PRINTF("ERROR IMG_Load: %s\n", IMG_GetError());
    }
    img_arrow_bottom = IMG_Load(MENU_PNG_ARROW_BOTTOM_PATH);
    if(!img_arrow_bottom) {
        MENU_ERROR_PRINTF("ERROR IMG_Load: %s\n", IMG_GetError());
    }

    /// ------ Init menu zones ------
    init_menu_zones();
}

void deinit_menu_SDL(){
    /// ------ Close font -------
    TTF_CloseFont(menu_title_font);
    TTF_CloseFont(menu_info_font);
    TTF_CloseFont(menu_small_info_font);

    /// ------ Free Surfaces -------
    for(int i=0; i < nb_menu_zones; i++){
        SDL_FreeSurface(menu_zone_surfaces[i]);
    }
    SDL_FreeSurface(backup_hw_screen);
    SDL_FreeSurface(draw_screen);

    SDL_FreeSurface(img_arrow_top);
    SDL_FreeSurface(img_arrow_bottom);

    /// ------ Free Menu memory and reset vars -----
    if(idx_menus){
        free(idx_menus);
    }
    idx_menus=NULL;
    nb_menu_zones = 0;
}


void draw_progress_bar(SDL_Surface * surface, uint16_t x, uint16_t y, uint16_t width,
                        uint16_t height, uint8_t percentage, uint16_t nb_bars){
    /// ------ Init Variables ------
    uint16_t line_width = 1; //px
    uint16_t padding_bars_ratio = 3;
    uint16_t nb_full_bars = 0;

    /// ------ Check values ------
    percentage = (percentage > 100)?100:percentage;
    x = (x > (surface->w-1))?(surface->w-1):x;
    y = (y > surface->h-1)?(surface->h-1):y;
    width = (width < line_width*2+1)?(line_width*2+1):width;
    width = (width > surface->w-x-1)?(surface->w-x-1):width;
    height = (height < line_width*2+1)?(line_width*2+1):height;
    height = (height > surface->h-y-1)?(surface->h-y-1):height;
    uint16_t nb_bars_max = ( width * padding_bars_ratio  /  (line_width*2+1) + 1 ) / (padding_bars_ratio+1);
    nb_bars = (nb_bars > nb_bars_max)?nb_bars_max:nb_bars;
    uint16_t bar_width = (width / nb_bars)*padding_bars_ratio/(padding_bars_ratio+1)+1;
    uint16_t bar_padding_x = bar_width/padding_bars_ratio;
    nb_full_bars = nb_bars*percentage/100;

    /// ------ draw full bars ------
    for (int i = 0; i < nb_full_bars; ++i)
    {
        /// ---- draw one bar ----
        //MENU_DEBUG_PRINTF("Drawing filled bar %d\n", i);
        SDL_Rect rect = {x+ i*(bar_width +bar_padding_x),
            y, bar_width, height};
        SDL_FillRect(surface, &rect, SDL_MapRGB(surface->format, GRAY_MAIN_R, GRAY_MAIN_G, GRAY_MAIN_B));
    }

    /// ------ draw full bars ------
    for (int i = 0; i < (nb_bars-nb_full_bars); ++i)
    {
        /// ---- draw one bar ----
        //MENU_DEBUG_PRINTF("Drawing empty bar %d\n", i);
        SDL_Rect rect = {x+ i*(bar_width +bar_padding_x) + nb_full_bars*(bar_width +bar_padding_x),
            y, bar_width, height};
        SDL_FillRect(surface, &rect, SDL_MapRGB(surface->format, GRAY_MAIN_R, GRAY_MAIN_G, GRAY_MAIN_B));

        SDL_Rect rect2 = {x+ i*(bar_width +bar_padding_x) + line_width + nb_full_bars*(bar_width +bar_padding_x),
            y + line_width, bar_width - line_width*2, height - line_width*2};
        SDL_FillRect(surface, &rect2, SDL_MapRGB(surface->format, WHITE_MAIN_R, WHITE_MAIN_R, WHITE_MAIN_R));
    }


}


void add_menu_zone(ENUM_MENU_TYPE menu_type){
    /// ------ Increase nb of menu zones -------
    nb_menu_zones++;

    /// ------ Realoc idx Menus array -------
    if(!idx_menus){
        idx_menus = malloc(nb_menu_zones*sizeof(int));
        menu_zone_surfaces = malloc(nb_menu_zones*sizeof(SDL_Surface*));
    }
    else{
        int *temp = realloc(idx_menus, nb_menu_zones*sizeof(int));
        idx_menus = temp;
        menu_zone_surfaces = realloc(menu_zone_surfaces, nb_menu_zones*sizeof(SDL_Surface*));
    }
    idx_menus[nb_menu_zones-1] = menu_type;

    /// ------ Reinit menu surface with height increased -------
    menu_zone_surfaces[nb_menu_zones-1] = IMG_Load(MENU_PNG_BG_PATH);
    if(!menu_zone_surfaces[nb_menu_zones-1]) {
        MENU_ERROR_PRINTF("ERROR IMG_Load: %s\n", IMG_GetError());
    }
    /// --------- Init Common Variables --------
    SDL_Surface *text_surface = NULL;
    SDL_Surface *surface = menu_zone_surfaces[nb_menu_zones-1];
    SDL_Rect text_pos;

    /// --------- Add new zone ---------
    switch(menu_type){
    case MENU_TYPE_VOLUME:
        MENU_DEBUG_PRINTF("Init MENU_TYPE_VOLUME\n");
        /// ------ Text ------
        text_surface = TTF_RenderText_Blended(menu_title_font, "VOLUME", text_color);
        text_pos.x = (surface->w - MENU_ZONE_WIDTH)/2 + (MENU_ZONE_WIDTH - text_surface->w)/2;
        text_pos.y = surface->h - MENU_ZONE_HEIGHT/2 - text_surface->h/2 - padding_y_from_center_menu_zone;
        SDL_BlitSurface(text_surface, NULL, surface, &text_pos);

        x_volume_bar = (surface->w - MENU_ZONE_WIDTH)/2 + (MENU_ZONE_WIDTH - width_progress_bar)/2;
        y_volume_bar = surface->h - MENU_ZONE_HEIGHT/2 - height_progress_bar/2 + padding_y_from_center_menu_zone;
        draw_progress_bar(surface, x_volume_bar, y_volume_bar,
            width_progress_bar, height_progress_bar, 0, 100/STEP_CHANGE_VOLUME);
        break;
    case MENU_TYPE_BRIGHTNESS:
        MENU_DEBUG_PRINTF("Init MENU_TYPE_BRIGHTNESS\n");
        /// ------ Text ------
        text_surface = TTF_RenderText_Blended(menu_title_font, "BRIGHTNESS", text_color);
        text_pos.x = (surface->w - MENU_ZONE_WIDTH)/2 + (MENU_ZONE_WIDTH - text_surface->w)/2;
        text_pos.y = surface->h - MENU_ZONE_HEIGHT/2 - text_surface->h/2 - padding_y_from_center_menu_zone;
        SDL_BlitSurface(text_surface, NULL, surface, &text_pos);

        x_brightness_bar = (surface->w - MENU_ZONE_WIDTH)/2 + (MENU_ZONE_WIDTH - width_progress_bar)/2;
        y_brightness_bar = surface->h - MENU_ZONE_HEIGHT/2 - height_progress_bar/2 + padding_y_from_center_menu_zone;
        draw_progress_bar(surface, x_brightness_bar, y_brightness_bar,
            width_progress_bar, height_progress_bar, 0, 100/STEP_CHANGE_BRIGHTNESS);
        break;
    case MENU_TYPE_SAVE:
        MENU_DEBUG_PRINTF("Init MENU_TYPE_SAVE\n");
        /// ------ Text ------
        text_surface = TTF_RenderText_Blended(menu_title_font, "SAVE", text_color);
        text_pos.x = (surface->w - MENU_ZONE_WIDTH)/2 + (MENU_ZONE_WIDTH - text_surface->w)/2;
        text_pos.y = surface->h - MENU_ZONE_HEIGHT/2 - text_surface->h/2 - padding_y_from_center_menu_zone*2;
        SDL_BlitSurface(text_surface, NULL, surface, &text_pos);
        break;
    case MENU_TYPE_LOAD:
        MENU_DEBUG_PRINTF("Init MENU_TYPE_LOAD\n");
        /// ------ Text ------
        text_surface = TTF_RenderText_Blended(menu_title_font, "LOAD", text_color);
        text_pos.x = (surface->w - MENU_ZONE_WIDTH)/2 + (MENU_ZONE_WIDTH - text_surface->w)/2;
        text_pos.y = surface->h - MENU_ZONE_HEIGHT/2 - text_surface->h/2 - padding_y_from_center_menu_zone*2;
        SDL_BlitSurface(text_surface, NULL, surface, &text_pos);
        break;
    case MENU_TYPE_ASPECT_RATIO:
        MENU_DEBUG_PRINTF("Init MENU_TYPE_ASPECT_RATIO\n");
        /// ------ Text ------
        text_surface = TTF_RenderText_Blended(menu_title_font, "ASPECT RATIO", text_color);
        text_pos.x = (surface->w - MENU_ZONE_WIDTH)/2 + (MENU_ZONE_WIDTH - text_surface->w)/2;
        text_pos.y = surface->h - MENU_ZONE_HEIGHT/2 - text_surface->h/2 - padding_y_from_center_menu_zone;
        SDL_BlitSurface(text_surface, NULL, surface, &text_pos);
        break;
    case MENU_TYPE_EXIT:
        MENU_DEBUG_PRINTF("Init MENU_TYPE_EXIT\n");
        /// ------ Text ------
        text_surface = TTF_RenderText_Blended(menu_title_font, "EXIT GAME", text_color);
        text_pos.x = (surface->w - MENU_ZONE_WIDTH)/2 + (MENU_ZONE_WIDTH - text_surface->w)/2;
        text_pos.y = surface->h - MENU_ZONE_HEIGHT/2 - text_surface->h/2;
        SDL_BlitSurface(text_surface, NULL, surface, &text_pos);
        break;
    case MENU_TYPE_POWERDOWN:
        MENU_DEBUG_PRINTF("Init MENU_TYPE_POWERDOWN\n");
        /// ------ Text ------
        text_surface = TTF_RenderText_Blended(menu_title_font, "POWERDOWN", text_color);
        text_pos.x = (surface->w - MENU_ZONE_WIDTH)/2 + (MENU_ZONE_WIDTH - text_surface->w)/2;
        text_pos.y = surface->h - MENU_ZONE_HEIGHT/2 - text_surface->h/2;
        SDL_BlitSurface(text_surface, NULL, surface, &text_pos);
        break;
    default:
        MENU_DEBUG_PRINTF("Warning - In add_menu_zone, unknown MENU_TYPE: %d\n", menu_type);
        break;
    }

    /// ------ Free Surfaces -------
    SDL_FreeSurface(text_surface);
}

void init_menu_zones(){
    /// Init Volume Menu
    add_menu_zone(MENU_TYPE_VOLUME);
    /// Init Brightness Menu
    add_menu_zone(MENU_TYPE_BRIGHTNESS);
    /// Init Save Menu
    add_menu_zone(MENU_TYPE_SAVE);
    /// Init Load Menu
    add_menu_zone(MENU_TYPE_LOAD);
    /// Init Aspect Ratio Menu
    add_menu_zone(MENU_TYPE_ASPECT_RATIO);
    /// Init Exit Menu
    add_menu_zone(MENU_TYPE_EXIT);
    /// Init Powerdown Menu
    //add_menu_zone(MENU_TYPE_POWERDOWN);
}


void init_menu_system_values(){
    FILE *fp;
    char res[100];

    /// ------- Get system volume percentage --------
    fp = popen(SHELL_CMD_VOLUME_GET, "r");
    if (fp == NULL) {
        MENU_ERROR_PRINTF("Failed to run command %s\n", SHELL_CMD_VOLUME_GET );
        volume_percentage = 50; ///wrong value: setting default to 50
    }
    else{
        fgets(res, sizeof(res)-1, fp);
        pclose(fp);

        /// Check if Volume is a number (at least the first char)
        if(res[0] < '0' || res[0] > '9'){
            MENU_ERROR_PRINTF("Wrong return value: %s for volume cmd: %s\n",res, SHELL_CMD_VOLUME_GET);
            volume_percentage = 50; ///wrong value: setting default to 50
        }
        else{
            volume_percentage = atoi(res);
            MENU_DEBUG_PRINTF("System volume = %d%%\n", volume_percentage);
        }
    }

    /// ------- Get system brightness percentage -------
    fp = popen(SHELL_CMD_BRIGHTNESS_GET, "r");
    if (fp == NULL) {
        MENU_ERROR_PRINTF("Failed to run command %s\n", SHELL_CMD_BRIGHTNESS_GET );
        brightness_percentage = 50; ///wrong value: setting default to 50
    }
    else{
        fgets(res, sizeof(res)-1, fp);
        pclose(fp);

        /// Check if brightness is a number (at least the first char)
        if(res[0] < '0' || res[0] > '9'){
            MENU_ERROR_PRINTF("Wrong return value: %s for volume cmd: %s\n",res, SHELL_CMD_BRIGHTNESS_GET);
            brightness_percentage = 50; ///wrong value: setting default to 50
        }
        else{
            brightness_percentage = atoi(res);
            MENU_DEBUG_PRINTF("System brightness = %d%%\n", brightness_percentage);
        }
    }

    /// ------ Save prev key repeat params and set new Key repeat -------
    SDL_GetKeyRepeat(&backup_key_repeat_delay, &backup_key_repeat_interval);
    if(SDL_EnableKeyRepeat(SDL_DEFAULT_REPEAT_DELAY, SDL_DEFAULT_REPEAT_INTERVAL)){
        MENU_ERROR_PRINTF("ERROR with SDL_EnableKeyRepeat: %s\n", SDL_GetError());
    }

    /// Get save slot from game
    state_slot = (state_slot%MAX_SAVE_SLOTS); // security
}

void menu_screen_refresh(int menuItem, int prevItem, int scroll, uint8_t menu_confirmation, uint8_t menu_action){
    /// --------- Vars ---------
    int print_arrows = (scroll==0)?1:0;

    /// --------- Clear HW screen ----------
    //SDL_FillRect(draw_screen, NULL, SDL_MapRGB(draw_screen->format, 255, 0, 0));
    if(SDL_BlitSurface(backup_hw_screen, NULL, draw_screen, NULL)){
        MENU_ERROR_PRINTF("ERROR Could not Clear draw_screen: %s\n", SDL_GetError());
    }

    /// --------- Setup Blit Window ----------
    SDL_Rect menu_blit_window;
    menu_blit_window.x = 0;
    menu_blit_window.w = SCREEN_HORIZONTAL_SIZE;

    /// --------- Blit prev menu Zone going away ----------
    menu_blit_window.y = scroll;
    menu_blit_window.h = SCREEN_VERTICAL_SIZE;
    if(SDL_BlitSurface(menu_zone_surfaces[prevItem], &menu_blit_window, draw_screen, NULL)){
        MENU_ERROR_PRINTF("ERROR Could not Blit surface on draw_screen: %s\n", SDL_GetError());
    }

    /// --------- Blit new menu Zone going in (only during animations) ----------
    if(scroll>0){
        menu_blit_window.y = SCREEN_VERTICAL_SIZE-scroll;
        menu_blit_window.h = SCREEN_VERTICAL_SIZE;
        if(SDL_BlitSurface(menu_zone_surfaces[menuItem], NULL, draw_screen, &menu_blit_window)){
            MENU_ERROR_PRINTF("ERROR Could not Blit surface on draw_screen: %s\n", SDL_GetError());
        }
    }
    else if(scroll<0){
        menu_blit_window.y = SCREEN_VERTICAL_SIZE+scroll;
        menu_blit_window.h = SCREEN_VERTICAL_SIZE;
        if(SDL_BlitSurface(menu_zone_surfaces[menuItem], &menu_blit_window, draw_screen, NULL)){
            MENU_ERROR_PRINTF("ERROR Could not Blit surface on draw_screen: %s\n", SDL_GetError());
        }
    }
    /// --------- No Scroll ? Blitting menu-specific info
    else{
        SDL_Surface * text_surface = NULL;
        char text_tmp[40];
        SDL_Rect text_pos;
        /*char fname[MAXPATHLEN];
        memset(fname, 0, MAXPATHLEN);*/
        char *fname = NULL;
        uint16_t limit_filename_size = 24;

        switch(idx_menus[menuItem]){
        case MENU_TYPE_VOLUME:
            draw_progress_bar(draw_screen, x_volume_bar, y_volume_bar,
                            width_progress_bar, height_progress_bar, volume_percentage, 100/STEP_CHANGE_VOLUME);
            break;

        case MENU_TYPE_BRIGHTNESS:
            draw_progress_bar(draw_screen, x_volume_bar, y_volume_bar,
                            width_progress_bar, height_progress_bar, brightness_percentage, 100/STEP_CHANGE_BRIGHTNESS);
            break;

        case MENU_TYPE_SAVE:
            /// ---- Write slot -----
            sprintf(text_tmp, "IN SLOT   < %d >", state_slot+1);
            text_surface = TTF_RenderText_Blended(menu_info_font, text_tmp, text_color);
            text_pos.x = (draw_screen->w - MENU_ZONE_WIDTH)/2 + (MENU_ZONE_WIDTH - text_surface->w)/2;
            text_pos.y = draw_screen->h - MENU_ZONE_HEIGHT/2 - text_surface->h/2;
            SDL_BlitSurface(text_surface, NULL, draw_screen, &text_pos);

            if(menu_action){
                sprintf(text_tmp, "Saving...");
                text_surface = TTF_RenderText_Blended(menu_info_font, text_tmp, text_color);
            }
            else{
                if(menu_confirmation){
                    sprintf(text_tmp, "Are you sure ?");
                    text_surface = TTF_RenderText_Blended(menu_info_font, text_tmp, text_color);
                }
                else{
		    fname = emu_get_save_fname(1, 0, state_slot, NULL);
		    if (fname == NULL) {
		        text_surface = TTF_RenderText_Blended(menu_info_font, "Free", text_color);
		    }
		    else{
		        printf("Found saved file: %s\n", fname);
			char *p = strrchr (fname, '/');
			char *basename = p ? p + 1 : (char *) fname;
			if(strlen(basename) > limit_filename_size){basename[limit_filename_size]=0;} //limiting size
			text_surface = TTF_RenderText_Blended(menu_small_info_font, basename, text_color);
		    }
                }
            }
            text_pos.x = (draw_screen->w - MENU_ZONE_WIDTH)/2 + (MENU_ZONE_WIDTH - text_surface->w)/2;
            text_pos.y = draw_screen->h - MENU_ZONE_HEIGHT/2 - text_surface->h/2 + 2*padding_y_from_center_menu_zone;
            SDL_BlitSurface(text_surface, NULL, draw_screen, &text_pos);
            break;

        case MENU_TYPE_LOAD:
            /// ---- Write slot -----
            if(quick_load_slot_chosen){
                sprintf(text_tmp, "FROM AUTO SAVE");
            }
            else{
                sprintf(text_tmp, "FROM SLOT   < %d >", state_slot+1);
            }
            text_surface = TTF_RenderText_Blended(menu_info_font, text_tmp, text_color);
            text_pos.x = (draw_screen->w - MENU_ZONE_WIDTH)/2 + (MENU_ZONE_WIDTH - text_surface->w)/2;
            text_pos.y = draw_screen->h - MENU_ZONE_HEIGHT/2 - text_surface->h/2;
            SDL_BlitSurface(text_surface, NULL, draw_screen, &text_pos);

            if(menu_action){
                sprintf(text_tmp, "Loading...");
                text_surface = TTF_RenderText_Blended(menu_info_font, text_tmp, text_color);
            }
            else{
                if(menu_confirmation){
                    sprintf(text_tmp, "Are you sure ?");
                    text_surface = TTF_RenderText_Blended(menu_info_font, text_tmp, text_color);
                }
                else{
                    if(quick_load_slot_chosen){
                        text_surface = TTF_RenderText_Blended(menu_info_font, " ", text_color);
                    }
                    else{
			fname = emu_get_save_fname(1, 0, state_slot, NULL);
			if (fname == NULL) {
			    text_surface = TTF_RenderText_Blended(menu_info_font, "Free", text_color);
			}
			else{
			    printf("Found saved file: %s\n", fname);
                            char *p = strrchr (fname, '/');
                            char *basename = p ? p + 1 : (char *) fname;
                            if(strlen(basename) > limit_filename_size){basename[limit_filename_size]=0;} //limiting size
                            text_surface = TTF_RenderText_Blended(menu_small_info_font, basename, text_color);
			}
                    }
                }
            }
            text_pos.x = (draw_screen->w - MENU_ZONE_WIDTH)/2 + (MENU_ZONE_WIDTH - text_surface->w)/2;
            text_pos.y = draw_screen->h - MENU_ZONE_HEIGHT/2 - text_surface->h/2 + 2*padding_y_from_center_menu_zone;
            SDL_BlitSurface(text_surface, NULL, draw_screen, &text_pos);
            break;

        case MENU_TYPE_ASPECT_RATIO:
            sprintf(text_tmp, "<   %s   >", aspect_ratio_name[aspect_ratio]);
            text_surface = TTF_RenderText_Blended(menu_info_font, text_tmp, text_color);
            text_pos.x = (draw_screen->w - MENU_ZONE_WIDTH)/2 + (MENU_ZONE_WIDTH - text_surface->w)/2;
            text_pos.y = draw_screen->h - MENU_ZONE_HEIGHT/2 - text_surface->h/2 + padding_y_from_center_menu_zone;
            SDL_BlitSurface(text_surface, NULL, draw_screen, &text_pos);
            break;

        case MENU_TYPE_EXIT:
        case MENU_TYPE_POWERDOWN:
            if(menu_confirmation){
                sprintf(text_tmp, "Are you sure ?");
                text_surface = TTF_RenderText_Blended(menu_info_font, text_tmp, text_color);
                text_pos.x = (draw_screen->w - MENU_ZONE_WIDTH)/2 + (MENU_ZONE_WIDTH - text_surface->w)/2;
                text_pos.y = draw_screen->h - MENU_ZONE_HEIGHT/2 - text_surface->h/2 + 2*padding_y_from_center_menu_zone;
                SDL_BlitSurface(text_surface, NULL, draw_screen, &text_pos);
            }
            break;
        default:
            break;
        }

        /// ------ Free Surfaces -------
        if(text_surface)
             SDL_FreeSurface(text_surface);
    }

    /// --------- Print arrows --------
    if(print_arrows){
        /// Top arrow
        SDL_Rect pos_arrow_top;
        pos_arrow_top.x = (draw_screen->w - img_arrow_top->w)/2;
        pos_arrow_top.y = (draw_screen->h - MENU_BG_SQUARE_HEIGHT)/4 - img_arrow_top->h/2;
        SDL_BlitSurface(img_arrow_top, NULL, draw_screen, &pos_arrow_top);

        /// Bottom arrow
        SDL_Rect pos_arrow_bottom;
        pos_arrow_bottom.x = (draw_screen->w - img_arrow_bottom->w)/2;
        pos_arrow_bottom.y = draw_screen->h -
            (draw_screen->h - MENU_BG_SQUARE_HEIGHT)/4 - img_arrow_bottom->h/2;
        SDL_BlitSurface(img_arrow_bottom, NULL, draw_screen, &pos_arrow_bottom);
    }

    // Rotate
    //SDL_Rotate_270(hw_screen, virtual_hw_screen);
    SDL_BlitSurface(draw_screen, NULL, hw_screen, NULL);

    /// --- Real Flip ---
    SDL_Flip(hw_screen);
}


void run_menu_loop()
{
    MENU_DEBUG_PRINTF("Launch Menu\n");

    SDL_Event event;
    uint32_t prev_ms = SDL_GetTicks();
    uint32_t cur_ms = SDL_GetTicks();
    int scroll=0;
    int start_scroll=0;
    uint8_t screen_refresh = 1;
    char shell_cmd[100];
    uint8_t menu_confirmation = 0;
    stop_menu_loop = 0;
    char fname[MAXPATHLEN];

    /// ------ Load default keymap ------
    system(SHELL_CMD_KEYMAP_DEFAULT);

    /// ------ Get init values -------
    init_menu_system_values();
    int prevItem=menuItem;

    /// ------ Copy currently displayed screen -------
   /* if(SDL_BlitSurface(virtual_hw_screen, NULL, backup_hw_screen, NULL)){
        MENU_ERROR_PRINTF("ERROR Could not copy draw_screen: %s\n", SDL_GetError());
    }*/

    memcpy(backup_hw_screen->pixels, (uint16_t*) virtual_hw_screen->pixels,
            RES_HW_SCREEN_HORIZONTAL * RES_HW_SCREEN_VERTICAL * sizeof(uint16_t));

    /* Stop Ampli */
    system(SHELL_CMD_AUDIO_AMP_OFF);

    /// ------ Wait for menu UP key event ------
    int actions[IN_BINDTYPE_COUNT] = { 0, };
    while(event.type != SDL_KEYUP || event.key.keysym.sym != SDLK_q){
        while (SDL_PollEvent(&event)){
            SDL_PushEvent(&event);
            in_update(actions);
        }

        /* 500ms timeout */
        if(SDL_GetTicks() - cur_ms > 500){
            MENU_ERROR_PRINTF("Timeout waiting for SDLK_q UP\n");
            break;
        }
    }

    /// -------- Main loop ---------
    while (!stop_menu_loop)
    {
        /// -------- Handle Keyboard Events ---------
        if(!scroll){
            while (SDL_PollEvent(&event))
            switch(event.type)
            {
                case SDL_QUIT:
                    engineState = PGS_Quit;
                    stop_menu_loop = 1;
		    break;
	    case SDL_KEYDOWN:
                switch (event.key.keysym.sym)
                {
                    case SDLK_b:
                        if(menu_confirmation){
                            /// ------ Reset menu confirmation ------
                            menu_confirmation = 0;
                            /// ------ Refresh screen ------
                            screen_refresh = 1;
                        }
                        /*else{
                            stop_menu_loop = 1;
                        }*/
                        break;

                    case SDLK_q:
                    case SDLK_ESCAPE:
                        stop_menu_loop = 1;
                        break;

                    case SDLK_d:
                    case SDLK_DOWN:
                        MENU_DEBUG_PRINTF("DOWN\n");
                        /// ------ Start scrolling to new menu -------
                        menuItem++;
                        if (menuItem>=nb_menu_zones) menuItem=0;
                        scroll=1;

                        /// ------ Reset menu confirmation ------
                        menu_confirmation = 0;

                        /// ------ Refresh screen ------
                        screen_refresh = 1;
                        break;

                    case SDLK_u:
                    case SDLK_UP:
                        MENU_DEBUG_PRINTF("UP\n");
                        /// ------ Start scrolling to new menu -------
                        menuItem--;
                        if (menuItem<0) menuItem=nb_menu_zones-1;
                        scroll=-1;

                        /// ------ Reset menu confirmation ------
                        menu_confirmation = 0;

                        /// ------ Refresh screen ------
                        screen_refresh = 1;
                        break;

                    case SDLK_l:
                    case SDLK_LEFT:
                        //MENU_DEBUG_PRINTF("LEFT\n");
                        if(idx_menus[menuItem] == MENU_TYPE_VOLUME){
                            MENU_DEBUG_PRINTF("Volume DOWN\n");
                            /// ----- Compute new value -----
                            volume_percentage = (volume_percentage < STEP_CHANGE_VOLUME)?
                                                    0:(volume_percentage-STEP_CHANGE_VOLUME);

                            /// ----- Shell cmd ----
                            sprintf(shell_cmd, "%s %d", SHELL_CMD_VOLUME_SET, volume_percentage);
                            system(shell_cmd);

                            /// ------ Refresh screen ------
                            screen_refresh = 1;
                        }
                        else if(idx_menus[menuItem] == MENU_TYPE_BRIGHTNESS){
                            MENU_DEBUG_PRINTF("Brightness DOWN\n");
                            /// ----- Compute new value -----
                            brightness_percentage = (brightness_percentage < STEP_CHANGE_BRIGHTNESS)?
                                                    0:(brightness_percentage-STEP_CHANGE_BRIGHTNESS);

                            /// ----- Shell cmd ----
                            sprintf(shell_cmd, "%s %d", SHELL_CMD_BRIGHTNESS_SET, brightness_percentage);
                            system(shell_cmd);

			    /// ------ Refresh screen ------
                            screen_refresh = 1;
                        }
                        else if(idx_menus[menuItem] == MENU_TYPE_SAVE){
                            MENU_DEBUG_PRINTF("Save Slot DOWN\n");
                            state_slot = (!state_slot)?(MAX_SAVE_SLOTS-1):(state_slot-1);
                            /// ------ Refresh screen ------
                            screen_refresh = 1;
                        }
                        else if(idx_menus[menuItem] == MENU_TYPE_LOAD){
                            MENU_DEBUG_PRINTF("Load Slot DOWN\n");

                            /** Choose quick save file or standard saveslot for loading */
                            if(!quick_load_slot_chosen &&
                                state_slot == 0 &&
                                access(quick_save_file, F_OK ) != -1){
                                quick_load_slot_chosen = 1;
                            }
                            else if(quick_load_slot_chosen){
                                quick_load_slot_chosen = 0;
                                state_slot = MAX_SAVE_SLOTS-1;
                            }
                            else{
                                state_slot = (!state_slot)?(MAX_SAVE_SLOTS-1):(state_slot-1);
                            }

                            /// ------ Refresh screen ------
                            screen_refresh = 1;
                        }
                        else if(idx_menus[menuItem] == MENU_TYPE_ASPECT_RATIO){
                            MENU_DEBUG_PRINTF("Aspect Ratio DOWN\n");
                            aspect_ratio = (!aspect_ratio)?(NB_ASPECT_RATIOS_TYPES-1):(aspect_ratio-1);
                            
                            /// ------ Refresh screen ------
                            screen_refresh = 1;

                            // Save config file
                            configfile_save(cfg_file_rom);
                        }
                        break;

                    case SDLK_r:
                    case SDLK_RIGHT:
                        //MENU_DEBUG_PRINTF("RIGHT\n");
                        if(idx_menus[menuItem] == MENU_TYPE_VOLUME){
                            MENU_DEBUG_PRINTF("Volume UP\n");
                            /// ----- Compute new value -----
                            volume_percentage = (volume_percentage > 100 - STEP_CHANGE_VOLUME)?
                                                    100:(volume_percentage+STEP_CHANGE_VOLUME);

                            /// ----- Shell cmd ----
                            sprintf(shell_cmd, "%s %d", SHELL_CMD_VOLUME_SET, volume_percentage);
                            system(shell_cmd);

                            /// ------ Refresh screen ------
                            screen_refresh = 1;
                        }
                        else if(idx_menus[menuItem] == MENU_TYPE_BRIGHTNESS){
                            MENU_DEBUG_PRINTF("Brightness UP\n");
                            /// ----- Compute new value -----
                            brightness_percentage = (brightness_percentage > 100 - STEP_CHANGE_BRIGHTNESS)?
                                                    100:(brightness_percentage+STEP_CHANGE_BRIGHTNESS);

                            /// ----- Shell cmd ----
                            sprintf(shell_cmd, "%s %d", SHELL_CMD_BRIGHTNESS_SET, brightness_percentage);
                            system(shell_cmd);

                            /// ------ Refresh screen ------
                            screen_refresh = 1;
                        }
                        else if(idx_menus[menuItem] == MENU_TYPE_SAVE){
                            MENU_DEBUG_PRINTF("Save Slot UP\n");
                            state_slot = (state_slot+1)%MAX_SAVE_SLOTS;
                            /// ------ Refresh screen ------
                            screen_refresh = 1;
                        }
                        else if(idx_menus[menuItem] == MENU_TYPE_LOAD){
                            MENU_DEBUG_PRINTF("Load Slot UP\n");

                            /** Choose quick save file or standard saveslot for loading */
                            if(!quick_load_slot_chosen &&
                                state_slot == MAX_SAVE_SLOTS-1 &&
                                access(quick_save_file, F_OK ) != -1){
                                quick_load_slot_chosen = 1;
                            }
                            else if(quick_load_slot_chosen){
                                quick_load_slot_chosen = 0;
                                state_slot = 0;
                            }
                            else{
                                state_slot = (state_slot+1)%MAX_SAVE_SLOTS;
                            }

                            /// ------ Refresh screen ------
                            screen_refresh = 1;
                        }
                        else if(idx_menus[menuItem] == MENU_TYPE_ASPECT_RATIO){
                            MENU_DEBUG_PRINTF("Aspect Ratio UP\n");
                            aspect_ratio = (aspect_ratio+1)%NB_ASPECT_RATIOS_TYPES;
                            
                            /// ------ Refresh screen ------
                            screen_refresh = 1;

                            // Save config file
                            configfile_save(cfg_file_rom);
                        }
                        break;

                    case SDLK_a:
                    case SDLK_RETURN:
                        if(idx_menus[menuItem] == MENU_TYPE_SAVE){
                            if(menu_confirmation){
                                MENU_DEBUG_PRINTF("Saving in slot %d\n", state_slot);
                                /// ------ Refresh Screen -------
                                menu_screen_refresh(menuItem, prevItem, scroll, menu_confirmation, 1);

                                /// ------ Save game ------
                                int ret = emu_save_load_game(0, 0);

                                /// ----- Hud Msg -----
                                if(ret){
                                    MENU_ERROR_PRINTF("Save Failed\n");
                                    sprintf(shell_cmd, "%s %d \"          SAVE FAILED\"",
                                        SHELL_CMD_NOTIF, NOTIF_SECONDS_DISP);
                                }
                                else{
                                    sprintf(shell_cmd, "%s %d \"        SAVED IN SLOT %d\"",
                                        SHELL_CMD_NOTIF, NOTIF_SECONDS_DISP, state_slot+1);
                                }
                                system(shell_cmd);
                                stop_menu_loop = 1;
                            }
                            else{
                                MENU_DEBUG_PRINTF("Save game - asking confirmation\n");
                                menu_confirmation = 1;
                                /// ------ Refresh screen ------
                                screen_refresh = 1;
                            }
                        }
                        else if(idx_menus[menuItem] == MENU_TYPE_LOAD){
                            if(menu_confirmation){
                                MENU_DEBUG_PRINTF("Loading in slot %d\n", state_slot);
                                /// ------ Refresh Screen -------
                                menu_screen_refresh(menuItem, prevItem, scroll, menu_confirmation, 1);

                                /// ------ Load game ------
                                int ret;
                                if(quick_load_slot_chosen){
                                    ret = emu_save_load_game_from_file(1, quick_save_file);
                                }
                                else{
                                    ret = emu_save_load_game(1, 0);
                                }

                                /// ----- Hud Msg -----
                                if(ret){
                                    MENU_ERROR_PRINTF("Load Failed\n");
                                    sprintf(shell_cmd, "%s %d \"          LOAD FAILED\"",
                                        SHELL_CMD_NOTIF, NOTIF_SECONDS_DISP);
                                }
                                else{
                                    if(quick_load_slot_chosen){
                                        sprintf(shell_cmd, "%s %d \"     LOADED FROM AUTO SAVE\"",
                                            SHELL_CMD_NOTIF, NOTIF_SECONDS_DISP);
                                    }
                                    else{
                                        sprintf(shell_cmd, "%s %d \"      LOADED FROM SLOT %d\"",
                                            SHELL_CMD_NOTIF, NOTIF_SECONDS_DISP, state_slot+1);
                                    }
                                }
                                system(shell_cmd);
                                /*snprintf(hud_msg, sizeof(hud_msg), ret == 0 ? "LOADED" : "FAIL!");
                                hud_new_msg = 3;*/
                                stop_menu_loop = 1;
                            }
                            else{
                                MENU_DEBUG_PRINTF("Save game - asking confirmation\n");
                                menu_confirmation = 1;
                                /// ------ Refresh screen ------
                                screen_refresh = 1;
                            }
                        }
                        else if(idx_menus[menuItem] == MENU_TYPE_EXIT){
                            MENU_DEBUG_PRINTF("Exit game\n");
                            if(menu_confirmation){
                                MENU_DEBUG_PRINTF("Exit game - confirmed\n");

                                /// ----- The game should be saved here ----
                                int ret = emu_save_load_game_from_file(0, quick_save_file);

                                /// ----- Exit game and back to launcher ----
                                engineState = PGS_Quit;
                                stop_menu_loop = 1;
                            }
                            else{
                                MENU_DEBUG_PRINTF("Exit game - asking confirmation\n");
                                menu_confirmation = 1;
                                /// ------ Refresh screen ------
                                screen_refresh = 1;
                            }
                        }
                        else if(idx_menus[menuItem] == MENU_TYPE_POWERDOWN){
                            if(menu_confirmation){
                                MENU_DEBUG_PRINTF("Powerdown - confirmed\n");
                                /// ----- Shell cmd ----
                                execlp(SHELL_CMD_SHUTDOWN_FUNKEY, SHELL_CMD_SHUTDOWN_FUNKEY, NULL);
                                MENU_ERROR_PRINTF("Failed to run command %s\n", SHELL_CMD_SHUTDOWN_FUNKEY);
                                exit(0);
                            }
                            else{
                                MENU_DEBUG_PRINTF("Powerdown - asking confirmation\n");
                                menu_confirmation = 1;
                                /// ------ Refresh screen ------
                                screen_refresh = 1;
                            }
                        }
                        break;

                    default:
                        //MENU_DEBUG_PRINTF("Keydown: %d\n", event.key.keysym.sym);
                        break;
                }
                break;
            }
        }

        /// --------- Handle Scroll effect ---------
        if ((scroll>0) || (start_scroll>0)){
            scroll+=MIN(SCROLL_SPEED_PX, MENU_ZONE_HEIGHT-scroll);
            start_scroll = 0;
            screen_refresh = 1;
        }
        else if ((scroll<0) || (start_scroll<0)){
            scroll-=MIN(SCROLL_SPEED_PX, MENU_ZONE_HEIGHT+scroll);
            start_scroll = 0;
            screen_refresh = 1;
        }
        if (scroll>=MENU_ZONE_HEIGHT || scroll<=-MENU_ZONE_HEIGHT) {
            prevItem=menuItem;
            scroll=0;
            screen_refresh = 1;
        }

        /// --------- Handle FPS ---------
        cur_ms = SDL_GetTicks();
        if(cur_ms-prev_ms < 1000/FPS_MENU){
            SDL_Delay(1000/FPS_MENU - (cur_ms-prev_ms));
        }
        prev_ms = SDL_GetTicks();


        /// --------- Refresh screen
        if(screen_refresh){
            menu_screen_refresh(menuItem, prevItem, scroll, menu_confirmation, 0);
        }

        /// --------- reset screen refresh ---------
        screen_refresh = 0;
    }

    /// ------ Restore last keymap ------
    system(SHELL_CMD_KEYMAP_RESUME);

    /// ------ Reset prev key repeat params -------
    if(SDL_EnableKeyRepeat(backup_key_repeat_delay, backup_key_repeat_interval)){
        MENU_ERROR_PRINTF("ERROR with SDL_EnableKeyRepeat: %s\n", SDL_GetError());
    }

    /* Start Ampli */
    system(SHELL_CMD_AUDIO_AMP_ON);

    /// ------ Reset last screen ------
    SDL_BlitSurface(backup_hw_screen, NULL, hw_screen, NULL);
    SDL_Flip(hw_screen);
}






/****************************/
/*    Quick Resume Menu     */
/****************************/
int launch_resume_menu_loop()
{
    MENU_DEBUG_PRINTF("Init resume menu\n");

    /* Decare vars */
    SDL_Surface *text_surface = NULL;
    char text_tmp[40];
    SDL_Rect text_pos;
    SDL_Event event;
    uint32_t prev_ms = SDL_GetTicks();
    uint32_t cur_ms = SDL_GetTicks();
    stop_menu_loop = 0;
    uint8_t screen_refresh = 1;
    uint8_t menu_confirmation = 0;
    int option_idx=RESUME_YES;

    /* Stop Ampli */
    system(SHELL_CMD_AUDIO_AMP_OFF);

    /* Save prev key repeat params and set new Key repeat */
    SDL_GetKeyRepeat(&backup_key_repeat_delay, &backup_key_repeat_interval);
    if(SDL_EnableKeyRepeat(SDL_DEFAULT_REPEAT_DELAY, SDL_DEFAULT_REPEAT_INTERVAL)){
        MENU_ERROR_PRINTF("ERROR with SDL_EnableKeyRepeat: %s\n", SDL_GetError());
    }

    /* Load BG */
    SDL_Surface *img_square_bg = IMG_Load(MENU_PNG_BG_PATH);
    if(!img_square_bg) {
        MENU_ERROR_PRINTF("ERROR IMG_Load: %s\n", IMG_GetError());
    }
    SDL_Surface *bg_surface = SDL_CreateRGBSurface(SDL_SWSURFACE, hw_screen->w, hw_screen->h, 16, 0, 0, 0, 0);
    SDL_BlitSurface(img_square_bg, NULL, bg_surface, NULL);
    SDL_FreeSurface(img_square_bg);


    /*  Print top arrow */
    SDL_Rect pos_arrow_top;
    pos_arrow_top.x = (bg_surface->w - img_arrow_top->w)/2;
    pos_arrow_top.y = (bg_surface->h - MENU_BG_SQUARE_HEIGHT)/4 - img_arrow_top->h/2;
    SDL_BlitSurface(img_arrow_top, NULL, bg_surface, &pos_arrow_top);

    /*  Print bottom arrow */
    SDL_Rect pos_arrow_bottom;
    pos_arrow_bottom.x = (bg_surface->w - img_arrow_bottom->w)/2;
    pos_arrow_bottom.y = bg_surface->h -
            (bg_surface->h - MENU_BG_SQUARE_HEIGHT)/4 - img_arrow_bottom->h/2;
    SDL_BlitSurface(img_arrow_bottom, NULL, bg_surface, &pos_arrow_bottom);

    if (text_surface)
        SDL_FreeSurface(text_surface);

    /* Main loop */
    while (!stop_menu_loop)
    {
        /* Handle keyboard events */
        while (SDL_PollEvent(&event))
        switch(event.type)
        {
            case SDL_QUIT:
                engineState = PGS_Quit;
                stop_menu_loop = 1;
                break;
            case SDL_KEYDOWN:
                switch (event.key.keysym.sym)
                {
                    case SDLK_b:
                        if(menu_confirmation){
                            /// ------ Reset menu confirmation ------
                            menu_confirmation = 0;

                            /// ------ Refresh screen ------
                            screen_refresh = 1;
                        }
                        /*else{
                            stop_menu_loop = 1;
                        }*/
                        break;

                    case SDLK_q:
                    case SDLK_ESCAPE:
                        /*stop_menu_loop = 1;*/
                        break;

                    case SDLK_u:
                    case SDLK_UP:
                        MENU_DEBUG_PRINTF("Option UP\n");
                        option_idx = (!option_idx)?(NB_RESUME_OPTIONS-1):(option_idx-1);

                        /// ------ Reset menu confirmation ------
                        menu_confirmation = 0;

                        /// ------ Refresh screen ------
                        screen_refresh = 1;
                        break;

                    case SDLK_d:
                    case SDLK_DOWN:
                        MENU_DEBUG_PRINTF("Option DWON\n");
                        option_idx = (option_idx+1)%NB_RESUME_OPTIONS;

                        /// ------ Reset menu confirmation ------
                        menu_confirmation = 0;

                        /// ------ Refresh screen ------
                        screen_refresh = 1;
                        break;

                    case SDLK_a:
                    case SDLK_RETURN:
                        MENU_DEBUG_PRINTF("Pressed A\n");
                        if(menu_confirmation){
                            MENU_DEBUG_PRINTF("Confirmed\n");

                            /// ----- exit menu  ----
                            stop_menu_loop = 1;
                        }
                        else{
                            MENU_DEBUG_PRINTF("Asking confirmation\n");
                            menu_confirmation = 1;

                            /// ------ Refresh screen ------
                            screen_refresh = 1;
                        }
                        break;

                    default:
                        //MENU_DEBUG_PRINTF("Keydown: %d\n", event.key.keysym.sym);
                        break;
            }
            break;
        }

        /* Handle FPS */
        cur_ms = SDL_GetTicks();
        if(cur_ms-prev_ms < 1000/FPS_MENU){
            SDL_Delay(1000/FPS_MENU - (cur_ms-prev_ms));
        }
        prev_ms = SDL_GetTicks();

        /* Refresh screen */
        if(screen_refresh){
            /* Clear and draw BG */
            SDL_FillRect(hw_screen, NULL, 0);
            if(SDL_BlitSurface(bg_surface, NULL, hw_screen, NULL)){
                MENU_ERROR_PRINTF("ERROR Could not draw background: %s\n", SDL_GetError());
            }

            /* Draw resume or reset option */
            text_surface = TTF_RenderText_Blended(menu_title_font, resume_options_str[option_idx], text_color);
            text_pos.x = (hw_screen->w - MENU_ZONE_WIDTH)/2 + (MENU_ZONE_WIDTH - text_surface->w)/2;
            text_pos.y = hw_screen->h - MENU_ZONE_HEIGHT/2 - text_surface->h/2;
            SDL_BlitSurface(text_surface, NULL, hw_screen, &text_pos);

            /* Draw confirmation */
            if(menu_confirmation){
                sprintf(text_tmp, "Are you sure ?");
                text_surface = TTF_RenderText_Blended(menu_info_font, text_tmp, text_color);
                text_pos.x = (hw_screen->w - MENU_ZONE_WIDTH)/2 + (MENU_ZONE_WIDTH - text_surface->w)/2;
                text_pos.y = hw_screen->h - MENU_ZONE_HEIGHT/2 - text_surface->h/2 + 2*padding_y_from_center_menu_zone;
                SDL_BlitSurface(text_surface, NULL, hw_screen, &text_pos);
            }

            /* Flip Screen */
            SDL_Flip(hw_screen);
        }

        /* reset screen refresh */
        screen_refresh = 0;
    }

    /// ----- Clear screen -----
    SDL_FillRect(hw_screen, NULL, 0);
    SDL_Flip(hw_screen);

    /* Free SDL Surfaces */
    if(bg_surface)
        SDL_FreeSurface(bg_surface);
    if(text_surface)
        SDL_FreeSurface(text_surface);

    /* Reset prev key repeat params */
    if(SDL_EnableKeyRepeat(backup_key_repeat_delay, backup_key_repeat_interval)){
        MENU_ERROR_PRINTF("ERROR with SDL_EnableKeyRepeat: %s\n", SDL_GetError());
    }

    /* Start Ampli */
    system(SHELL_CMD_AUDIO_AMP_ON);

    return option_idx;
}










































































static void make_bg(int no_scale)
{
	unsigned short *src = (void *)g_menubg_src_ptr;
	int w = g_screen_width, h = g_screen_height;
	int pp = g_screen_ppitch;
	short *dst;
	int x, y;

	if (src == NULL) {
		memset(g_menubg_ptr, 0, g_menuscreen_w * g_menuscreen_h * 2);
		return;
	}

	if (!no_scale && g_menuscreen_w / w >= 2 && g_menuscreen_h / h >= 2)
	{
		unsigned int t, *d = g_menubg_ptr;
		d += (g_menuscreen_h / 2 - h * 2 / 2)
			* g_menuscreen_w / 2;
		d += (g_menuscreen_w / 2 - w * 2 / 2) / 2;
		for (y = 0; y < h; y++, src += pp, d += g_menuscreen_w*2/2) {
			for (x = 0; x < w; x++) {
				t = src[x];
				t = ((t & 0xf79e)>>1) - ((t & 0xc618)>>3);
				t |= t << 16;
				d[x] = d[x + g_menuscreen_w / 2] = t;
			}
		}
		return;
	}

	if (w > g_menuscreen_w)
		w = g_menuscreen_w;
	if (h > g_menuscreen_h)
		h = g_menuscreen_h;
	dst = (short *)g_menubg_ptr +
		(g_menuscreen_h / 2 - h / 2) * g_menuscreen_w +
		(g_menuscreen_w / 2 - w / 2);

	// darken the active framebuffer
	for (; h > 0; dst += g_menuscreen_w, src += pp, h--)
		menu_darken_bg(dst, src, w, 1);
}

static void menu_enter(int is_rom_loaded)
{
	if (is_rom_loaded)
	{
		make_bg(0);
	}
	else
	{
		int pos;
		char buff[256];
		pos = plat_get_skin_dir(buff, 256);
		strcpy(buff + pos, "background.png");

		// should really only happen once, on startup..
		if (readpng(g_menubg_ptr, buff, READPNG_BG,
						g_menuscreen_w, g_menuscreen_h) < 0)
			memset(g_menubg_ptr, 0, g_menuscreen_w * g_menuscreen_h * 2);
	}

	plat_video_menu_enter(is_rom_loaded);
}

static void draw_savestate_bg(int slot)
{
	const char *fname;
	void *tmp_state;

	fname = emu_get_save_fname(1, 0, slot, NULL);
	if (!fname)
		return;

	tmp_state = PicoTmpStateSave();

	PicoStateLoadGfx(fname);

	/* do a frame and fetch menu bg */
	pemu_forced_frame(0, 0);

	make_bg(0);

	PicoTmpStateRestore(tmp_state);
}

// --------- loading ROM screen ----------

static int cdload_called = 0;

static void load_progress_cb(int percent)
{
	int ln, len = percent * g_menuscreen_w / 100;
	unsigned short *dst;

	if (len > g_menuscreen_w)
		len = g_menuscreen_w;

	menu_draw_begin(0, 1);
	dst = (unsigned short *)g_menuscreen_ptr + g_menuscreen_pp * me_sfont_h * 2;
	for (ln = me_sfont_h - 2; ln > 0; ln--, dst += g_menuscreen_pp)
		memset(dst, 0xff, len * 2);
	menu_draw_end();
}

static void cdload_progress_cb(const char *fname, int percent)
{
	int ln, len = percent * g_menuscreen_w / 100;
	unsigned short *dst;

	menu_draw_begin(0, 1);
	dst = (unsigned short *)g_menuscreen_ptr + g_menuscreen_pp * me_sfont_h * 2;

	menuscreen_memset_lines(dst, 0xff, me_sfont_h - 2);

	smalltext_out16(1, 3 * me_sfont_h, "Processing CD image / MP3s", 0xffff);
	smalltext_out16(1, 4 * me_sfont_h, fname, 0xffff);
	dst += g_menuscreen_pp * me_sfont_h * 3;

	if (len > g_menuscreen_w)
		len = g_menuscreen_w;

	for (ln = (me_sfont_h - 2); ln > 0; ln--, dst += g_menuscreen_pp)
		memset(dst, 0xff, len * 2);
	menu_draw_end();

	cdload_called = 1;
}

void menu_romload_prepare(const char *rom_name)
{
#ifndef ALLOW_TEXT_OUT
    return;
#endif //ALLOW_TEXT_OUT

	const char *p = rom_name + strlen(rom_name);

	while (p > rom_name && *p != '/')
		p--;

	/* fill all buffers, callbacks won't update in full */
	for (int i = 0; i < 3; i++) {
		menu_draw_begin(1, 1);
		smalltext_out16(1, 1, "Loading", 0xffff);
		smalltext_out16(1, me_sfont_h, p, 0xffff);
		menu_draw_end();
	}

	PicoCartLoadProgressCB = load_progress_cb;
	PicoCDLoadProgressCB = cdload_progress_cb;
	cdload_called = 0;
}

void menu_romload_end(void)
{
#ifndef ALLOW_TEXT_OUT
    return;
#endif //ALLOW_TEXT_OUT

	PicoCartLoadProgressCB = NULL;
	PicoCDLoadProgressCB = NULL;

	for (int i = 0; i < 3; i++) {
		menu_draw_begin(0, 1);
		smalltext_out16(1, (cdload_called ? 6 : 3) * me_sfont_h,
			"Starting emulation...", 0xffff);
		menu_draw_end();
	}
}

// ------------ patch/gg menu ------------

static void draw_patchlist(int sel)
{
	int max_cnt, start, i, pos, active;

	max_cnt = g_menuscreen_h / me_sfont_h;
	start = max_cnt / 2 - sel;

	menu_draw_begin(1, 0);

	for (i = 0; i < PicoPatchCount; i++) {
		pos = start + i;
		if (pos < 0) continue;
		if (pos >= max_cnt) break;
		active = PicoPatches[i].active;
		smalltext_out16(14,                pos * me_sfont_h, active ? "ON " : "OFF", active ? 0xfff6 : 0xffff);
		smalltext_out16(14 + me_sfont_w*4, pos * me_sfont_h, PicoPatches[i].name,    active ? 0xfff6 : 0xffff);
	}
	pos = start + i;
	if (pos < max_cnt)
		smalltext_out16(14, pos * me_sfont_h, "done", 0xffff);

	text_out16(5, max_cnt / 2 * me_sfont_h, ">");
	menu_draw_end();
}

static void menu_loop_patches(void)
{
	static int menu_sel = 0;
	int inp;

	for (;;)
	{
		draw_patchlist(menu_sel);
		inp = in_menu_wait(PBTN_UP|PBTN_DOWN|PBTN_LEFT|PBTN_RIGHT|PBTN_L|PBTN_R
				|PBTN_MOK|PBTN_MBACK, NULL, 33);
		if (inp & PBTN_UP  ) { menu_sel--; if (menu_sel < 0) menu_sel = PicoPatchCount; }
		if (inp & PBTN_DOWN) { menu_sel++; if (menu_sel > PicoPatchCount) menu_sel = 0; }
		if (inp &(PBTN_LEFT|PBTN_L))  { menu_sel-=10; if (menu_sel < 0) menu_sel = 0; }
		if (inp &(PBTN_RIGHT|PBTN_R)) { menu_sel+=10; if (menu_sel > PicoPatchCount) menu_sel = PicoPatchCount; }
		if (inp & PBTN_MOK) { // action
			if (menu_sel < PicoPatchCount)
				PicoPatches[menu_sel].active = !PicoPatches[menu_sel].active;
			else 	break;
		}
		if (inp & PBTN_MBACK)
			break;
	}
}

// -------------- key config --------------

// PicoIn.pad[] format: MXYZ SACB RLDU
me_bind_action me_ctrl_actions[] =
{
	{ "UP     ", 0x0001 },
	{ "DOWN   ", 0x0002 },
	{ "LEFT   ", 0x0004 },
	{ "RIGHT  ", 0x0008 },
	{ "A      ", 0x0040 },
	{ "B      ", 0x0010 },
	{ "C      ", 0x0020 },
	{ "A turbo", 0x4000 },
	{ "B turbo", 0x1000 },
	{ "C turbo", 0x2000 },
	{ "START  ", 0x0080 },
	{ "MODE   ", 0x0800 },
	{ "X      ", 0x0400 },
	{ "Y      ", 0x0200 },
	{ "Z      ", 0x0100 },
	{ NULL,      0 },
};

me_bind_action emuctrl_actions[] =
{
	{ "Load State       ", PEV_STATE_LOAD },
	{ "Save State       ", PEV_STATE_SAVE },
	{ "Prev Save Slot   ", PEV_SSLOT_PREV },
	{ "Next Save Slot   ", PEV_SSLOT_NEXT },
	{ "Switch Renderer  ", PEV_SWITCH_RND },
	{ "Volume Down      ", PEV_VOL_DOWN },
	{ "Volume Up        ", PEV_VOL_UP },
	{ "Fast forward     ", PEV_FF },
	{ "Reset Game       ", PEV_RESET },
	{ "Enter Menu       ", PEV_MENU },
	{ "Pico Next page   ", PEV_PICO_PNEXT },
	{ "Pico Prev page   ", PEV_PICO_PPREV },
	{ "Pico Switch input", PEV_PICO_SWINP },
	{ NULL,                0 }
};

static int key_config_loop_wrap(int id, int keys)
{
	switch (id) {
		case MA_CTRL_PLAYER1:
			key_config_loop(me_ctrl_actions, array_size(me_ctrl_actions) - 1, 0);
			break;
		case MA_CTRL_PLAYER2:
			key_config_loop(me_ctrl_actions, array_size(me_ctrl_actions) - 1, 1);
			break;
		case MA_CTRL_EMU:
			key_config_loop(emuctrl_actions, array_size(emuctrl_actions) - 1, -1);
			break;
		default:
			break;
	}
	return 0;
}

static const char *mgn_dev_name(int id, int *offs)
{
	const char *name = NULL;
	static int it = 0;

	if (id == MA_CTRL_DEV_FIRST)
		it = 0;

	for (; it < IN_MAX_DEVS; it++) {
		name = in_get_dev_name(it, 1, 1);
		if (name != NULL)
			break;
	}

	it++;
	return name;
}

static int mh_saveloadcfg(int id, int keys);
static const char *mgn_saveloadcfg(int id, int *offs);

const char *indev_names[] = { "none", "3 button pad", "6 button pad", NULL };

static menu_entry e_menu_keyconfig[] =
{
	mee_handler_id("Player 1",          MA_CTRL_PLAYER1,    key_config_loop_wrap),
	mee_handler_id("Player 2",          MA_CTRL_PLAYER2,    key_config_loop_wrap),
	mee_handler_id("Emulator controls", MA_CTRL_EMU,        key_config_loop_wrap),
	mee_enum      ("Input device 1",    MA_OPT_INPUT_DEV0,  currentConfig.input_dev0, indev_names),
	mee_enum      ("Input device 2",    MA_OPT_INPUT_DEV1,  currentConfig.input_dev1, indev_names),
	mee_range     ("Turbo rate",        MA_CTRL_TURBO_RATE, currentConfig.turbo_rate, 1, 30),
	mee_range     ("Analog deadzone",   MA_CTRL_DEADZONE,   currentConfig.analog_deadzone, 1, 99),
	mee_cust_nosave("Save global config",       MA_OPT_SAVECFG, mh_saveloadcfg, mgn_saveloadcfg),
	mee_cust_nosave("Save cfg for loaded game", MA_OPT_SAVECFG_GAME, mh_saveloadcfg, mgn_saveloadcfg),
	mee_label     (""),
	mee_label     ("Input devices:"),
	mee_label_mk  (MA_CTRL_DEV_FIRST, mgn_dev_name),
	mee_label_mk  (MA_CTRL_DEV_NEXT,  mgn_dev_name),
	mee_label_mk  (MA_CTRL_DEV_NEXT,  mgn_dev_name),
	mee_label_mk  (MA_CTRL_DEV_NEXT,  mgn_dev_name),
	mee_label_mk  (MA_CTRL_DEV_NEXT,  mgn_dev_name),
	mee_label_mk  (MA_CTRL_DEV_NEXT,  mgn_dev_name),
	mee_label_mk  (MA_CTRL_DEV_NEXT,  mgn_dev_name),
	mee_end,
};

static int menu_loop_keyconfig(int id, int keys)
{
	static int sel = 0;

	me_enable(e_menu_keyconfig, MA_OPT_SAVECFG_GAME, PicoGameLoaded);
	me_loop(e_menu_keyconfig, &sel);

	PicoSetInputDevice(0, currentConfig.input_dev0);
	PicoSetInputDevice(1, currentConfig.input_dev1);

	return 0;
}

// ------------ SCD options menu ------------

static const char h_cdleds[] = "Show power/CD LEDs of emulated console";
static const char h_cdda[]   = "Play audio tracks from mp3s/wavs/bins";
static const char h_cdpcm[]  = "Emulate PCM audio chip for effects/voices/music";
static const char h_srcart[] = "Emulate the save RAM cartridge accessory\n"
				"most games don't need this";
static const char h_scfx[]   = "Emulate scale/rotate ASIC chip for graphics effects\n"
				"disable to improve performance";

static menu_entry e_menu_cd_options[] =
{
	mee_onoff_h("CD LEDs",              MA_CDOPT_LEDS,          currentConfig.EmuOpt, EOPT_EN_CD_LEDS, h_cdleds),
	mee_onoff_h("CDDA audio",           MA_CDOPT_CDDA,          PicoIn.opt, POPT_EN_MCD_CDDA, h_cdda),
	mee_onoff_h("PCM audio",            MA_CDOPT_PCM,           PicoIn.opt, POPT_EN_MCD_PCM, h_cdpcm),
	mee_onoff_h("SaveRAM cart",         MA_CDOPT_SAVERAM,       PicoIn.opt, POPT_EN_MCD_RAMCART, h_srcart),
	mee_onoff_h("Scale/Rot. fx",        MA_CDOPT_SCALEROT_CHIP, PicoIn.opt, POPT_EN_MCD_GFX, h_scfx),
	mee_end,
};

static int menu_loop_cd_options(int id, int keys)
{
	static int sel = 0;
	me_loop(e_menu_cd_options, &sel);
	return 0;
}

// ------------ 32X options menu ------------

#ifndef NO_32X

// convert from multiplier of VClk
static int mh_opt_sh2cycles(int id, int keys)
{
	int *khz = (id == MA_32XOPT_MSH2_CYCLES) ?
		&currentConfig.msh2_khz : &currentConfig.ssh2_khz;

	if (keys & (PBTN_LEFT|PBTN_RIGHT))
		*khz += (keys & PBTN_LEFT) ? -50 : 50;
	if (keys & (PBTN_L|PBTN_R))
		*khz += (keys & PBTN_L) ? -500 : 500;

	if (*khz < 1)
		*khz = 1;
	else if (*khz > 0x7fffffff / 1000)
		*khz = 0x7fffffff / 1000;

	return 0;
}

static const char *mgn_opt_sh2cycles(int id, int *offs)
{
	int khz = (id == MA_32XOPT_MSH2_CYCLES) ?
		currentConfig.msh2_khz : currentConfig.ssh2_khz;

	sprintf(static_buff, "%d", khz);
	return static_buff;
}

static const char h_32x_enable[] = "Enable emulation of the 32X addon";
static const char h_pwm[]        = "Disabling may improve performance, but break sound";
static const char h_sh2cycles[]  = "Cycles/millisecond (similar to DOSBox)\n"
				   "lower values speed up emulation but break games\n"
				   "at least 11000 recommended for compatibility";

static menu_entry e_menu_32x_options[] =
{
	mee_onoff_h   ("32X enabled",       MA_32XOPT_ENABLE_32X,  PicoIn.opt, POPT_EN_32X, h_32x_enable),
	mee_enum      ("32X renderer",      MA_32XOPT_RENDERER,    currentConfig.renderer32x, renderer_names32x),
	mee_onoff_h   ("PWM sound",         MA_32XOPT_PWM,         PicoIn.opt, POPT_EN_PWM, h_pwm),
	mee_cust_h    ("Master SH2 cycles", MA_32XOPT_MSH2_CYCLES, mh_opt_sh2cycles, mgn_opt_sh2cycles, h_sh2cycles),
	mee_cust_h    ("Slave SH2 cycles",  MA_32XOPT_SSH2_CYCLES, mh_opt_sh2cycles, mgn_opt_sh2cycles, h_sh2cycles),
	mee_end,
};

static int menu_loop_32x_options(int id, int keys)
{
	static int sel = 0;

	me_enable(e_menu_32x_options, MA_32XOPT_RENDERER, renderer_names32x[0] != NULL);
	me_loop(e_menu_32x_options, &sel);

	Pico32xSetClocks(currentConfig.msh2_khz * 1000, currentConfig.msh2_khz * 1000);

	return 0;
}

#endif

// ------------ adv options menu ------------

static const char h_ovrclk[] = "Will break some games, keep at 0";

static menu_entry e_menu_adv_options[] =
{
	mee_onoff     ("SRAM/BRAM saves",          MA_OPT_SRAM_STATES,    currentConfig.EmuOpt, EOPT_EN_SRAM),
	mee_onoff     ("Disable sprite limit",     MA_OPT2_NO_SPRITE_LIM, PicoIn.opt, POPT_DIS_SPRITE_LIM),
	mee_range_h   ("Overclock M68k (%%)",       MA_OPT2_OVERCLOCK_M68K,currentConfig.overclock_68k, 0, 1000, h_ovrclk),
	mee_onoff     ("Emulate Z80",              MA_OPT2_ENABLE_Z80,    PicoIn.opt, POPT_EN_Z80),
	mee_onoff     ("Emulate YM2612 (FM)",      MA_OPT2_ENABLE_YM2612, PicoIn.opt, POPT_EN_FM),
	mee_onoff     ("Emulate SN76496 (PSG)",    MA_OPT2_ENABLE_SN76496,PicoIn.opt, POPT_EN_PSG),
	mee_onoff     ("gzip savestates",          MA_OPT2_GZIP_STATES,   currentConfig.EmuOpt, EOPT_GZIP_SAVES),
	mee_onoff     ("Don't save last used ROM", MA_OPT2_NO_LAST_ROM,   currentConfig.EmuOpt, EOPT_NO_AUTOSVCFG),
	mee_onoff     ("Disable idle loop patching",MA_OPT2_NO_IDLE_LOOPS,PicoIn.opt, POPT_DIS_IDLE_DET),
	mee_onoff     ("Disable frame limiter",    MA_OPT2_NO_FRAME_LIMIT,currentConfig.EmuOpt, EOPT_NO_FRMLIMIT),
	mee_onoff     ("Enable dynarecs",          MA_OPT2_DYNARECS,      PicoIn.opt, POPT_EN_DRC),
	mee_onoff     ("Status line in main menu", MA_OPT2_STATUS_LINE,   currentConfig.EmuOpt, EOPT_SHOW_RTC),
	MENU_OPTIONS_ADV
	mee_end,
};

static int menu_loop_adv_options(int id, int keys)
{
	static int sel = 0;

	me_loop(e_menu_adv_options, &sel);
	PicoIn.overclockM68k = currentConfig.overclock_68k; // int vs short

	return 0;
}

// ------------ gfx options menu ------------

static const char h_gamma[] = "Gamma/brightness adjustment (default 1.00)";

static const char *mgn_aopt_gamma(int id, int *offs)
{
	sprintf(static_buff, "%i.%02i", currentConfig.gamma / 100, currentConfig.gamma % 100);
	return static_buff;
}

static menu_entry e_menu_gfx_options[] =
{
	mee_enum   ("Video output mode", MA_OPT_VOUT_MODE, plat_target.vout_method, men_dummy),
	mee_enum   ("Renderer",          MA_OPT_RENDERER, currentConfig.renderer, renderer_names),
	mee_enum   ("Filter",            MA_OPT3_FILTERING, currentConfig.filter, men_dummy),
	mee_range_cust_h("Gamma correction", MA_OPT2_GAMMA, currentConfig.gamma, 1, 300, mgn_aopt_gamma, h_gamma),
	MENU_OPTIONS_GFX
	mee_end,
};

static int menu_loop_gfx_options(int id, int keys)
{
	static int sel = 0;

	me_enable(e_menu_gfx_options, MA_OPT_RENDERER, renderer_names[0] != NULL);
	me_loop(e_menu_gfx_options, &sel);

	return 0;
}

// ------------ options menu ------------

static menu_entry e_menu_options[];

static int sndrate_prevnext(int rate, int dir)
{
	static const int rates[] = { 8000, 11025, 16000, 22050, 44100 };
	int i;

	for (i = 0; i < 5; i++)
		if (rates[i] == rate) break;

	i += dir ? 1 : -1;
	if (i > 4) {
		if (!(PicoIn.opt & POPT_EN_STEREO)) {
			PicoIn.opt |= POPT_EN_STEREO;
			return rates[0];
		}
		return rates[4];
	}
	if (i < 0) {
		if (PicoIn.opt & POPT_EN_STEREO) {
			PicoIn.opt &= ~POPT_EN_STEREO;
			return rates[4];
		}
		return rates[0];
	}
	return rates[i];
}

static void region_prevnext(int right)
{
	// jp_ntsc=1, jp_pal=2, usa=4, eu=8
	static const int rgn_orders[] = { 0x148, 0x184, 0x814, 0x418, 0x841, 0x481 };
	int i;

	if (right) {
		if (!PicoIn.regionOverride) {
			for (i = 0; i < 6; i++)
				if (rgn_orders[i] == PicoIn.autoRgnOrder) break;
			if (i < 5) PicoIn.autoRgnOrder = rgn_orders[i+1];
			else PicoIn.regionOverride=1;
		}
		else
			PicoIn.regionOverride <<= 1;
		if (PicoIn.regionOverride > 8)
			PicoIn.regionOverride = 8;
	} else {
		if (!PicoIn.regionOverride) {
			for (i = 0; i < 6; i++)
				if (rgn_orders[i] == PicoIn.autoRgnOrder) break;
			if (i > 0) PicoIn.autoRgnOrder = rgn_orders[i-1];
		}
		else
			PicoIn.regionOverride >>= 1;
	}
}

static int mh_opt_misc(int id, int keys)
{
	switch (id) {
	case MA_OPT_SOUND_QUALITY:
		PicoIn.sndRate = sndrate_prevnext(PicoIn.sndRate, keys & PBTN_RIGHT);
		break;
	case MA_OPT_REGION:
		region_prevnext(keys & PBTN_RIGHT);
		break;
	default:
		break;
	}
	return 0;
}

static int mh_saveloadcfg(int id, int keys)
{
	int ret;

	if (keys & (PBTN_LEFT|PBTN_RIGHT)) { // multi choice
		config_slot += (keys & PBTN_LEFT) ? -1 : 1;
		if (config_slot < 0) config_slot = 9;
		else if (config_slot > 9) config_slot = 0;
		me_enable(e_menu_options, MA_OPT_LOADCFG, config_slot != config_slot_current);
		return 0;
	}

	switch (id) {
	case MA_OPT_SAVECFG:
	case MA_OPT_SAVECFG_GAME:
		if (emu_write_config(id == MA_OPT_SAVECFG_GAME ? 1 : 0))
			menu_update_msg("config saved");
		else
			menu_update_msg("failed to write config");
		break;
	case MA_OPT_LOADCFG:
		ret = emu_read_config(rom_fname_loaded, 1);
		if (!ret) ret = emu_read_config(NULL, 1);
		if (ret)  menu_update_msg("config loaded");
		else      menu_update_msg("failed to load config");
		break;
	default:
		return 0;
	}

	return 1;
}

static int mh_restore_defaults(int id, int keys)
{
	emu_set_defconfig();
	menu_update_msg("defaults restored");
	return 1;
}

static const char *mgn_opt_fskip(int id, int *offs)
{
	if (currentConfig.Frameskip < 0)
		return "Auto";
	sprintf(static_buff, "%d", currentConfig.Frameskip);
	return static_buff;
}

static const char *mgn_opt_sound(int id, int *offs)
{
	const char *str2;
	*offs = -8;
	str2 = (PicoIn.opt & POPT_EN_STEREO) ? "stereo" : "mono";
	sprintf(static_buff, "%5iHz %s", PicoIn.sndRate, str2);
	return static_buff;
}

static const char *mgn_opt_region(int id, int *offs)
{
	static const char *names[] = { "Auto", "      Japan NTSC", "      Japan PAL", "      USA", "      Europe" };
	static const char *names_short[] = { "", " JP", " JP", " US", " EU" };
	int code = PicoIn.regionOverride;
	int u, i = 0;

	*offs = -6;
	if (code) {
		code <<= 1;
		while ((code >>= 1)) i++;
		if (i > 4)
			return "unknown";
		return names[i];
	} else {
		strcpy(static_buff, "Auto:");
		for (u = 0; u < 3; u++) {
			code = (PicoIn.autoRgnOrder >> u*4) & 0xf;
			for (i = 0; code; code >>= 1, i++)
				;
			strcat(static_buff, names_short[i]);
		}
		return static_buff;
	}
}

static const char *mgn_saveloadcfg(int id, int *offs)
{
	static_buff[0] = 0;
	if (config_slot != 0)
		sprintf(static_buff, "[%i]", config_slot);
	return static_buff;
}

static const char *men_confirm_save[] = { "OFF", "writes", "loads", "both", NULL };
static const char h_confirm_save[]    = "Ask for confirmation when overwriting save,\n"
					"loading state or both";

static menu_entry e_menu_options[] =
{
	// mee_range     ("Save slot",                MA_OPT_SAVE_SLOT,     state_slot, 0, 9),
	mee_range_cust("Frameskip",                MA_OPT_FRAMESKIP,     currentConfig.Frameskip, -1, 16, mgn_opt_fskip),
	mee_onoff     ("Show FPS",                 MA_OPT_SHOW_FPS,      currentConfig.EmuOpt, EOPT_SHOW_FPS),
	mee_cust      ("Region",                   MA_OPT_REGION,        mh_opt_misc, mgn_opt_region),
	mee_onoff     ("Enable sound",             MA_OPT_ENABLE_SOUND,  currentConfig.EmuOpt, EOPT_EN_SOUND),
	mee_cust      ("Sound Quality",            MA_OPT_SOUND_QUALITY, mh_opt_misc, mgn_opt_sound),
	mee_enum_h    ("Confirm savestate",        MA_OPT_CONFIRM_STATES,currentConfig.confirm_save, men_confirm_save, h_confirm_save),
	mee_range     ("",                         MA_OPT_CPU_CLOCKS,    currentConfig.CPUclock, 20, 3200),
	mee_handler   ("[Controls]",                      menu_loop_keyconfig),
	mee_handler   ("[Display]",        menu_loop_gfx_options),
	mee_handler   ("[Sega/Mega CD]",   menu_loop_cd_options),
#ifndef NO_32X
	mee_handler   ("[32X]",            menu_loop_32x_options),
#endif
	mee_handler   ("[Advanced options]",       menu_loop_adv_options),
	mee_cust_nosave("Save global config",      MA_OPT_SAVECFG, mh_saveloadcfg, mgn_saveloadcfg),
	mee_cust_nosave("Save cfg for loaded game",MA_OPT_SAVECFG_GAME, mh_saveloadcfg, mgn_saveloadcfg),
	mee_cust_nosave("Load cfg from profile",   MA_OPT_LOADCFG, mh_saveloadcfg, mgn_saveloadcfg),
	mee_handler   ("Restore defaults",         mh_restore_defaults),
	mee_handler_id("Credits",            MA_MAIN_CREDITS,     main_menu_handler),
	mee_end,
};

static int menu_loop_options(int id, int keys)
{
	static int sel = 0;

	me_enable(e_menu_options, MA_OPT_SAVECFG_GAME, PicoGameLoaded);
	me_enable(e_menu_options, MA_OPT_LOADCFG, config_slot != config_slot_current);

	me_loop(e_menu_options, &sel);

	return 0;
}

// ------------ debug menu ------------

#include <pico/debug.h>

extern void SekStepM68k(void);

static void mplayer_loop(void)
{
	pemu_sound_start();

	while (1)
	{
		PDebugZ80Frame();
		if (in_menu_wait_any(NULL, 0) & PBTN_MA3)
			break;
		emu_sound_wait();
	}

	emu_sound_stop();
}

static void draw_text_debug(const char *str, int skip, int from)
{
	const char *p;
	int line;

	p = str;
	while (skip-- > 0)
	{
		while (*p && *p != '\n')
			p++;
		if (*p == 0 || p[1] == 0)
			return;
		p++;
	}

	str = p;
	for (line = from; line < g_menuscreen_h / me_sfont_h; line++)
	{
		smalltext_out16(1, line * me_sfont_h, str, 0xffff);
		while (*p && *p != '\n')
			p++;
		if (*p == 0)
			break;
		p++; str = p;
	}
}

#ifdef __GNUC__
#define COMPILER "gcc " __VERSION__
#else
#define COMPILER
#endif

static void draw_frame_debug(void)
{
	char layer_str[48] = "layers:                   ";
	struct PicoVideo *pv = &Pico.video;

	if (!(pv->debug_p & PVD_KILL_B))    memcpy(layer_str +  8, "B", 1);
	if (!(pv->debug_p & PVD_KILL_A))    memcpy(layer_str + 10, "A", 1);
	if (!(pv->debug_p & PVD_KILL_S_LO)) memcpy(layer_str + 12, "spr_lo", 6);
	if (!(pv->debug_p & PVD_KILL_S_HI)) memcpy(layer_str + 19, "spr_hi", 6);
	if (!(pv->debug_p & PVD_KILL_32X))  memcpy(layer_str + 26, "32x", 4);

	pemu_forced_frame(1, 0);
	make_bg(1);

	smalltext_out16(4, 1, "build: r" REVISION "  "__DATE__ " " __TIME__ " " COMPILER, 0xffff);
	smalltext_out16(4, g_menuscreen_h - me_sfont_h, layer_str, 0xffff);
}

static void debug_menu_loop(void)
{
	struct PicoVideo *pv = &Pico.video;
	int inp, mode = 0;
	int spr_offs = 0, dumped = 0;
	char *tmp;

	while (1)
	{
		menu_draw_begin(1, 0);
		switch (mode)
		{
			case 0: tmp = PDebugMain();
				plat_debug_cat(tmp);
				draw_text_debug(tmp, 0, 0);
				if (dumped) {
					smalltext_out16(g_menuscreen_w - 6 * me_sfont_h,
						g_menuscreen_h - me_mfont_h, "dumped", 0xffff);
					dumped = 0;
				}
				break;
			case 1: draw_frame_debug();
				break;
			case 2: pemu_forced_frame(1, 0);
				make_bg(1);
				PDebugShowSpriteStats((unsigned short *)g_menuscreen_ptr
					+ (g_menuscreen_h/2 - 240/2) * g_menuscreen_pp
					+ g_menuscreen_w/2 - 320/2, g_menuscreen_pp);
				break;
			case 3: menuscreen_memset_lines(g_menuscreen_ptr, 0, g_menuscreen_h);
				PDebugShowPalette(g_menuscreen_ptr, g_menuscreen_pp);
				PDebugShowSprite((unsigned short *)g_menuscreen_ptr
					+ g_menuscreen_pp * 120 + g_menuscreen_w / 2 + 16,
					g_menuscreen_pp, spr_offs);
				draw_text_debug(PDebugSpriteList(), spr_offs, 6);
				break;
			case 4: tmp = PDebug32x();
				draw_text_debug(tmp, 0, 0);
				break;
		}
		menu_draw_end();

		inp = in_menu_wait(PBTN_MOK|PBTN_MBACK|PBTN_MA2|PBTN_MA3|PBTN_L|PBTN_R |
					PBTN_UP|PBTN_DOWN|PBTN_LEFT|PBTN_RIGHT, NULL, 70);
		if (inp & PBTN_MBACK) return;
		if (inp & PBTN_L) { mode--; if (mode < 0) mode = 4; }
		if (inp & PBTN_R) { mode++; if (mode > 4) mode = 0; }
		switch (mode)
		{
			case 0:
				if (inp & PBTN_MOK)
					PDebugCPUStep();
				if (inp & PBTN_MA3) {
					while (inp & PBTN_MA3)
						inp = in_menu_wait_any(NULL, -1);
					mplayer_loop();
				}
				if ((inp & (PBTN_MA2|PBTN_LEFT)) == (PBTN_MA2|PBTN_LEFT)) {
					mkdir("dumps", 0777);
					PDebugDumpMem();
					while (inp & PBTN_MA2) inp = in_menu_wait_any(NULL, -1);
					dumped = 1;
				}
				break;
			case 1:
				if (inp & PBTN_LEFT)  pv->debug_p ^= PVD_KILL_B;
				if (inp & PBTN_RIGHT) pv->debug_p ^= PVD_KILL_A;
				if (inp & PBTN_DOWN)  pv->debug_p ^= PVD_KILL_S_LO;
				if (inp & PBTN_UP)    pv->debug_p ^= PVD_KILL_S_HI;
				if (inp & PBTN_MA2)   pv->debug_p ^= PVD_KILL_32X;
				if (inp & PBTN_MOK) {
					PicoIn.sndOut = NULL; // just in case
					PicoIn.skipFrame = 1;
					PicoFrame();
					PicoIn.skipFrame = 0;
					while (inp & PBTN_MOK) inp = in_menu_wait_any(NULL, -1);
				}
				break;
			case 3:
				if (inp & PBTN_DOWN)  spr_offs++;
				if (inp & PBTN_UP)    spr_offs--;
				if (spr_offs < 0) spr_offs = 0;
				break;
		}
	}
}

// ------------ main menu ------------

static void draw_frame_credits(void)
{
	smalltext_out16(4, 1, "build: " __DATE__ " " __TIME__, 0xe7fc);
}

static const char credits[] =
	"PicoDrive v" VERSION " (c) notaz, 2006-2013\n\n\n"
	"Credits:\n"
	"fDave: initial code\n"
#ifdef EMU_C68K
	"      Cyclone 68000 core\n"
#else
	"Stef, Chui: FAME/C 68k core\n"
#endif
#ifdef _USE_DRZ80
	"Reesy & FluBBa: DrZ80 core\n"
#else
	"Stef, NJ: CZ80 core\n"
#endif
	"MAME devs: SH2, YM2612 and SN76496 cores\n"
	"Eke, Stef: some Sega CD code\n"
	"Inder, ketchupgun: graphics\n"
#ifdef __GP2X__
	"Squidge: mmuhack\n"
	"Dzz: ARM940 sample\n"
#endif
	"\n"
	"special thanks (for docs, ideas):\n"
	" Charles MacDonald, Haze,\n"
	" Stephane Dallongeville,\n"
	" Lordus, Exophase, Rokas,\n"
	" Eke, Nemesis, Tasco Deluxe";

static void menu_main_draw_status(void)
{
	static time_t last_bat_read = 0;
	static int last_bat_val = -1;
	unsigned short *bp = g_screen_ptr;
	int bat_h = me_mfont_h * 2 / 3;
	int i, u, w, wfill, batt_val;
	struct tm *tmp;
	time_t ltime;
	char time_s[16];

	if (!(currentConfig.EmuOpt & EOPT_SHOW_RTC))
		return;

	ltime = time(NULL);
	tmp = gmtime(&ltime);
	strftime(time_s, sizeof(time_s), "%H:%M", tmp);

	text_out16(g_screen_width - me_mfont_w * 6, me_mfont_h + 2, time_s);

	if (ltime - last_bat_read > 10) {
		last_bat_read = ltime;
		last_bat_val = batt_val = plat_target_bat_capacity_get();
	}
	else
		batt_val = last_bat_val;

	if (batt_val < 0 || batt_val > 100)
		return;

	/* battery info */
	bp += (me_mfont_h * 2 + 2) * g_screen_ppitch + g_screen_width - me_mfont_w * 3 - 3;
	for (i = 0; i < me_mfont_w * 2; i++)
		bp[i] = menu_text_color;
	for (i = 0; i < me_mfont_w * 2; i++)
		bp[i + g_screen_ppitch * bat_h] = menu_text_color;
	for (i = 0; i <= bat_h; i++)
		bp[i * g_screen_ppitch] =
		bp[i * g_screen_ppitch + me_mfont_w * 2] = menu_text_color;
	for (i = 2; i < bat_h - 1; i++)
		bp[i * g_screen_ppitch - 1] =
		bp[i * g_screen_ppitch - 2] = menu_text_color;

	w = me_mfont_w * 2 - 1;
	wfill = batt_val * w / 100;
	for (u = 1; u < bat_h; u++)
		for (i = 0; i < wfill; i++)
			bp[(w - i) + g_screen_ppitch * u] = menu_text_color;
}

int main_menu_handler(int id, int keys)
{
	const char *ret_name;

	switch (id)
	{
	case MA_MAIN_RESUME_GAME:
		if (PicoGameLoaded)
			return 1;
		break;
	case MA_MAIN_SAVE_STATE:
		if (PicoGameLoaded)
			return menu_loop_savestate(0);
		break;
	case MA_MAIN_LOAD_STATE:
		if (PicoGameLoaded)
			return menu_loop_savestate(1);
		break;
	case MA_MAIN_RESET_GAME:
		if (PicoGameLoaded) {
			emu_reset_game();
			return 1;
		}
		break;
	case MA_MAIN_LOAD_ROM:
		rom_fname_reload = NULL;
		ret_name = menu_loop_romsel(rom_fname_loaded,
			sizeof(rom_fname_loaded), rom_exts, NULL);
		if (ret_name != NULL) {
			lprintf("selected file: %s\n", ret_name);
			rom_fname_reload = ret_name;
			engineState = PGS_ReloadRom;
			return 1;
		}
		break;
	case MA_MAIN_CHANGE_CD:
		if (PicoIn.AHW & PAHW_MCD) {
			// if cd is loaded, cdd_unload() triggers eject and
			// returns 1, else we'll select and load new CD here
			if (!cdd_unload())
				menu_loop_tray();
			return 1;
		}
		break;
	case MA_MAIN_CREDITS:
		draw_menu_message(credits, draw_frame_credits);
		in_menu_wait(PBTN_MOK|PBTN_MBACK, NULL, 70);
		break;
	case MA_MAIN_EXIT:
		engineState = PGS_Quit;
		return 1;
	case MA_MAIN_PATCHES:
		if (PicoGameLoaded && PicoPatches) {
			menu_loop_patches();
			PicoPatchApply();
			menu_update_msg("Patches applied");
		}
		break;
	default:
		lprintf("%s: something unknown selected\n", __FUNCTION__);
		break;
	}

	return 0;
}

static menu_entry e_menu_main[] =
{
	mee_label     ("PicoDrive " VERSION),
	mee_label     (""),
	mee_label     (""),
	mee_label     (""),
	// mee_handler_id("Resume game",        MA_MAIN_RESUME_GAME, main_menu_handler),
	mee_handler_id("Load state",         MA_MAIN_LOAD_STATE,  main_menu_handler),
	mee_handler_id("Save state",         MA_MAIN_SAVE_STATE,  main_menu_handler),
	// mee_handler_id("Load new ROM/ISO",   MA_MAIN_LOAD_ROM,    main_menu_handler),
	mee_handler_id("Change CD/ISO",      MA_MAIN_CHANGE_CD,   main_menu_handler),
	// mee_handler   ("Configure controls",                      menu_loop_keyconfig),
	// mee_handler_id("Credits",            MA_MAIN_CREDITS,     main_menu_handler),
	mee_handler_id("Patches / GameGenie",MA_MAIN_PATCHES,     main_menu_handler),
	mee_handler   ("Settings",                                menu_loop_options),
	mee_handler_id("Reset",         MA_MAIN_RESET_GAME,  main_menu_handler),
	mee_handler_id("Exit",               MA_MAIN_EXIT,        main_menu_handler),
	mee_end,
};

void menu_loop(void)
{
	static int sel = 0;

	me_enable(e_menu_main, MA_MAIN_RESUME_GAME, PicoGameLoaded);
	me_enable(e_menu_main, MA_MAIN_SAVE_STATE,  PicoGameLoaded);
	me_enable(e_menu_main, MA_MAIN_LOAD_STATE,  PicoGameLoaded);
	me_enable(e_menu_main, MA_MAIN_RESET_GAME,  PicoGameLoaded);
	me_enable(e_menu_main, MA_MAIN_CHANGE_CD,   PicoIn.AHW & PAHW_MCD);
	me_enable(e_menu_main, MA_MAIN_PATCHES, PicoPatches != NULL);

	menu_enter(PicoGameLoaded);
	in_set_config_int(0, IN_CFG_BLOCKING, 1);
	me_loop_d(e_menu_main, &sel, NULL, menu_main_draw_status);

	if (PicoGameLoaded) {
		if (engineState == PGS_Menu)
			engineState = PGS_Running;
		/* wait until menu, ok, back is released */
		while (in_menu_wait_any(NULL, 50) & (PBTN_MENU|PBTN_MOK|PBTN_MBACK))
			;
	}

	in_set_config_int(0, IN_CFG_BLOCKING, 0);
	plat_video_menu_leave();
}

void menu_loop_funkey(void)
{
	in_set_config_int(0, IN_CFG_BLOCKING, 1);
	run_menu_loop();

	if (PicoGameLoaded) {
		if (engineState == PGS_Menu)
			engineState = PGS_Running;
		/* wait until menu, ok, back is released */
		/*while (in_menu_wait_any(NULL, 50) & (PBTN_MENU|PBTN_MOK|PBTN_MBACK))
			;*/
	}

	in_set_config_int(0, IN_CFG_BLOCKING, 0);
	plat_video_menu_leave();
}

// --------- CD tray close menu ----------

static int mh_tray_load_cd(int id, int keys)
{
	const char *ret_name;

	rom_fname_reload = NULL;
	ret_name = menu_loop_romsel(rom_fname_loaded,
			sizeof(rom_fname_loaded), rom_exts, NULL);
	if (ret_name == NULL)
		return 0;

	rom_fname_reload = ret_name;
	engineState = PGS_RestartRun;
	return emu_swap_cd(ret_name);
}

static int mh_tray_nothing(int id, int keys)
{
	return 1;
}

static menu_entry e_menu_tray[] =
{
	mee_label  ("The CD tray has opened."),
	mee_label  (""),
	mee_label  (""),
	mee_handler("Load CD image",  mh_tray_load_cd),
	mee_handler("Insert nothing", mh_tray_nothing),
	mee_end,
};

int menu_loop_tray(void)
{
	int ret = 1, sel = 0;

	menu_enter(PicoGameLoaded);

	in_set_config_int(0, IN_CFG_BLOCKING, 1);
	me_loop(e_menu_tray, &sel);

	if (engineState != PGS_RestartRun) {
		engineState = PGS_RestartRun;
		ret = 0; /* no CD inserted */
	}

	while (in_menu_wait_any(NULL, 50) & (PBTN_MENU|PBTN_MOK|PBTN_MBACK))
		;
	in_set_config_int(0, IN_CFG_BLOCKING, 0);
	plat_video_menu_leave();

	return ret;
}

void menu_update_msg(const char *msg)
{
	strncpy(menu_error_msg, msg, sizeof(menu_error_msg));
	menu_error_msg[sizeof(menu_error_msg) - 1] = 0;

	menu_error_time = plat_get_ticks_ms();
	lprintf("msg: %s\n", menu_error_msg);
}

// ------------ util ------------

/* hidden options for config engine only */
static menu_entry e_menu_hidden[] =
{
	mee_onoff("Accurate sprites", MA_OPT_ACC_SPRITES, PicoIn.opt, 0x080),
	mee_onoff("autoload savestates", MA_OPT_AUTOLOAD_SAVE, g_autostateld_opt, 1),
	mee_end,
};

static menu_entry *e_menu_table[] =
{
	e_menu_options,
	e_menu_gfx_options,
	e_menu_adv_options,
	e_menu_cd_options,
#ifndef NO_32X
	e_menu_32x_options,
#endif
	e_menu_keyconfig,
	e_menu_hidden,
};

static menu_entry *me_list_table = NULL;
static menu_entry *me_list_i = NULL;

menu_entry *me_list_get_first(void)
{
	me_list_table = me_list_i = e_menu_table[0];
	return me_list_i;
}

menu_entry *me_list_get_next(void)
{
	int i;

	me_list_i++;
	if (me_list_i->name != NULL)
		return me_list_i;

	for (i = 0; i < array_size(e_menu_table); i++)
		if (me_list_table == e_menu_table[i])
			break;

	if (i + 1 < array_size(e_menu_table))
		me_list_table = me_list_i = e_menu_table[i + 1];
	else
		me_list_table = me_list_i = NULL;

	return me_list_i;
}

void menu_init(void)
{
	int i;

	menu_init_base();

	i = 0;
#if defined(_SVP_DRC) || defined(DRC_SH2)
	i = 1;
#endif
	me_enable(e_menu_adv_options, MA_OPT2_DYNARECS, i);

	i = me_id2offset(e_menu_gfx_options, MA_OPT_VOUT_MODE);
	e_menu_gfx_options[i].data = plat_target.vout_methods;
	me_enable(e_menu_gfx_options, MA_OPT_VOUT_MODE,
		plat_target.vout_methods != NULL);

	i = me_id2offset(e_menu_gfx_options, MA_OPT3_FILTERING);
	e_menu_gfx_options[i].data = plat_target.hwfilters;
	me_enable(e_menu_gfx_options, MA_OPT3_FILTERING,
		plat_target.hwfilters != NULL);

	me_enable(e_menu_gfx_options, MA_OPT2_GAMMA,
                plat_target.gamma_set != NULL);

	i = me_id2offset(e_menu_options, MA_OPT_CPU_CLOCKS);
	e_menu_options[i].enabled = 0;
	if (plat_target.cpu_clock_set != NULL) {
		e_menu_options[i].name = "CPU clock";
		e_menu_options[i].enabled = 1;
	}
}
