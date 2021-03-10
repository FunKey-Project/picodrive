/*
 * PicoDrive
 * (C) notaz, 2006-2010
 *
 * This work is licensed under the terms of MAME license.
 * See COPYING file in the top-level directory.
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <strings.h>
#include <signal.h>

#include "../libpicofe/input.h"
#include "../libpicofe/plat.h"
#include "menu_pico.h"
#include "configfile.h"
#include "emu.h"
#include "version.h"
#include <cpu/debug.h>


char **g_argv;
char *prog_name;
static char *load_state_file = NULL;
static int load_state_slot = -1;
static char *quick_save_file_extension = "quicksave";
char *mRomName = NULL;
char *mRomPath = NULL;
char *quick_save_file = NULL;
char *cfg_file_default = NULL;
char *cfg_file_rom = NULL;
static char *cfg_file_default_name = "default_config";
static char *cfg_file_extension = "cfg";
int mQuickSaveAndPoweroff=0;


void usage(){
	printf("\n\n\nPicoDrive v" VERSION " (c) notaz, 2006-2009,2013\n");
	printf("usage: PicoDriveBin [options] [romfile]\n");
	printf("options:\n"
		" -config <file>    use specified config file instead of default 'config.cfg'\n"
		" -fps				use to show fps\n"
		" -loadStateSlot <num>  if ROM is specified, try loading savestate slot <num>\n"
		" -loadStateFile <filePath>  if ROM is specified, try loading savestate file <filePath>\n");
  exit(1);
}

/* Handler for SIGUSR1, caused by closing the console */
void handle_sigusr1(int sig)
{
    //printf("Caught signal USR1 %d\n", sig);

    /* Exit menu if it was launched */
    stop_menu_loop = 1;

    /* Signal to quick save and poweoff after next loop */
    mQuickSaveAndPoweroff = 1;
}

void parse_cmd_line(int argc, char *argv[])
{
	int x, unrecognized = 0;

	for (x = 1; x < argc; x++)
	{
		if (argv[x][0] == '-')
		{
			if (strcasecmp(argv[x], "-config") == 0) {
				if (x+1 < argc) { ++x; PicoConfigFile = argv[x]; }
			}
			else if (strcasecmp(argv[x], "-loadStateSlot") == 0
				 || strcasecmp(argv[x], "-load") == 0)
			{
				if (x+1 < argc) { ++x; load_state_slot = atoi(argv[x]); }
			}
			else if (strcasecmp(argv[x], "-loadStateFile") == 0)
			{
				if (x+1 < argc) { ++x; load_state_file = argv[x]; }
			}
			else if (strcasecmp(argv[x], "-fps") == 0) {
				currentConfig.EmuOpt |= EOPT_SHOW_FPS;
				show_fps_bypass = 1;
			}
			else if (strcasecmp(argv[x], "-pdb") == 0) {
				if (x+1 < argc) { ++x; pdb_command(argv[x]); }
			}
			else if (strcasecmp(argv[x], "-pdb_connect") == 0) {
				if (x+2 < argc) { pdb_net_connect(argv[x+1], argv[x+2]); x += 2; }
			}
			else {
				unrecognized = 1;
				break;
			}
		}
		/* Check if file exists, Save ROM name, and ROM path */
		else {
			mRomName = argv[x];
			FILE *f = fopen(argv[x], "rb");
			if (f) {
				/* Save Rom path */
		        mRomPath = (char *)malloc(strlen(mRomName)+1);
		        strcpy(mRomPath, mRomName);
		        char *slash = strrchr ((char*)mRomPath, '/');
		        *slash = 0;

		        /* Rom name without extension */
		        char *point = strrchr ((char*)slash+1, '.');
		        *point = 0;

		        /* Set quicksave filename */
		        quick_save_file = (char *)malloc(strlen(mRomPath) + strlen(slash+1) +
		          strlen(quick_save_file_extension) + 2 + 1);
		        sprintf(quick_save_file, "%s/%s.%s",
		          mRomPath, slash+1, quick_save_file_extension);
		        printf("Quick_save_file: %s\n", quick_save_file);
				
		        /* Set rom cfg filepath */
		        cfg_file_rom = (char *)malloc(strlen(mRomPath) + strlen(slash+1) +
		          strlen(cfg_file_extension) + 2 + 1);
		        sprintf(cfg_file_rom, "%s/%s.%s",
		          mRomPath, slash+1, cfg_file_extension);
		        printf("cfg_file_rom: %s\n", cfg_file_rom);

		        /* Set console cfg filepath */
		        cfg_file_default = (char *)malloc(strlen(mRomPath) + strlen(cfg_file_default_name) +
		          strlen(cfg_file_extension) + 2 + 1);
		        sprintf(cfg_file_default, "%s/%s.%s",
		          mRomPath, cfg_file_default_name, cfg_file_extension);
		        printf("cfg_file_default: %s\n", cfg_file_default);

		        /** Load config files */
		        configfile_load(cfg_file_default);
		        configfile_load(cfg_file_rom);

		        /* Close file*/
				fclose(f);
				rom_fname_reload = argv[x];
				engineState = PGS_ReloadRom;
			}
			else{
				printf("Rom %s not found \n", mRomName);
				unrecognized = 1;
			}
			break;
		}
	}

	if (unrecognized) {
		usage();
	}
}


int main(int argc, char *argv[])
{
	g_argv = argv;

	/* Save program name */
	prog_name = argv[0];

	/* Engine initial state */
	engineState = PGS_Menu;

	/* Parse arguments */
	if (argc > 1){
		parse_cmd_line(argc, argv);
	}
	else{
    		usage();
	}

	/* Init Signals */
	signal(SIGUSR1, handle_sigusr1);

    /* Set env var for no mouse */
    putenv(strdup("SDL_NOMOUSE=1"));

	plat_early_init();

	in_init();
	//in_probe();

	plat_target_init();
	plat_init();

	emu_prep_defconfig(); // depends on input
	emu_read_config(NULL, 0);

	emu_init();
	menu_init();

	if (engineState == PGS_ReloadRom)
	{
		if (emu_reload_rom(rom_fname_reload)) {
			engineState = PGS_Running;

			/* Load slot */
			if(load_state_slot != -1){
				printf("LOADING FROM SLOT %d...\n", load_state_slot+1);
				char fname[1024];
				emu_save_load_game(1, 0);
				printf("LOADED FROM SLOT %d\n", load_state_slot+1);
				load_state_slot = -1;
			}
			/* Load file */
			else if(load_state_file != NULL){
				printf("LOADING FROM FILE %s...\n", load_state_file);
				emu_save_load_game_from_file(1, load_state_file);
				printf("LOADED FROM SLOT %s\n", load_state_file);
				load_state_file = NULL;
			}
			/* Load quick save file */
			else if(access( quick_save_file, F_OK ) != -1){
				printf("Found quick save file: %s\n", quick_save_file);

				int resume = launch_resume_menu_loop();
				if(resume == RESUME_YES){
					printf("Resume game from quick save file: %s\n", quick_save_file);
					emu_save_load_game_from_file(1, quick_save_file);
				}
				else{
					printf("Reset game\n");

					/* Remove quicksave file if present */
					if (remove(quick_save_file) == 0){
						printf("Deleted successfully: %s\n", quick_save_file);
					}
					else{
						printf("Unable to delete the file: %s\n", quick_save_file);
					}
				}
			}
		}
	}

	for (;;)
	{
		switch (engineState)
		{
			case PGS_Menu:
				//menu_loop();
				menu_loop_funkey();
				break;

			case PGS_TrayMenu:
				menu_loop_tray();
				break;

			case PGS_ReloadRom:
				if (emu_reload_rom(rom_fname_reload))
					engineState = PGS_Running;
				else {
					printf("PGS_ReloadRom == 0\n");
					engineState = PGS_Menu;
				}
				break;

			case PGS_RestartRun:
				engineState = PGS_Running;
				/* vvv fallthrough */

			case PGS_Running:
				emu_loop();
				break;

			case PGS_Quit:
				goto endloop;

			default:
				printf("engine got into unknown state (%i), exitting\n", engineState);
				goto endloop;
		}
	}

	endloop:

	emu_finish();
	plat_finish();
	plat_target_finish();

	return 0;
}
