#ifndef CONFIGFILE_H
#define CONFIGFILE_H

#include <stdbool.h>


///------ Definition of the different aspect ratios
#define ASPECT_RATIOS \
    X(ASPECT_RATIOS_TYPE_MANUAL, "ZOOMED") \
    X(ASPECT_RATIOS_TYPE_STRETCHED, "STRETCHED") \
    X(ASPECT_RATIOS_TYPE_CROPPED, "CROPPED") \
    X(ASPECT_RATIOS_TYPE_SCALED, "SCALED") \
    X(NB_ASPECT_RATIOS_TYPES, "")

////------ Enumeration of the different aspect ratios ------
#undef X
#define X(a, b) a,
typedef enum {ASPECT_RATIOS} ENUM_ASPECT_RATIOS_TYPES;

extern unsigned int aspect_ratio;
extern unsigned int aspect_ratio_factor_percent;
extern const char *	aspect_ratio_name[];
extern unsigned int aspect_ratio_factor_step;

void configfile_load(const char *filename);
void configfile_save(const char *filename);

#endif
