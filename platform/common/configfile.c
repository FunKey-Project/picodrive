// configfile.c - handles loading and saving the configuration options
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <ctype.h>

#include "configfile.h"

#define ARRAY_LEN(arr) (sizeof(arr) / sizeof(arr[0]))

enum ConfigOptionType {
    CONFIG_TYPE_BOOL,
    CONFIG_TYPE_UINT,
    CONFIG_TYPE_FLOAT,
    CONFIG_TYPE_ASPECT_RATIO,
};

struct ConfigOption {
    const char *name;
    enum ConfigOptionType type;
    union {
        bool *boolValue;
        unsigned int *uintValue;
        float *floatValue;
    };
};

#undef X
#define X(a, b) b,
const char  *aspect_ratio_name[] = {ASPECT_RATIOS};
unsigned int aspect_ratio_factor_step = 10;

/*
 *Config options and default values
 */
unsigned int aspect_ratio                   = ASPECT_RATIOS_TYPE_STRETCHED;
unsigned int aspect_ratio_factor_percent    = 50;

static const struct ConfigOption options[] = {
    {.name = "aspect_ratio",                .type = CONFIG_TYPE_ASPECT_RATIO,    .uintValue = &aspect_ratio},
    {.name = "aspect_ratio_factor_percent", .type = CONFIG_TYPE_UINT,            .uintValue = &aspect_ratio_factor_percent},
};

// Reads an entire line from a file (excluding the newline character) and returns an allocated string
// Returns NULL if no lines could be read from the file
static char *read_file_line(FILE *file) {
    char *buffer;
    size_t bufferSize = 64;
    size_t offset = 0; // offset in buffer to write

    buffer = (char*)malloc(bufferSize);
    while (1) {
        // Read a line from the file
        if (fgets(buffer + offset, bufferSize - offset, file) == NULL) {
            free(buffer);
            return NULL; // Nothing could be read.
        }
        offset = strlen(buffer);
        assert(offset > 0);

        if (feof(file)) // EOF was reached
            break;

        // If a newline was found, remove the trailing newline and exit
        if (buffer[offset - 1] == '\n') {
            buffer[offset - 1] = '\0';
            break;
        }

        // If no newline or EOF was reached, then the whole line wasn't read.
        bufferSize *= 2; // Increase buffer size
        buffer = (char*)realloc(buffer, bufferSize);
        assert(buffer != NULL);
    }

    return buffer;
}

// Returns the position of the first non-whitespace character
static char *skip_whitespace(char *str) {
    while (isspace(*str))
        str++;
    return str;
}

// Returns the position of the first non-whitespace or '=' character
static char *skip_whitespace_or_equal(char *str) {
    while (isspace(*str) || *str=='=')
        str++;
    return str;
}

// NULL-terminates the current whitespace-delimited word, and returns a pointer to the next word
static char *word_split(char *str) {
    // Precondition: str must not point to whitespace
    assert(!isspace(*str));

    // Find either the next whitespace, '=' or end of string
    while (!isspace(*str) && *str != '\0' && *str != '=')
        str++;
    if (*str == '\0') // End of string
        return str;

    // Terminate current word
    *(str++) = '\0';

    // Skip whitespace to next word
    return skip_whitespace_or_equal(str);
}

// Splits a string into words, and stores the words into the 'tokens' array
// 'maxTokens' is the length of the 'tokens' array
// Returns the number of tokens parsed
static unsigned int tokenize_string(char *str, int maxTokens, char **tokens) {
    int count = 0;

    str = skip_whitespace(str);
    while (str[0] != '\0' && count < maxTokens) {
        tokens[count] = str;
        str = word_split(str);
        count++;
    }
    return count;
}

// Loads the config file specified by 'filepath'
void configfile_load(const char *filepath) {
    FILE *file;
    char *line;
    unsigned int cur_line = 0;
    char *current_section = NULL;

    printf("Loading configuration from '%s'\n", filepath);

    // Open file or create it if it does not exist
    file = fopen(filepath, "r");
    if (file == NULL) {
        // Create a new config file and save defaults
        printf("Config file '%s' not found. Creating it.\n", filepath);
        configfile_save(filepath);
        return;
    }

    // Go through each line in the file
    while ((line = read_file_line(file)) != NULL) {
        char *p = line;
        char *tokens[2];
        int numTokens;
        cur_line++;

        // Get tokens
        while (isspace(*p)) p++;
        numTokens = tokenize_string(p, 2, tokens);

        // Get content
        if (numTokens != 0) {            

            // Pass comments
            if(tokens[0][0]=='#') continue;

            // Check sections - useless for now
            if(tokens[0][0]=='['){
                p=tokens[0];
                while(*p != '\0' && *p!=']') p++;
                if(*p == '\0') continue;
                *p=0;
                if(current_section) free(current_section);
                current_section = (char*)malloc(strlen(tokens[0])); //strlen(tokens[0])-1+1
                strcpy(current_section, &tokens[0][1]);
                printf("New Section: %s\n", current_section);
                continue;
            }

            if (numTokens == 2) {
                const struct ConfigOption *option = NULL;

                for (unsigned int i = 0; i < ARRAY_LEN(options); i++) {
                    if (strcmp(tokens[0], options[i].name) == 0) {
                        option = &options[i];
                        break;
                    }
                }
                if (option == NULL){
                    printf("Unknown option '%s'\n", tokens[0]);
                }
                else {
                    printf("Reading option: '%s', value: '%s'\n", tokens[0], tokens[1]);
                    switch (option->type) {
                        case CONFIG_TYPE_BOOL:
                            if (strcmp(tokens[1], "true") == 0)
                                *option->boolValue = true;
                            else if (strcmp(tokens[1], "false") == 0)
                                *option->boolValue = false;
                            else{
                                printf("Unknown CONFIG_TYPE_BOOL value: '%s', using default: %s\n", 
                                    tokens[1], (*option->boolValue)?"true":"false");
                            }
                            break;
                        case CONFIG_TYPE_UINT:
                            sscanf(tokens[1], "%u", option->uintValue);
                            break;
                        case CONFIG_TYPE_FLOAT:
                            sscanf(tokens[1], "%f", option->floatValue);
                            break;
                        case CONFIG_TYPE_ASPECT_RATIO:
                            ;unsigned int cur_ar;
                            for(cur_ar=0; cur_ar<NB_ASPECT_RATIOS_TYPES; cur_ar++){
                                if(!strcmp(aspect_ratio_name[cur_ar], tokens[1])){
                                    *option->uintValue = cur_ar;
                                    break;
                                }
                            }
                            if(cur_ar >= NB_ASPECT_RATIOS_TYPES){
                                printf("Unknown CONFIG_TYPE_ASPECT_RATIO value: '%s', using default value: %s\n", 
                                    tokens[1], aspect_ratio_name[*option->uintValue]);
                            }
                            break;
                        default:
                            printf("Unknown option type '%d'\n", option->type);
                            break;
                    }
                }
            } 
            else{
                fprintf(stderr, "Error in line %d: wrong format\n", cur_line);
            }
        }
        free(line);
    }

    fclose(file);
}

// Writes the config file to 'filepath'
void configfile_save(const char *filepath) {
    FILE *file;

    printf("Saving configuration to '%s'\n", filepath);

    file = fopen(filepath, "w");
    if (file == NULL) {
        // error
        printf("Could not save\n");
        return;
    }
    printf("Saved !\n");

    for (unsigned int i = 0; i < ARRAY_LEN(options); i++) {
        const struct ConfigOption *option = &options[i];

        switch (option->type) {
            case CONFIG_TYPE_BOOL:
                fprintf(file, "%s = %s\n", option->name, *option->boolValue ? "true" : "false");
                break;
            case CONFIG_TYPE_UINT:
                fprintf(file, "%s = %u\n", option->name, *option->uintValue);
                break;
            case CONFIG_TYPE_FLOAT:
                fprintf(file, "%s = %f\n", option->name, *option->floatValue);
                break;
            case CONFIG_TYPE_ASPECT_RATIO:
                fprintf(file, "%s = %s\n", option->name, aspect_ratio_name[*option->uintValue]);
                break;
            default:
                assert(0); // unknown type
        }
    }

    fclose(file);
}
