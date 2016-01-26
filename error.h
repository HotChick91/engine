#pragma once

#include <stdio.h>

#include "cl.h"

void die(char *context, char *detail, int code);
void check_nn(void *ptr, char *msg);
void check_ferror(FILE *stream, char *msg);
void check_cl(cl_int status, char *msg);
void check_gl(void);
void error_callback(int error, const char* description);
