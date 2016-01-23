#pragma once

#include <stdio.h>

#include "cl.h"

void die(char *context, char *detail, int code);
void check_nn(void *ptr, char *msg);
void check_ferror(FILE *stream, char *msg);
#if TRACER_CL
void check_cl(cl_int status, char *msg);
#endif
void error_callback(int error, const char* description);