#pragma once

#ifndef TRACER_CL
#define TRACER_CL 1
#endif

#if TRACER_CL
#include <CL/cl.h>
#include <CL/cl_gl.h>

cl_context_properties *getContextProperties(cl_platform_id platform_id);
#endif
