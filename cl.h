#pragma once

#include <CL/cl.h>
#include <CL/cl_gl.h>
#include <CL/cl_ext.h>

cl_context_properties *getContextProperties(cl_platform_id platform_id);
