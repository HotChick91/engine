#include "cl.h"

// this provides a nice _WIN32 definition
#include <GLFW/glfw3.h>

// TODO: add support for OS X, EGL
#ifdef _WIN32
#include <Windows.h>
#else
#include <GL/glx.h>
#endif

#include "globals.h"

cl_context_properties *getContextProperties(cl_platform_id platform_id) {
    static cl_context_properties props[] = {
        CL_CONTEXT_PLATFORM,
        0,
        CL_GL_CONTEXT_KHR,
        0,
        CL_WGL_HDC_KHR,
        0,
        0,
        0,
        0,
    };
    props[1] = (cl_context_properties)platform_id;
#if _WIN32
    props[3] = (cl_context_properties)wglGetCurrentContext();
    props[4] = CL_WGL_HDC_KHR;
    props[5] = (cl_context_properties)wglGetCurrentDC();
#else
    props[3] = (cl_context_properties)glXGetCurrentContext();
    props[4] = CL_GLX_DISPLAY_KHR;
    props[5] = (cl_context_properties)glXGetCurrentDisplay();
#endif
    return props;
}
