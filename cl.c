#include "cl.h"

// this provides a nice _WIN32 definition
#include <GLFW/glfw3.h>
#if _WIN32
#include <Windows.h>
#endif

// TODO: add support for EGL

#if defined(_WIN32)
#define GLFW_EXPOSE_NATIVE_WIN32 1
#define GLFW_EXPOSE_NATIVE_WGL 1
#elif defined(__APPLE__)
#define GLFW_EXPOSE_NATIVE_COCOA 1
#define GLFW_EXPOSE_NATIVE_NSGL 1
#else
#define GLFW_EXPOSE_NATIVE_X11 1
#define GLFW_EXPOSE_NATIVE_GLX 1
#endif

#include <GLFW/glfw3native.h>

#include "globals.h"

#if TRACER_CL
cl_context_properties *getContextProperties(cl_platform_id platform_id) {
    static cl_context_properties props[] = {
        CL_CONTEXT_PLATFORM,
        0,
        CL_GL_CONTEXT_KHR,
        0,
        CL_WGL_HDC_KHR,
        0,
        0,
    };
    props[1] = (cl_context_properties)platform_id;
#if _WIN32
    props[3] = (cl_context_properties)glfwGetWGLContext(window);
    props[5] =  (cl_context_properties)wglGetCurrentDC();
#elif __APPLE__
    props[3] = (cl_context_properties)glfwGetNSGLContext(window);
#else
    props[3] = (cl_context_properties)glfwGetGLXContext(window);
#endif
    return props;
}
#endif
