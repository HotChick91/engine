# Engine

Requires GLFW 3.

OpenCL is recommended (1.2 and 2.0 are supported).
When running with OpenCL support enabled, make sure that `ray.cl` is in the working directory.
To compile without OpenCL, pass `-DTRACER_CL=0` (or similar, depending on compiler).

To get this working in Visual Studio, add:

* GLFW and optionally OpenCL include directories to Configuration Properties -> C/C++ -> General -> Additional Include Directories
* GLFW and optionally OpenCL library directories to Configuration Properties -> Linker -> General -> Additional Library Directories
* glfw3.lib, opengl32.lib, and optionally OpenCL.lib to Configuration Properties -> Linker -> Input -> Additional Dependencies
