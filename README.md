# Engine

Requires GLFW 3.

OpenCL is required (1.2 and 2.0 are supported).
When running with OpenCL support enabled, make sure that `ray.cl` is in the working directory.

To get this working in Visual Studio, add:

* GLFW and OpenCL include directories to Configuration Properties -> C/C++ -> General -> Additional Include Directories
* GLFW and OpenCL library directories to Configuration Properties -> Linker -> General -> Additional Library Directories
* glfw3.lib, opengl32.lib, and OpenCL.lib to Configuration Properties -> Linker -> Input -> Additional Dependencies
