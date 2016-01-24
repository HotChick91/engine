# Engine

## Setup

Requires GLFW 3.

An OpenCL library is required (1.2 and 2.0 are supported).
When running with OpenCL support enabled, make sure that `ray.cl` is in the working directory.

To get this working:

* install GLFW and OpenCL (`ocl-icd-opencl-dev` is fine if you don't want hardware rendering)
* if running Windows, add paths to these libraries to `gameengine.cabal`
* install stack, run `stack setup`
* run `stack init` in the directory containing `gameengine.cabal`
* finally, run `stack install` to compile
