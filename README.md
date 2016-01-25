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

## Troubleshooting

* 

        Warning: 'extra-lib-dirs: "C:\Program\' directory does not exist.

 Solution: escape your backslashes (`"C:\\Program Files\\..."`)

* 

        X Error of failed request:  BadMatch (invalid parameter attributes)
        Major opcode of failed request:  156 (GLX)
        Minor opcode of failed request:  5 (X_GLXMakeCurrent)

 Solution: add `/usr/lib/fglrx` to `LD_LIBRARY_PATH`
