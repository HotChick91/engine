# Engine

## Setup

Requires GLFW 3.

An OpenCL library is required (1.2 and 2.0 are supported).
When running with OpenCL support enabled, make sure that `ray.cl` is in the working directory.

To get this working:

* Install `stack` (<http://docs.haskellstack.org/en/stable/README.html>)
* Install GLFW3 and OpenCL (`ocl-icd-opencl-dev` is enough if you don't want hardware rendering)
* If running Windows, add their include directories to the `CPATH` environment variable, and library directories to `LIBRARY_PATH` (semicolon-separated).
You can confirm that all variables are set correctly by running `stack exec env`. Reboot in case of problems.
* Finally, run `stack setup` and `stack install` in the directory containing `gameengine.cabal`

## Troubleshooting

*

        X Error of failed request:  BadMatch (invalid parameter attributes)
        Major opcode of failed request:  156 (GLX)
        Minor opcode of failed request:  5 (X_GLXMakeCurrent)

 Solution: add `/usr/lib/fglrx` to `LD_LIBRARY_PATH`
