# Using the dev container
Prerequisites:
- [Docker](https://www.docker.com/) is installed and running.
- The [Dev Containers extension](https://marketplace.visualstudio.com/items?itemName=ms-vscode-remote.remote-containers) for [Visual Studio Code (VSCode)](https://code.visualstudio.com/) is installed.

When opening this folder in VSCode, a popup should appear prompting you to reopen the folder in a dev container. If not, this can be done by pressing <kbd>F1</kbd> and finding the command `Dev Containers: Reopen in Container`.  
Now, the container will start to build. The first time, this may take a while.

# Using GLEE
For the remainder of the documentation, I will assume the dev container is used.

## Installation
The program first needs to be built. Simply run the command
```
$ cabal install --overwrite-policy=always
```
in the root folder. This builds the prorgam and puts in on PATH so it can easily be used.
You can check if this step was successful by running the following command, and see if the output matches:
```
$ glee --version
The (Glorious) GCL Logical Execution Engine, version 1.0.0
```

## Usage
To run the program on a GCL program, run the command
```
$ glee <file> -k <depth>
```
This will verify all execution paths of length up to `k` of the program in the specified file.<sup>[1](#benchmarkFootnote)</sup>


Optimisations can be enabled using the following flags:
- `-p <depth>`: check for infeasible paths, up to the specified depth.
- `-e`: perform front-end simplification of expressions.
- `-i`: detect annotated loop invariants, and try to prove these.

Additionally, some statistics can be reported using the flag `-s`.

For more information, you can use
```
$ glee --help
```

--------------------

<a name="benchmarkFootnote">1</a>: Note that the benchmark programs cannot be used directly, since the parameter N needs to be replaced. This is done by the test program.
