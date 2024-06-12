# Drawing Trees

Implementation of [Andrew J. Kennedy's paper](https://www.microsoft.com/en-us/research/wp-content/uploads/1996/01/drawingtrees.pdf) on drawing beautiful trees.

![Screenshot of Tikz Tree](/assets/tikz-drawing.png)

# Usage
The primary usage is described around the `Makefile`, but it's also possible to use the standard dotnet CLI for the below.

## Run from the project root

Presents a simple cli interface to create trees.

```
make run
```

## Run tests

Runs all tests for the project.

```
make test
```

## Show an example

Show 1 of 5 examples using plotly

```
make example[1-5]
```

# How to build as a self-contained application

Outputs a self-contained application in the `release` folder.

```
make build
```

## Requirements

 - .NET 8.0
