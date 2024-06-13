#r "src/TreeModel/bin/Debug/net8.0/TreeModel.dll"
#r "tests/TreeModel/bin/Debug/net8.0/TestTreeModel.dll"
#r "nuget:FsCheck"
#r "nuget:FsCheck.Xunit"
open FsCheck
open Helpers
open Tests


Check.Quick ``Rule 1 - There is at least a given distance between nodes at the same level``
Check.Quick ``Rule 2 - Absolute; A parent should be centered over its children``
Check.Quick ``Rule 2 - Relative; A parent should be centered over its children``
Check.Quick ``Rule 3 - The tree should be symmetric with respect to reflection``
Check.Quick ``Rule 4 - identical subtrees are rendered the same``