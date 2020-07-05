# Linux IDE FSharp support

## 2020-06-23

```sh
dotnet --info
.NET Core SDK (reflecting any global.json):
 Version:   3.1.301
 Commit:    7feb845744

Runtime Environment:
 OS Name:     arch
 OS Version:  
 OS Platform: Linux
 RID:         linux-x64
 Base Path:   /usr/share/dotnet/sdk/3.1.301/

Host (useful for support):
  Version: 5.0.0-preview.5.20278.1
  Commit:  4ae4e2fe08

.NET SDKs installed:
  3.1.103 [/usr/share/dotnet/sdk]
  3.1.301 [/usr/share/dotnet/sdk]
  5.0.100-preview.5.20279.10 [/usr/share/dotnet/sdk]

.NET runtimes installed:
  Microsoft.AspNetCore.App 3.1.3 [/usr/share/dotnet/shared/Microsoft.AspNetCore.App]
  Microsoft.AspNetCore.App 3.1.5 [/usr/share/dotnet/shared/Microsoft.AspNetCore.App]
  Microsoft.AspNetCore.App 5.0.0-preview.5.20279.2 [/usr/share/dotnet/shared/Microsoft.AspNetCore.App]
  Microsoft.NETCore.App 3.1.3 [/usr/share/dotnet/shared/Microsoft.NETCore.App]
  Microsoft.NETCore.App 3.1.5 [/usr/share/dotnet/shared/Microsoft.NETCore.App]
  Microsoft.NETCore.App 5.0.0-preview.5.20278.1 [/usr/share/dotnet/shared/Microsoft.NETCore.App]
```

### Rider

TL/DR doesn't seem to work. Use VS Code (or another editor).

Version: Rider 2020.1.3

Error message:

```sh
Microsoft (R) Build Engine version 16.6.0+5ff7b0c9e for .NET Core
Copyright (C) Microsoft Corporation. All rights reserved.
MSBUILD : error MSB1025: An internal failure occurred while running MSBuild.
System.ComponentModel.Win32Exception (13): Permission denied
   at System.Diagnostics.Process.set_PriorityClassCore(ProcessPriorityClass value)
   at System.Diagnostics.Process.set_PriorityClass(ProcessPriorityClass value)
   at Microsoft.Build.CommandLine.MSBuildApp.Execute(String[] commandLine)
Unhandled exception. System.ComponentModel.Win32Exception (13): Permission denied
   at System.Diagnostics.Process.set_PriorityClassCore(ProcessPriorityClass value)
   at System.Diagnostics.Process.set_PriorityClass(ProcessPriorityClass value)
   at Microsoft.Build.CommandLine.MSBuildApp.Execute(String[] commandLine)
   at Microsoft.Build.CommandLine.MSBuildApp.Main(String[] args)
```

### VS Code

Ionide works.

## Resources

// https://github.com/todoa2c/blackjack-fsharp
// https://github.com/dudeNumber4/fsharp-blackjack
// https://github.com/defshef/defshef-blackjack/tree/master/fsharp
// https://github.com/leandrosilva/fsharp-learning/blob/master/fsharp-tutorial-jaoo-2009/tutorial/FunctionalTypes/ExerciseSolution.fsx