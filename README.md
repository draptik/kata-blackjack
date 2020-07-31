# Blackjack

## TODOs

- the easy part is implementing the rules for a single hand with a given deck
- the difficult part is building a clean model for interactive code, with multiple players, and bets (https://de.wikipedia.org/wiki/Black_Jack#Spielablauf)
- property-based-testing

## Linux IDE FSharp support

### 2020-07-31

```sh
.NET Core SDK (reflecting any global.json):
 Version:   3.1.301
 Commit:    7feb845744

Runtime Environment:
 OS Name:     arch
 OS Version:
 OS Platform: Linux
 RID:         arch-x64
 Base Path:   /usr/share/dotnet/sdk/3.1.301/

Host (useful for support):
  Version: 5.0.0-preview.7.20364.11
  Commit:  53976d38b1

.NET SDKs installed:
  3.1.106 [/usr/share/dotnet/sdk]
  3.1.301 [/usr/share/dotnet/sdk]

.NET runtimes installed:
  Microsoft.AspNetCore.App 3.1.5 [/usr/share/dotnet/shared/Microsoft.AspNetCore.App]
  Microsoft.AspNetCore.App 3.1.6 [/usr/share/dotnet/shared/Microsoft.AspNetCore.App]
  Microsoft.NETCore.App 3.1.5 [/usr/share/dotnet/shared/Microsoft.NETCore.App]
  Microsoft.NETCore.App 3.1.6 [/usr/share/dotnet/shared/Microsoft.NETCore.App]
```

### 2020-06-23

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

#### Rider

Version: Rider 2020.1.4

NOTE: Opening the solution file from the command line via `rider BlackJack.sln&` fails. Opening the
solution from within Rider works.

- unit tests work
- fsi works

#### VS Code

Ionide works.

## Resources

// https://github.com/todoa2c/blackjack-fsharp
// https://github.com/dudeNumber4/fsharp-blackjack
// https://github.com/defshef/defshef-blackjack/tree/master/fsharp
// https://github.com/leandrosilva/fsharp-learning/blob/master/fsharp-tutorial-jaoo-2009/tutorial/FunctionalTypes/ExerciseSolution.fsx
