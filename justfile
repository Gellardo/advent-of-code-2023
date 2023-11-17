run DAY:
  dotnet run --project src/Day{{DAY}}

create DAY:
  # create the project
  dotnet new console -lang "F#" -o src/Day{{DAY}}
  # add the global library as a dependency
  dotnet add src/Day{{DAY}} reference src/Library
  # add project to top level solution
  dotnet sln add src/App/App.fsproj
