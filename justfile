set export

year := "2023"

run DAY:
  dotnet run --project src/Day{{DAY}} --no-build

create DAY:
  # create the project
  dotnet new console -lang "F#" -o src/Day{{DAY}}
  # add the global library as a dependency
  dotnet add src/Day{{DAY}} reference src/Library
  # add project to top level solution
  dotnet sln add src/App/App.fsproj

load DAY:
  mkdir -p input/$DAY
  curl -sf "https://adventofcode.com/$year/day/$DAY/input" -H "Cookie: session=$(cat .session)" -o input/$DAY/input.txt

repl:
  dotnet fsi
