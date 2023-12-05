set export

year := "2023"

r:
  dotnet run --project src/Day$(date "+%-d")
run DAY:
  dotnet run --project src/Day{{DAY}}

create DAY:
  # create the project
  dotnet new console -lang "F#" -o src/Day{{DAY}}
  # add the global library as a dependency
  dotnet add src/Day{{DAY}} reference src/Library
  # add project to top level solution
  dotnet sln add src/Day{{DAY}}
  TESTINPUT=$(pbpaste) perl -pe 's/DAY/{{DAY}}/; s/TESTINPUT/$ENV{"TESTINPUT"}/' template.fs >src/Day{{DAY}}/Program.fs
  just load {{DAY}}
  git add src/Day{{DAY}}

load DAY:
  mkdir -p input/$DAY
  curl -sf "https://adventofcode.com/$year/day/$DAY/input" -H "Cookie: session=$(cat .session)" -o input/$DAY/input.txt

repl:
  dotnet fsi
