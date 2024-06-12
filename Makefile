run:
	dotnet run --project src/Application/Application.fsproj

test:
	dotnet test

example1:
	dotnet run --project src/Application/Application.fsproj -- example 1

example2:
	dotnet run --project src/Application/Application.fsproj -- example 2

example3:
	dotnet run --project src/Application/Application.fsproj -- example 3

example4:
	dotnet run --project src/Application/Application.fsproj -- example 4

example5:
	dotnet run --project src/Application/Application.fsproj -- example 5

clean:
	dotnet clean
	rm -rf release

build:
	dotnet publish \
  --framework net8.0 \
  --self-contained true \
	--configuration Release \
  -p:PublishSingleFile=true \
  -p:IncludeNativeLibrariesForSelfExtract=true \
  -p:PublishTrimmed=true \
  -p:PublishReadyToRun=true \
  -p:EnableCompressionInSingleFile=true \
  -p:DebugType=None \
	-p:DebugSymbols=false \
  --output release
