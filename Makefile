run:
	dotnet run --project src/Application/Application.fsproj

test:
	dotnet test

bench:
	dotnet run -c Release --project src/Benchmarking/Benchmarking.fsproj

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
	--configuration Release \
  	--framework net8.0 \
 	--self-contained true \
	-p:DebugSymbols=false \
  	-p:DebugType=None \
  	-p:EnableCompressionInSingleFile=true \
  	-p:IncludeNativeLibrariesForSelfExtract=true \
  	-p:PublishReadyToRun=true \
 	-p:PublishSingleFile=true \
  	-p:PublishTrimmed=true \
  	--output release
