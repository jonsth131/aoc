program tests;

{$mode objfpc}{$H+}

uses
  fpcunit,
  testregistry,
  testutils,
  consoletestrunner,
  uUtils_tests,
  day01_tests,
  day02_tests,
  day03_tests,
  day04_tests,
  day05_tests;

begin
  // Run all registered tests using a console runner
  with TTestRunner.Create(nil) do
    try
      Initialize;   // sets up runner
      Run;          // executes all tests
    finally
      Free;
    end;
end.
