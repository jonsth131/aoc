unit day01_tests;

{$mode objfpc}{$H+}

interface

uses
  fpcunit, testregistry, day01, uTestUtils;

type
  TDay01Tests = class(TTestCase)
  published
    procedure TestPart1Example;
    procedure TestPart2Example;
  end;

implementation

procedure TDay01Tests.TestPart1Example;
var
  Problem: TDay01;
  Input: string;
begin
  Input := LoadFileAsString('inputs/day01_example.txt');
  Problem := TDay01.Create(Input);
  try
    AssertEquals('Part1 example failed', '3', Problem.Part1);
  finally
    Problem.Free;
  end;
end;

procedure TDay01Tests.TestPart2Example;
var
  Problem: TDay01;
  Input: string;
begin
  Input := LoadFileAsString('inputs/day01_example.txt');
  Problem := TDay01.Create(Input);
  try
    AssertEquals('Part2 example failed', '6', Problem.Part2);
  finally
    Problem.Free;
  end;
end;

initialization
  RegisterTest(TDay01Tests);

end.
