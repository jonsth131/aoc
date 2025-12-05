unit day05_tests;

{$mode objfpc}{$H+}

interface

uses
  fpcunit, testregistry, day05, uTestUtils;

type
  TDay05Tests = class(TTestCase)
  published
    procedure TestPart1Example;
    procedure TestPart2Example;
  end;

implementation

procedure TDay05Tests.TestPart1Example;
var
  Problem: TDay05;
  Input: string;
begin
  Input := LoadFileAsString('inputs/day05_example.txt');
  Problem := TDay05.Create(Input);
  try
    AssertEquals('Part1 example failed', '3', Problem.Part1);
  finally
    Problem.Free;
  end;
end;

procedure TDay05Tests.TestPart2Example;
var
  Problem: TDay05;
  Input: string;
begin
  Input := LoadFileAsString('inputs/day05_example.txt');
  Problem := TDay05.Create(Input);
  try
    AssertEquals('Part2 example failed', '14', Problem.Part2);
  finally
    Problem.Free;
  end;
end;

initialization
  RegisterTest(TDay05Tests);

end.
