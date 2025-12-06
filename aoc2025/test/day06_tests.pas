unit day06_tests;

{$mode objfpc}{$H+}

interface

uses
  fpcunit, testregistry, day06, uTestUtils, Parsers, Classes;

type
  TDay06Tests = class(TTestCase)
  published
    procedure TestPart1Example;
    procedure TestPart2Example;
  end;

implementation

procedure TDay06Tests.TestPart1Example;
var
  Problem: TDay06;
  Input: string;
begin
  Input := LoadFileAsString('inputs/day06_example.txt');
  Problem := TDay06.Create(Input);
  try
    AssertEquals('Part1 example failed', '4277556', Problem.Part1);
  finally
    Problem.Free;
  end;
end;

procedure TDay06Tests.TestPart2Example;
var
  Problem: TDay06;
  Input: string;
begin
  Input := LoadFileAsString('inputs/day06_example.txt');
  Problem := TDay06.Create(Input);
  try
    AssertEquals('Part2 example failed', '3263827', Problem.Part2);
  finally
    Problem.Free;
  end;
end;

initialization
  RegisterTest(TDay06Tests);

end.
