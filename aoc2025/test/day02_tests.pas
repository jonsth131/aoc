unit day02_tests;

{$mode objfpc}{$H+}

interface

uses
  fpcunit, testregistry, day02, uTestUtils;

type
  TDay02Tests = class(TTestCase)
  published
    procedure TestPart1Example;
    procedure TestPart2Example;
  end;

implementation

procedure TDay02Tests.TestPart1Example;
var
  Problem: TDay02;
  Input: string;
begin
  Input := LoadFileAsString('inputs/day02_example.txt');
  Problem := TDay02.Create(Input);
  try
    AssertEquals('Part1 example failed', '1227775554', Problem.Part1);
  finally
    Problem.Free;
  end;
end;

procedure TDay02Tests.TestPart2Example;
var
  Problem: TDay02;
  Input: string;
begin
  Input := LoadFileAsString('inputs/day02_example.txt');
  Problem := TDay02.Create(Input);
  try
    AssertEquals('Part2 example failed', '4174379265', Problem.Part2);
  finally
    Problem.Free;
  end;
end;

initialization
  RegisterTest(TDay02Tests);

end.
