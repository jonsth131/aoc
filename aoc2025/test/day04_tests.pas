unit day04_tests;

{$mode objfpc}{$H+}

interface

uses
  fpcunit, testregistry, day04, uTestUtils;

type
  TDay04Tests = class(TTestCase)
  published
    procedure TestPart1Example;
    procedure TestPart2Example;
  end;

implementation

procedure TDay04Tests.TestPart1Example;
var
  Problem: TDay04;
  Input: string;
begin
  Input := LoadFileAsString('inputs/day04_example.txt');
  Problem := TDay04.Create(Input);
  try
    AssertEquals('Part1 example failed', '13', Problem.Part1);
  finally
    Problem.Free;
  end;
end;

procedure TDay04Tests.TestPart2Example;
var
  Problem: TDay04;
  Input: string;
begin
  Input := LoadFileAsString('inputs/day04_example.txt');
  Problem := TDay04.Create(Input);
  try
    AssertEquals('Part2 example failed', '43', Problem.Part2);
  finally
    Problem.Free;
  end;
end;

initialization
  RegisterTest(TDay04Tests);

end.
