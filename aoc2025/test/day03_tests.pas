unit day03_tests;

{$mode objfpc}{$H+}

interface

uses
  fpcunit, testregistry, day03, uTestUtils;

type
  TDay03Tests = class(TTestCase)
  published
    procedure TestPart1Example;
    procedure TestPart2Example;
    procedure TestCalculateLargest;
  end;

implementation

procedure TDay03Tests.TestPart1Example;
var
  Problem: TDay03;
  Input: string;
begin
  Input := LoadFileAsString('inputs/day03_example.txt');
  Problem := TDay03.Create(Input);
  try
    AssertEquals('Part1 example failed', '357', Problem.Part1);
  finally
    Problem.Free;
  end;
end;

procedure TDay03Tests.TestPart2Example;
var
  Problem: TDay03;
  Input: string;
begin
  Input := LoadFileAsString('inputs/day03_example.txt');
  Problem := TDay03.Create(Input);
  try
    AssertEquals('Part2 example failed', '3121910778619', Problem.Part2);
  finally
    Problem.Free;
  end;
end;

procedure TDay03Tests.TestCalculateLargest;
begin
  AssertEquals(UInt64(0), CalculateLargest('1234', 5));
  AssertEquals(UInt64(4), CalculateLargest('1234', 1));
  AssertEquals(UInt64(34), CalculateLargest('1234', 2));
  AssertEquals(UInt64(234), CalculateLargest('1234', 3));
  AssertEquals(UInt64(98), CalculateLargest('987654321111111', 2));
  AssertEquals(UInt64(89), CalculateLargest('811111111111119', 2));
  AssertEquals(UInt64(987654321111), CalculateLargest('987654321111111', 12));
  AssertEquals(UInt64(888911112111), CalculateLargest('818181911112111', 12));
end;

initialization
  RegisterTest(TDay03Tests);

end.
