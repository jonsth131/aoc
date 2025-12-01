unit uUtils_tests;

{$mode objfpc}{$H+}

interface

uses
  fpcunit, testregistry, Utils;

type
  TUtilsTests = class(TTestCase)
  published
    procedure TestSortAscending;
    procedure TestSortEmpty;
    procedure TestSortSingleElement;
    procedure TestSortAlreadySorted;
    procedure TestSortReverseSorted;
  end;

implementation

procedure TUtilsTests.TestSortAscending;
var
  A: TIntArray;
begin
  A := [5, 2, 9, 1, 3];
  SortIntArray(A);
  AssertEquals(1, A[0]);
  AssertEquals(2, A[1]);
  AssertEquals(3, A[2]);
  AssertEquals(5, A[3]);
  AssertEquals(9, A[4]);
end;

procedure TUtilsTests.TestSortEmpty;
var
  A: TIntArray;
begin
  SetLength(A, 0);
  SortIntArray(A);
  AssertEquals(0, Length(A));
end;

procedure TUtilsTests.TestSortSingleElement;
var
  A: TIntArray;
begin
  A := [42];
  SortIntArray(A);
  AssertEquals(42, A[0]);
end;

procedure TUtilsTests.TestSortAlreadySorted;
var
  A: TIntArray;
begin
  A := [1, 2, 3, 4, 5];
  SortIntArray(A);
  AssertEquals(1, A[0]);
  AssertEquals(5, A[4]);
end;

procedure TUtilsTests.TestSortReverseSorted;
var
  A: TIntArray;
begin
  A := [9, 7, 5, 3, 1];
  SortIntArray(A);
  AssertEquals(1, A[0]);
  AssertEquals(9, A[4]);
end;

initialization
  RegisterTest(TUtilsTests);

end.
