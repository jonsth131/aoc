unit Utils;

{$mode objfpc}{$H+}

interface

type
  TIntArray = array of integer;

/// Sorts an array of integers in ascending order using QuickSort algorithm.
procedure SortIntArray(var A: TIntArray);

implementation

procedure QuickSort(var A: TIntArray; L, R: integer);
var
  I, J, Pivot, Temp: integer;
begin
  I := L;
  J := R;
  Pivot := A[(L + R) div 2];
  repeat
    while A[I] < Pivot do
      Inc(I);
    while A[J] > Pivot do
      Dec(J);
    if I <= J then
    begin
      Temp := A[I];
      A[I] := A[J];
      A[J] := Temp;
      Inc(I);
      Dec(J);
    end;
  until I > J;
  if L < J then
    QuickSort(A, L, J);
  if I < R then
    QuickSort(A, I, R);
end;

procedure SortIntArray(var A: TIntArray);
begin
  if Length(A) > 1 then
    QuickSort(A, 0, High(A));
end;

end.
