unit day05;

{$mode objfpc}{$H+}

interface

uses
  Runner, SysUtils, Classes, Utils, Parsers;

type
  TDay05 = class(TProblem)
  private
    FRanges: TRangeArray;
    FValues: TUInt64Array;
    FParsed: boolean;
    procedure EnsureParsed;
  public
    function Part1: string; override;
    function Part2: string; override;
  end;

function InRange(const R: TRange; const Value: UInt64): boolean;

function CompareRanges(const R1, R2: TRange): integer;

procedure SortRanges(R: TRangeArray);

function MergeRanges(R: TRangeArray): TRangeArray;

function CountTotalRangeLength(R: TRangeArray): UInt64;

implementation

procedure TDay05.EnsureParsed;
var
  Ranges, Values: string;
begin
  if not FParsed then
  begin
    Ranges := Trim(Copy(FInput, 1, Pos(#10#10, FInput) - 1));
    Values := Trim(Copy(FInput, Pos(#10#10, FInput) + 2, Length(FInput)));

    FRanges := ParseLinesToRanges(Ranges);
    FValues := ParseLinesToUInt64Array(Values);

    FParsed := True;
  end;
end;

function InRange(const R: TRange; const Value: UInt64): boolean;
begin
  Result := (Value >= R.StartPos) and (Value <= R.EndPos);
end;

function CompareRanges(const R1, R2: TRange): integer;
begin
  if R1.StartPos < R2.StartPos then
    Exit(-1)
  else if R1.StartPos > R2.StartPos then
    Exit(1)
  else
    Exit(0);
end;

procedure SortRanges(R: TRangeArray);
var
  i, j: integer;
  Temp: TRange;
begin
  for i := 0 to High(R) - 1 do
    for j := i + 1 to High(R) do
      if CompareRanges(R[i], R[j]) > 0 then
      begin
        Temp := R[i];
        R[i] := R[j];
        R[j] := Temp;
      end;
end;

function MergeRanges(R: TRangeArray): TRangeArray;
var
  i, j: integer;
  Merged: TRangeArray;
  Current: TRange;
begin
  if Length(R) = 0 then
    Exit(R);

  SortRanges(R);

  SetLength(Merged, 0);
  Current := R[0];

  for i := 1 to High(R) do
  begin
    if R[i].StartPos <= Current.EndPos + 1 then
    begin
      if R[i].EndPos > Current.EndPos then
        Current.EndPos := R[i].EndPos;
    end
    else
    begin
      j := Length(Merged);
      SetLength(Merged, j + 1);
      Merged[j] := Current;
      Current := R[i];
    end;
  end;

  j := Length(Merged);
  SetLength(Merged, j + 1);
  Merged[j] := Current;

  Result := Merged;
end;

function CountTotalRangeLength(R: TRangeArray): UInt64;
var
  i: integer;
begin
  Result := 0;
  if Length(R) = 0 then
    Exit;
  for i := 0 to High(R) do
  begin
    Result := Result + (R[i].EndPos - R[i].StartPos + 1);
  end;
end;

function TDay05.Part1: string;
var
  Res, i, j: integer;
begin
  EnsureParsed;

  Res := 0;

  for i := 0 to Length(FValues) - 1 do
  begin
    for j := 0 to Length(FRanges) - 1 do
    begin
      if InRange(FRanges[j], FValues[i]) then
      begin
        Inc(Res);
        Break;
      end;
    end;
  end;

  Result := IntToStr(Res);
end;

function TDay05.Part2: string;
var
  Res: UInt64;
  i: integer;
  Merged: TRangeArray;
begin
  EnsureParsed;

  Res := 0;

  Merged := MergeRanges(FRanges);

  Res := CountTotalRangeLength(Merged);

  Result := IntToStr(Res);
end;

initialization
  RegisterDay(5, TDay05);

end.
