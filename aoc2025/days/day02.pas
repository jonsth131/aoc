unit day02;

{$mode objfpc}{$H+}

interface

uses
  Runner, SysUtils, Classes, Utils, Parsers, Math;

type
  TRange = record
    First: UInt64;
    Last: UInt64;
  end;
  TRangeList = array of TRange;

  TDay02 = class(TProblem)
  private
    FList: TRangeList;
    FParsed: boolean;
    procedure EnsureParsed;
  public
    function Part1: string; override;
    function Part2: string; override;
  end;

implementation

function SumRepeatedNumbersInRange(const A, B: UInt64): UInt64;
var
  HalfLen, StartP, EndP, Multiplier: UInt64;
  P, N: UInt64;
  MaxDigits: integer;
begin
  Result := 0;
  MaxDigits := Length(UIntToStr(B));

  for HalfLen := 1 to MaxDigits div 2 do
  begin
    StartP := 1;
    if HalfLen > 1 then
      for P := 2 to HalfLen do
        StartP := StartP * 10;

    EndP := (StartP * 10) - 1;

    for P := StartP to EndP do
    begin
      Multiplier := Round(IntPower(10, HalfLen));
      N := (P * Multiplier) + P;

      if N > B then
        Break;

      if (N >= A) and (N <= B) then
        Inc(Result, N);
    end;
  end;
end;

procedure TDay02.EnsureParsed;
var
  Lines: TStringList;
  i: integer;
  Parts: TStringList;
begin
  if not FParsed then
  begin
    Lines := ParseLinesToStringList(FInput, ',');
    SetLength(FList, Lines.Count);
    for i := 0 to Lines.Count - 1 do
    begin
      Parts := ParseLinesToStringList(Lines[i], '-');
      try
        if Parts.Count = 2 then
        begin
          FList[i].First := StrToUInt64Def(Trim(Parts[0]), 0);
          FList[i].Last := StrToUInt64Def(Trim(Parts[1]), 0);
        end;
      finally
        Parts.Free;
      end;
    end;
    Lines.Free;
    FParsed := True;
  end;
end;

function TDay02.Part1: string;
var
  Res: UInt64;
  i: integer;
begin
  EnsureParsed;

  Res := 0;

  for i := 0 to Length(FList) - 1 do
    Inc(Res, SumRepeatedNumbersInRange(FList[i].First, FList[i].Last));

  Result := IntToStr(Res);
end;

function TDay02.Part2: string;
var
  Res, j: QWord;
  i, L: integer;
  StrVal: string;
  Pattern, Repeated: string;
  Rep: integer;
begin
  EnsureParsed;

  Res := 0;

  for i := 0 to Length(FList) - 1 do
  begin
    if FList[i].First > FList[i].Last then
      Continue;
    for j := FList[i].First to FList[i].Last do
    begin
      StrVal := IntToStr(j);
      if (StrVal[1] = '0') then
        Continue;

      for L := 1 to (Length(StrVal) div 2) do
      begin
        if (Length(StrVal) mod L = 0) and (Length(StrVal) >= 2 * L) then
        begin
          Pattern := Copy(StrVal, 1, L);
          Repeated := '';
          for Rep := 1 to (Length(StrVal) div L) do
            Repeated := Repeated + Pattern;
          if Repeated = StrVal then
          begin
            Inc(Res, j);
            Break;
          end;
        end;
      end;
    end;
  end;

  Result := IntToStr(Res);
end;

initialization
  RegisterDay(2, TDay02);

end.
