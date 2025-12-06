unit day06;

{$mode objfpc}{$H+}

interface

uses
  Runner, SysUtils, Classes, Utils, Parsers, RegExpr, StrUtils;

type
  TDay06 = class(TProblem)
  public
    function Part1: string; override;
    function Part2: string; override;
  end;

  TStringArrayOfArray = array of TStringArray;

function CalculateCephalopodMath(const Input: TStringList; Idx, L: integer;
  Sign: char): UInt64;

function ExtractGrid(const Input: TStringList; Idx, L: integer): TArrayOfArrayOfChar;

function RotateGridCounterClockwise(
  const Grid: TArrayOfArrayOfChar): TArrayOfArrayOfChar;

implementation

function RotateGridCounterClockwise(const Grid: TArrayOfArrayOfChar): TArrayOfArrayOfChar;
var
  Rows, Cols, i, j: integer;
begin
  Rows := Length(Grid);
  if Rows = 0 then
    Exit(nil);
  Cols := Length(Grid[0]);
  SetLength(Result, Cols, Rows);

  for i := 0 to Rows - 1 do
    for j := 0 to Cols - 1 do
      Result[Cols - 1 - j][i] := Grid[i][j];
end;

function ExtractGrid(const Input: TStringList; Idx, L: integer): TArrayOfArrayOfChar;
var
  i, j: integer;
  InputLength: integer;
begin
  Result := nil;
  InputLength := Input.Count - 1;

  SetLength(Result, InputLength);

  for i := 0 to InputLength - 1 do
  begin
    SetLength(Result[i], L);
    for j := Idx to Idx + L - 1 do
    begin
      Result[i][j - Idx] := Input[i][j + 1];
    end;
  end;

  Result := RotateGridCounterClockwise(Result);
end;

function CalculateCephalopodMath(const Input: TStringList; Idx, L: integer;
  Sign: char): UInt64;
var
  i, j: integer;
  Grid: TArrayOfArrayOfChar;
  CurrentValue: integer;
  CurrentChar: char;
begin
  Result := 0;

  if Idx < 0 then
    Idx := 0;

  Grid := ExtractGrid(Input, Idx, L);

  for i := 0 to Length(Grid) - 1 do
  begin
    CurrentValue := 0;
    for j := 0 to Length(Grid[i]) - 1 do
    begin
      CurrentChar := Grid[i][j];
      if CurrentChar = ' ' then
        Continue;

      CurrentValue := (CurrentValue * 10) + StrToInt(CurrentChar);
    end;
    if Result = 0 then
      Result := CurrentValue
    else
    begin
      if Sign = '*' then
        Result := Result * CurrentValue;
      if Sign = '+' then
        Result := Result + CurrentValue;
    end;
  end;
end;

function TDay06.Part1: string;
var
  Res: UInt64;
  i, j: integer;
  Regex: TRegExpr;
  NormalizedInput: string;
  InputData: TStringList;
  Parts: TStringArray;
  Parsed: TStringArrayOfArray;
  Temp: UInt64;
begin
  Res := 0;

  Regex := TRegExpr.Create('(\ +)');
  try
    NormalizedInput := Regex.Replace(FInput, ' ', True);
  finally
    Regex.Free;
  end;

  InputData := ParseLinesToStringList(NormalizedInput);

  Parsed := nil;
  SetLength(Parsed, InputData.Count);

  for i := 0 to InputData.Count - 1 do
  begin
    Parts := SplitString(Trim(InputData[i]), ' ');
    SetLength(Parsed[i], Length(Parts));
    for j := 0 to Length(Parts) - 1 do
    begin
      Parsed[i][j] := Parts[j];
    end;
  end;

  for i := 0 to High(Parsed[0]) do
  begin
    Temp := 0;
    for j := 0 to High(Parsed) - 1 do
    begin
      if Temp = 0 then
      begin
        Temp := StrToUInt64(Parsed[j][i]);
        Continue;
      end;

      if Parsed[High(Parsed)][i] = '*' then
        Temp := Temp * StrToUInt64(Parsed[j][i]);
      if Parsed[High(Parsed)][i] = '+' then
        Temp := Temp + StrToUInt64(Parsed[j][i]);
    end;
    Inc(Res, Temp);
  end;

  Result := IntToStr(Res);
end;

function TDay06.Part2: string;
var
  Res: UInt64;
  InputData: TStringList;
  i: integer;
  LastLine: integer;
  CurrentChar: char;
  CurrentSign: char;
  CurrentLength: integer;
  Temp: UInt64;
begin
  Res := 0;
  CurrentLength := 0;

  InputData := ParseLinesToStringListNoTrim(FInput);
  LastLine := InputData.Count - 1;

  for i := 1 to Length(InputData[LastLine]) do
  begin
    CurrentChar := InputData[LastLine][i];
    if CurrentChar <> ' ' then
    begin
      if CurrentLength = 0 then
      begin
        CurrentLength := 1;
        CurrentSign := CurrentChar;
        Continue;
      end;

      Temp := CalculateCephalopodMath(InputData, i - CurrentLength -
        2, CurrentLength, CurrentSign);
      Inc(Res, Temp);
      CurrentSign := CurrentChar;
      CurrentLength := 0;
      Continue;
    end;

    if CurrentChar = ' ' then
      Inc(CurrentLength);

    if i = Length(InputData[LastLine]) then
    begin
      Temp := CalculateCephalopodMath(InputData, i - CurrentLength -
        1, CurrentLength + 1, CurrentSign);
      Inc(Res, Temp);
    end;
  end;

  Result := IntToStr(Res);
end;

initialization
  RegisterDay(6, TDay06);

end.
