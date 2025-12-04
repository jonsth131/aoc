unit day04;

{$mode objfpc}{$H+}

interface

uses
  Runner, SysUtils, Classes, Utils, Parsers;

type
  TDay04 = class(TProblem)
  private
    FList: TArrayOfArrayOfChar;
    FParsed: boolean;
    procedure EnsureParsed;
  public
    function Part1: string; override;
    function Part2: string; override;
  end;

  TPoint = record
    X: integer;
    Y: integer;
  end;

  TPointArray = array of TPoint;

function GetNumberOfAdjacentChars(const CharMatrix: TArrayOfArrayOfChar;
  const Row, Col: integer): integer;

function ProcessPaperRolls(const CharMatrix, Clone: TArrayOfArrayOfChar): integer;

function ProcessAllPaperRolls(CharMatrix: TArrayOfArrayOfChar): integer;

function CloneCharMatrix(const CharMatrix: TArrayOfArrayOfChar): TArrayOfArrayOfChar;

implementation

procedure TDay04.EnsureParsed;
begin
  if not FParsed then
  begin
    FList := ParseLinesToCharMatrix(FInput);
    FParsed := True;
  end;
end;

function GetNumberOfAdjacentChars(const CharMatrix: TArrayOfArrayOfChar;
  const Row, Col: integer): integer;
var
  x, y: integer;
begin
  Result := 0;

  if CharMatrix[Row][Col] <> '@' then
    Exit(0);

  for x := -1 to 1 do
    for y := -1 to 1 do
      if Row - x < 0 then
        Continue
      else if Col - y < 0 then
        Continue
      else if (x = 0) and (y = 0) then
        Continue
      else if Row - x >= Length(CharMatrix) then
        Continue
      else if Col - y >= Length(CharMatrix[Row - x]) then
        Continue
      else if CharMatrix[Row - x][Col - y] = '@' then
        Inc(Result);
end;

function ProcessPaperRolls(const CharMatrix, Clone: TArrayOfArrayOfChar): integer;
var
  i, j: integer;
  Adjacent: integer;
begin
  Result := 0;

  for i := 0 to Length(CharMatrix) - 1 do
  begin
    for j := 0 to Length(CharMatrix[i]) - 1 do
    begin
      if CharMatrix[i][j] <> '@' then
        Continue;

      Adjacent := GetNumberOfAdjacentChars(CharMatrix, i, j);

      if Adjacent < 4 then
      begin
        Inc(Result);
        Clone[i][j] := '.';
      end;
    end;
  end;
end;

function CloneCharMatrix(const CharMatrix: TArrayOfArrayOfChar): TArrayOfArrayOfChar;
var
  i: integer;
begin
  Result := nil;
  SetLength(Result, Length(CharMatrix));
  for i := 0 to Length(CharMatrix) - 1 do
    Result[i] := Copy(CharMatrix[i], 0, Length(CharMatrix[i]));
end;

function ProcessAllPaperRolls(CharMatrix: TArrayOfArrayOfChar): integer;
var
  i: integer;
  Clone: TArrayOfArrayOfChar;
  PassResult: integer;
begin
  Result := 0;
  Clone := CloneCharMatrix(CharMatrix);
  repeat
    PassResult := ProcessPaperRolls(CharMatrix, Clone);
    if PassResult = 0 then
      Break;
    Inc(Result, PassResult);
    CharMatrix := CloneCharMatrix(Clone);
  until (PassResult = 0);
end;

function TDay04.Part1: string;
var
  Res: integer;
  i, j: integer;
  Adjacent: integer;
  Clone: TArrayOfArrayOfChar;
begin
  EnsureParsed;

  Clone := CloneCharMatrix(FList);
  Res := ProcessPaperRolls(FList, Clone);

  Result := IntToStr(Res);
end;

function TDay04.Part2: string;
var
  Res: integer;
  i, j: integer;
  Adjacent: integer;
begin
  EnsureParsed;

  Res := ProcessAllPaperRolls(FList);

  Result := IntToStr(Res);
end;

initialization
  RegisterDay(4, TDay04);

end.
