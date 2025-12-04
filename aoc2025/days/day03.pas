unit day03;

{$mode objfpc}{$H+}

interface

uses
  Runner, SysUtils, Classes, Utils, Parsers;

type
  TDay03 = class(TProblem)
  private
    FList: TStringList;
    FParsed: boolean;
    procedure EnsureParsed;
  public
    function Part1: string; override;
    function Part2: string; override;
  end;

function CalculateLargest(const S: string; const WindowSize: integer): UInt64;

implementation

procedure TDay03.EnsureParsed;
begin
  if not FParsed then
  begin
    FList := ParseLinesToStringList(FInput);
    FParsed := True;
  end;
end;

function CalculateLargest(const S: string; const WindowSize: integer): UInt64;
var
  Stack: array of integer;
  i, Top, Digit, DropCount: integer;
  C: char;
begin
  Result := 0;
  if (WindowSize <= 0) or (S = '') or (Length(S) < WindowSize) then
    Exit(0);

  SetLength(Stack, Length(S));
  Top := 0;
  DropCount := Length(S) - WindowSize;

  for i := 1 to Length(S) do
  begin
    C := S[i];
    Digit := Ord(C) - Ord('0');

    while (Top > 0) and (DropCount > 0) and (Digit > Stack[Top - 1]) do
    begin
      Dec(Top);
      Dec(DropCount);
    end;

    Stack[Top] := Digit;
    Inc(Top);
  end;

  if Top > WindowSize then
    Top := WindowSize;

  for i := 0 to WindowSize - 1 do
    Result := (Result * 10) + Stack[i];
end;

function TDay03.Part1: string;
var
  Res, i: integer;
begin
  EnsureParsed;
  Res := 0;

  for i := 0 to FList.Count - 1 do
  begin
    Inc(Res, CalculateLargest(FList[i], 2));
  end;

  Result := IntToStr(Res);
end;

function TDay03.Part2: string;
var
  Res: UInt64;
  i: integer;
begin
  EnsureParsed;
  Res := 0;

  for i := 0 to FList.Count - 1 do
  begin
    Inc(Res, CalculateLargest(FList[i], 12));
  end;

  Result := IntToStr(Res);
end;

initialization
  RegisterDay(3, TDay03);

end.
