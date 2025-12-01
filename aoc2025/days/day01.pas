unit day01;

{$mode objfpc}{$H+}

interface

uses
  Runner, SysUtils, Classes, Utils, Parsers;

type
  TDay01 = class(TProblem)
  private
    FList: TStringList;
    FParsed: boolean;
    procedure EnsureParsed;
  public
    function Part1: string; override;
    function Part2: string; override;
  end;

implementation

procedure TDay01.EnsureParsed;
begin
  if not FParsed then
  begin
    FList := ParseLinesToStringList(FInput);
    FParsed := True;
  end;
end;

function TDay01.Part1: string;
var
  Res, i: integer;
  Position, Steps: integer;
  Direction: string;
begin
  EnsureParsed;

  Position := 50;
  Res := 0;

  for i := 0 to FList.Count - 1 do
  begin
    Direction := FList[i][1];

    Steps := StrToIntDef(Trim(Copy(FList[i], 2, Length(FList[i]))), 0);
    if Direction = 'L' then
      Position := Position - Steps
    else if Direction = 'R' then
      Position := Position + Steps;

    if Position mod 100 = 0 then
      Res := Res + 1;
  end;

  Result := IntToStr(Res);
end;

function TDay01.Part2: string;
var
  Res, i, j: integer;
  Position, Steps: integer;
  Direction: string;
begin
  EnsureParsed;

  Position := 50;
  Res := 0;

  for i := 0 to FList.Count - 1 do
  begin
    Direction := FList[i][1];
    Steps := StrToIntDef(Trim(Copy(FList[i], 2, Length(FList[i]))), 0);

    if Direction = 'L' then
      for j := 1 to Steps do
      begin
        Position := Position - 1;
        if Position mod 100 = 0 then
          Res := Res + 1;
      end
    else if Direction = 'R' then
      for j := 1 to Steps do
      begin
        Position := Position + 1;
        if Position mod 100 = 0 then
          Res := Res + 1;
      end;
  end;

  Result := IntToStr(Res);
end;

initialization
  RegisterDay(1, TDay01);

end.
