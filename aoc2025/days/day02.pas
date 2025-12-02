unit day02;

{$mode objfpc}{$H+}

interface

uses
  Runner, SysUtils, Classes, Utils, Parsers;

type
  TRange = record
    First: QWord;
    Last: QWord;
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
          FList[i].First := StrToQWordDef(Trim(Parts[0]), 0);
          FList[i].Last := StrToQWordDef(Trim(Parts[1]), 0);
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
  Res, j: QWord;
  i: integer;
  StrVal: string;
  Middle: integer;
  LeftPart, RightPart: string;
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
      if (Length(StrVal) > 0) and (Length(StrVal) mod 2 = 0) then
      begin
        Middle := Length(StrVal) div 2;
        if (Middle > 0) and (Middle + 1 <= Length(StrVal)) then
        begin
          LeftPart := Copy(StrVal, 1, Middle);
          RightPart := Copy(StrVal, Middle + 1, Middle);
          if LeftPart = RightPart then
            Inc(Res, j);
        end;
      end;
    end;
  end;

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
