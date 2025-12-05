unit Parsers;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes;

type
  TRange = record
    StartPos: UInt64;
    EndPos: UInt64;
  end;

  TIntArray = array of integer;
  TUInt64Array = array of UInt64;
  TArrayOfArrayOfChar = array of array of char;
  TRangeArray = array of TRange;

  TLists = record
    List1: TIntArray;
    List2: TIntArray;
  end;

/// Parses input lines like "3   4" into two integer lists.
function ParseTwoIntColumns(const input: string): TLists;

/// Parses input lines to a string list.
function ParseLinesToStringList(const input: string): TStringList;

/// Parses input lines to a string list with custom separator.
function ParseLinesToStringList(const input: string; const separator: char): TStringList;

/// Parses input lines to char matrix.
function ParseLinesToCharMatrix(const input: string): TArrayOfArrayOfChar;

/// Parses input lines to an array of UInt64.
function ParseLinesToUInt64Array(const input: string): TUInt64Array;

/// Parses input like "123-456" into ranges.
function ParseLinesToRanges(const input: string): TRangeArray;

implementation

function ParseTwoIntColumns(const input: string): TLists;
var
  SL: TStringList;
  i: integer;
  Line: string;
  P: integer;
begin
  Result.List1 := nil;
  Result.List2 := nil;
  SL := TStringList.Create;
  try
    SL.Text := Trim(input);

    SetLength(Result.List1, SL.Count);
    SetLength(Result.List2, SL.Count);

    for i := 0 to SL.Count - 1 do
    begin
      Line := Trim(SL[i]);
      if Line = '' then
        Continue;

      P := Pos(' ', Line);
      if P > 0 then
      begin
        Result.List1[i] := StrToIntDef(Trim(Copy(Line, 1, P - 1)), 0);
        Result.List2[i] := StrToIntDef(Trim(Copy(Line, P + 1, Length(Line))), 0);
      end
      else
      begin
        Result.List1[i] := 0;
        Result.List2[i] := 0;
      end;
    end;
  finally
    SL.Free;
  end;
end;

function ParseLinesToStringList(const input: string): TStringList;
begin
  Result := TStringList.Create;
  Result.Text := Trim(input);
end;

function ParseLinesToStringList(const input: string; const separator: char): TStringList;
begin
  Result := TStringList.Create;
  Result.Delimiter := separator;
  Result.StrictDelimiter := True;
  Result.DelimitedText := Trim(input);
end;

function ParseLinesToCharMatrix(const input: string): TArrayOfArrayOfChar;
var
  SL: TStringList;
  i, j: integer;
  Line: string;
begin
  SL := TStringList.Create;
  try
    SL.Text := Trim(input);
    SetLength(Result, SL.Count);

    for i := 0 to SL.Count - 1 do
    begin
      Line := SL[i];
      SetLength(Result[i], Length(Line));
      for j := 1 to Length(Line) do
      begin
        Result[i][j - 1] := Line[j];
      end;
    end;
  finally
    SL.Free;
  end;
end;

function ParseLinesToUInt64Array(const input: string): TUInt64Array;
var
  SL: TStringList;
  i: integer;
  Line: string;
begin
  SL := TStringList.Create;
  try
    SL.Text := Trim(input);
    SetLength(Result, SL.Count);
    for i := 0 to SL.Count - 1 do
    begin
      Line := Trim(SL[i]);
      if Line = '' then
        Continue;
      Result[i] := StrToUInt64Def(Line, 0);
    end;
  finally
    SL.Free;
  end;
end;

function ParseLinesToRanges(const input: string): TRangeArray;
var
  SL: TStringList;
  i: integer;
  Line: string;
  P: integer;
begin
  SL := TStringList.Create;
  try
    SL.Text := Trim(input);
    SetLength(Result, SL.Count);
    for i := 0 to SL.Count - 1 do
    begin
      Line := Trim(SL[i]);
      if Line = '' then
        Continue;
      P := Pos('-', Line);
      if P > 0 then
      begin
        Result[i].StartPos := StrToUInt64Def(Trim(Copy(Line, 1, P - 1)), 0);
        Result[i].EndPos := StrToUInt64Def(Trim(Copy(Line, P + 1, Length(Line))), 0);
      end;
    end;
  finally
    SL.Free;
  end;
end;

end.
