unit Args;

{$mode objfpc}{$H+}

interface

uses
  SysUtils;

type
  TOptions = record
    Day: integer;     // 0 = all registered days
    Part: integer;    // 0 = both, 1 or 2
    InputPath: string;// optional override path
    Bench: integer;   // 0 = off, >0 = iterations
    Quiet: boolean;   // reduce chatter
  end;

function ParseOptions: TOptions;

implementation

function ParseOptions: TOptions;
var
  i: integer;
  N: integer;
  S: string;
begin
  // defaults
  Result.Day := 0;
  Result.Part := 0;
  Result.InputPath := '';
  Result.Bench := 0;
  Result.Quiet := False;

  N := ParamCount;
  i := 1;
  while i <= N do
  begin
    S := ParamStr(i);
    if (S = '--day') and (i < N) then
    begin
      Inc(i);
      Result.Day := StrToIntDef(ParamStr(i), 0);
    end
    else if (S = '--part') and (i < N) then
    begin
      Inc(i);
      Result.Part := StrToIntDef(ParamStr(i), 0);
    end
    else if (S = '--input') and (i < N) then
    begin
      Inc(i);
      Result.InputPath := ParamStr(i);
    end
    else if (S = '--bench') and (i < N) then
    begin
      Inc(i);
      Result.Bench := StrToIntDef(ParamStr(i), 0);
    end
    else if (S = '--all') then
      Result.Day := 0
    else if (S = '--quiet') or (S = '-q') then
      Result.Quiet := True
    else if (S = '--help') or (S = '-h') then
    begin
      Writeln('Advent of Code Runner');
      Writeln('Usage: aoc [--day N] [--part (1|2)] [--input path] [--bench K] [--all] [--quiet]');
      Halt(0);
    end;
    Inc(i);
  end;
end;

end.
