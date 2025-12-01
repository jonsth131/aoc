unit Runner;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, DateUtils, Args;

type
  TProblem = class
  protected
    FInput: string;
  public
    constructor Create(const AInput: string); virtual;
    function Part1: string; virtual; abstract;
    function Part2: string; virtual; abstract;
  end;

  TProblemClass = class of TProblem;

  TPartFunc = function: string of object;

procedure RegisterDay(ADay: integer; AClass: TProblemClass);
function RunAOC(const Opt: TOptions): integer;

function ReadAllTextUTF8(const FileName: string): string;
function DefaultInputPath(Day: integer): string;

implementation

type
  TDayEntry = record
    Day: integer;
    Cls: TProblemClass;
  end;

var
  Registry: array of TDayEntry;

constructor TProblem.Create(const AInput: string);
begin
  inherited Create;
  FInput := AInput;
end;

procedure RegisterDay(ADay: integer; AClass: TProblemClass);
var
  i: integer;
begin
  for i := 0 to High(Registry) do
    if Registry[i].Day = ADay then
      raise Exception.CreateFmt('Day %d already registered', [ADay]);

  SetLength(Registry, Length(Registry) + 1);
  Registry[High(Registry)].Day := ADay;
  Registry[High(Registry)].Cls := AClass;
end;

function ReadAllTextUTF8(const FileName: string): string;
var
  FS: TFileStream;
  S: TStringStream;
begin
  if not FileExists(FileName) then
    raise Exception.CreateFmt('Input file not found: %s', [FileName]);
  FS := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    S := TStringStream.Create('');
    try
      S.CopyFrom(FS, 0);
      Result := S.DataString;
    finally
      S.Free;
    end;
  finally
    FS.Free;
  end;
end;

function DefaultInputPath(Day: integer): string;
begin
  Result := Format('inputs/day%.2d.txt', [Day]);
end;

type
  TTimeResult = record
    Value: string;
    Ms: int64;
  end;

function TimeIt(Func: TPartFunc): TTimeResult;
var
  t0, t1: QWord;
begin
  t0 := GetTickCount64;
  Result.Value := Func();
  t1 := GetTickCount64;
  Result.Ms := t1 - t0;
end;

procedure PrintHeader;
begin
  Writeln('=== Advent of Code 2025 ===');
end;

procedure PrintRow(Day, Part: integer; const Ans: string; Ms: int64);
begin
  Writeln(Format('Day %2d Part %d: %-20s (%d ms)', [Day, Part, Ans, Ms]));
end;

function LoadInputFor(Day: integer; const OverridePath: string): string;
begin
  if OverridePath <> '' then
    Result := ReadAllTextUTF8(OverridePath)
  else
    Result := ReadAllTextUTF8(DefaultInputPath(Day));
end;

function RunSingleDay(Day: integer; const Opt: TOptions): integer;
var
  i: integer;
  Cls: TProblemClass;
  Prob: TProblem;
  r: TTimeResult;
  Ans: string;
  AccTime: int64;
begin
  Cls := nil;
  for i := 0 to High(Registry) do
    if Registry[i].Day = Day then
      Cls := Registry[i].Cls;

  if Cls = nil then
  begin
    Writeln(Format('Day %d not registered.', [Day]));
    Exit(2);
  end;

  Prob := Cls.Create(LoadInputFor(Day, Opt.InputPath));
  try
    if Opt.Part in [0, 1] then
    begin
      if Opt.Bench > 0 then
      begin
        AccTime := 0;
        Ans := '';
        for i := 1 to Opt.Bench do
        begin
          r := TimeIt(@Prob.Part1);
          AccTime += r.Ms;
          if i = 1 then
            Ans := r.Value;
        end;
        if not Opt.Quiet then
          PrintRow(Day, 1, Ans, AccTime);
      end
      else
      begin
        r := TimeIt(@Prob.Part1);
        if not Opt.Quiet then
          PrintRow(Day, 1, r.Value, r.Ms);
      end;
    end;

    if Opt.Part in [0, 2] then
    begin
      if Opt.Bench > 0 then
      begin
        AccTime := 0;
        Ans := '';
        for i := 1 to Opt.Bench do
        begin
          r := TimeIt(@Prob.Part2);
          AccTime += r.Ms;
          if i = 1 then
            Ans := r.Value;
        end;
        if not Opt.Quiet then
          PrintRow(Day, 2, Ans, AccTime);
      end
      else
      begin
        r := TimeIt(@Prob.Part2);
        if not Opt.Quiet then
          PrintRow(Day, 2, r.Value, r.Ms);
      end;
    end;
  finally
    Prob.Free;
  end;
  Result := 0;
end;

function RunAll(const Opt: TOptions): integer;
var
  i, j: integer;
  tmp: TDayEntry;
begin
  // bubble sort days
  for i := 0 to High(Registry) do
    for j := i + 1 to High(Registry) do
      if Registry[j].Day < Registry[i].Day then
      begin
        tmp := Registry[i];
        Registry[i] := Registry[j];
        Registry[j] := tmp;
      end;

  for i := 0 to High(Registry) do
    RunSingleDay(Registry[i].Day, Opt);
  Result := 0;
end;

function RunAOC(const Opt: TOptions): integer;
begin
  if not Opt.Quiet then
    PrintHeader;

  if Opt.Day = 0 then
    Result := RunAll(Opt)
  else
    Result := RunSingleDay(Opt.Day, Opt);
end;

end.
