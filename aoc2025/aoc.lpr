program aoc;

{$mode objfpc}{$H+}

uses
  SysUtils,
  Args,
  Runner,
  day01;

var
  Opt: TOptions;
  ExitCode: integer;
begin
  try
    Opt := ParseOptions;
    ExitCode := RunAOC(Opt);
    Halt(ExitCode);
  except
    on E: Exception do
    begin
      Writeln('Fatal: ', E.ClassName, ': ', E.Message);
      Halt(1);
    end;
  end;
end.
