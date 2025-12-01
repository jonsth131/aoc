unit uTestUtils;

{$mode objfpc}{$H+}

interface

function LoadFileAsString(const FileName: string): string;

implementation

uses
  SysUtils, Classes;

function LoadFileAsString(const FileName: string): string;
var
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    SL.LoadFromFile(FileName);
    Result := SL.Text;
  finally
    SL.Free;
  end;
end;

end.
