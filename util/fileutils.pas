unit fileutils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

function fileToStream(AFilePath: string): TStream;
function fileToText(AFilePath: string): string;

function streamToFile(AStream: TStream; AFilePath: string): Boolean;
function streamToText(AStream: TStream): string;

function textToStream(txt: string): TStream;
function textToFile(txt: string; AFilePath: string): Boolean;

implementation

function fileToStream(AFilePath: string): TStream;
begin
  Exit(TFileStream.Create(AFilePath, fmOpenRead or fmShareDenyWrite));
end;

function fileToText(AFilePath: string): string;
begin
  with TStringList.Create do begin
    LoadFromFile(AFilePath);
    Result := Text;
    Free;
  end;
end;

function streamToFile(AStream: TStream; AFilePath: string): Boolean;
begin
  with TMemoryStream.Create do begin
    AStream.Seek(0, soFromBeginning);
    CopyFrom(AStream, AStream.Size);
    try
      SaveToFile(AFilePath);
      Result := True;
    except
      Result := False;
    end;
    Free;
  end;
end;

function streamToText(AStream: TStream): string;
begin
  with TStringList.Create do begin
    LoadFromStream(AStream);
    Result := Text;
    Free;
  end;
end;

function textToStream(txt: string): TStream;
begin
  Exit(TStringStream.Create(txt));
end;

function textToFile(txt: string; AFilePath: string): Boolean;
begin
  with TStringList.Create do begin
    Text:= txt;
    try
      SaveToFile(AFilePath);
      Result := True;
    except
      Result := False;
    end;
    Free;
  end;
end;

end.

