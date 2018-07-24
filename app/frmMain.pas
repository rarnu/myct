unit frmMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, frmBase, orca_scene2d, threads;

type

  { TFormMain }

  TFormMain = class(TFormBase)
    procedure FormCreate(Sender: TObject);
  private
    FBtn: TD2HudButton;
    procedure onHttpCallback(Sender: TObject; ACode: Integer; AResult: string;
      AError: string);
  public

  end;

var
  FormMain: TFormMain;

implementation

uses
  orcautils;

{$R *.frm}

{ TFormMain }

procedure TFormMain.FormCreate(Sender: TObject);
var
  h: Single;
begin
  inherited;
  FBtn := TD2HudButton(createD2VO(Root, TD2HudButton, 8, 8, 60, 40, vaTop));
  with THttpThread.Create('https://www.hujiang.com') do begin
    Callback:=@onHttpCallback;
    Start;
  end;
end;

procedure TFormMain.onHttpCallback(Sender: TObject; ACode: Integer;
  AResult: string; AError: string);
begin
  WriteLn(Format('code: %d, text: %s, error: %s', [ACode, AResult, AError]));
end;

end.

