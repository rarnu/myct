unit alertutils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Forms, dlgAlert;

type
  TAlertEditCallback = procedure(AAccept: Boolean; AText: string) of object;

procedure alert(ATitle: string; AMessage: string; ABtnTitle: string);
function alert(ATitle: string; AMessage: string; ABtnLeftTitle: string; ABtnRightTitle: string): Integer;
function alert(ATitle: string; AMessage: string; ABtnLeftTitle: string; ABtnMiddleTitle: string; ABtnRightTitle: string): Integer;
procedure alert(ATitle: string; AMessage: string; AText: string; AHint: string; ABtnLeftTitle: string; ABtnRightTitle: string; ACallback: TAlertEditCallback);

implementation

procedure alert(ATitle: string; AMessage: string; ABtnTitle: string);
begin
  with TDialogAlert.Create(nil) do begin
    ShowEdit:= False;
    ButtonCount:= 1;
    BtnOkTitle:= ABtnTitle;
    Title:= ATitle;
    MessageText:= AMessage;
    ShowModal;
    Free;
  end;
end;

function alert(ATitle: string; AMessage: string; ABtnLeftTitle: string;
  ABtnRightTitle: string): Integer;
var
  m: TModalResult;
begin
  with TDialogAlert.Create(nil) do begin
    ShowEdit:= False;
    ButtonCount:= 2;
    BtnOkTitle:= ABtnRightTitle;
    BtnCancelTitle:= ABtnLeftTitle;
    Title:= ATitle;
    MessageText:= AMessage;
    m := ShowModal;
    case m of
    mrOK: Result := 0;
    else
      Result := 1;
    end;
    Free;
  end;
end;

function alert(ATitle: string; AMessage: string; ABtnLeftTitle: string;
  ABtnMiddleTitle: string; ABtnRightTitle: string): Integer;
var
  m: TModalResult;
begin
  with TDialogAlert.Create(nil) do begin
    ShowEdit:= False;
    ButtonCount:= 3;
    BtnOkTitle:= ABtnRightTitle;
    BtnMiddleTitle:= ABtnMiddleTitle;
    BtnCancelTitle:= ABtnLeftTitle;
    Title:= ATitle;
    MessageText:= AMessage;
    m := ShowModal;
    case m of
    mrOK: Result := 0;
    mrYes: Result := 1;
    else
      Result := 2;
    end;
    Free;
  end;
end;

procedure alert(ATitle: string; AMessage: string; AText: string; AHint: string;
  ABtnLeftTitle: string; ABtnRightTitle: string; ACallback: TAlertEditCallback);
var
  m: TModalResult;
begin
  with TDialogAlert.Create(nil) do begin
    ShowEdit:= True;
    ButtonCount:= 2;
    BtnOkTitle:= ABtnRightTitle;
    BtnCancelTitle:= ABtnLeftTitle;
    Title:= ATitle;
    MessageText:= AMessage;
    InputText:= AText;
    InputHint:= AHint;
    m := ShowModal;
    if (Assigned(ACallback)) then begin
      case m of
      mrOK: ACallback(True, InputText);
      else
        ACallback(False, InputText);
      end;
    end;
    Free;
  end;
end;

end.

