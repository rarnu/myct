unit dlgAlert;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, frmBase, orca_scene2d;

type
  TAlertMode = (amMessage, amConfirm, amSelect, amInput);

  { TDialogAlert }

  TDialogAlert = class(TFormBase)
    procedure FormCreate(Sender: TObject);
  private
    FBtnCancelTitle: string;
    FBtnMiddleTitle: string;
    FBtnOkTitle: string;
    FButtonCount: Integer;
    FlblMessage: TD2Text;
    FMessageText: string;
    FShowEdit: Boolean;
    FTitle: string;
    FtxtInput: TD2HudTextBox;
    FbtnOK: TD2HudCornerButton;
    FbtnCancel: TD2HudCornerButton;
    FBtnMiddle: TD2HudCornerButton;
    FMode: TAlertMode;
    function GetInputHint: string;
    function GetInputText: string;
    procedure onBtnCancelClicked(Sender: TObject);
    procedure onBtnMiddleClicked(Sender: TObject);
    procedure onBtnOkClicked(Sender: TObject);
    procedure SetBtnCancelTitle(AValue: string);
    procedure SetBtnMiddleTitle(AValue: string);
    procedure SetBtnOkTitle(AValue: string);
    procedure SetButtonCount(AValue: Integer);
    procedure SetInputHint(AValue: string);
    procedure SetInputText(AValue: string);
    procedure SetMessageText(AValue: string);
    procedure SetMode(AValue: TAlertMode);
    procedure SetShowEdit(AValue: Boolean);
    procedure SetTitle(AValue: string);
  public
    property Mode: TAlertMode read FMode write SetMode;
    property Title: string read FTitle write SetTitle;
    property MessageText: string read FMessageText write SetMessageText;
    property ShowEdit: Boolean read FShowEdit write SetShowEdit;
    property ButtonCount: Integer read FButtonCount write SetButtonCount;
    property BtnOkTitle: string read FBtnOkTitle write SetBtnOkTitle;
    property BtnCancelTitle: string read FBtnCancelTitle write SetBtnCancelTitle;
    property BtnMiddleTitle: string read FBtnMiddleTitle write SetBtnMiddleTitle;
    property InputText: string read GetInputText write SetInputText;
    property InputHint: string read GetInputHint write SetInputHint;
  end;

var
  DialogAlert: TDialogAlert;

implementation

uses
  orcautils;

{$R *.frm}

{ TDialogAlert }

procedure TDialogAlert.FormCreate(Sender: TObject);
var
  layBtn: TD2Layout;
  layMsg: TD2Layout;
begin
  inherited;
  Width:= 300;
  Height:= 150;

  layBtn := TD2Layout(createD2VO(Root, TD2Layout, 0, 0, 0, 32, vaBottom, 8, 8, 8, 8));
  layMsg := TD2Layout(createD2VO(Root, TD2Layout, 0, 0, 0, 0, vaClient, 8, 8, 8, 8));

  FlblMessage:= TD2Text(createD2VO(layMsg, TD2Text, 0, 0, 0, 0, vaClient));
  FlblMessage.setup('', vcWhite, d2TextAlignNear, d2TextAlignCenter, True);
  FtxtInput := TD2HudTextBox(createD2VO(layMsg, TD2HudTextBox, 0, 0, 0, 32, vaBottom));

  FbtnOK:= TD2HudCornerButton(createD2VO(layBtn, TD2HudCornerButton, 0, 0, 60, 32, vaMostRight));
  FBtnMiddle:= TD2HudCornerButton(createD2VO(layBtn, TD2HudCornerButton, Width - 200, 0, 60, 32, vaRight));
  FbtnCancel:= TD2HudCornerButton(createD2VO(layBtn, TD2HudCornerButton, Width - 400, 0, 60, 32, vaRight));

  FbtnOK.OnClick:=@onBtnOkClicked;
  FbtnCancel.OnClick:=@onBtnCancelClicked;
  FBtnMiddle.OnClick:=@onBtnMiddleClicked;
end;

procedure TDialogAlert.onBtnOkClicked(Sender: TObject);
begin
  ModalResult:= mrOK;
end;

procedure TDialogAlert.SetBtnCancelTitle(AValue: string);
begin
  FBtnCancelTitle:=AValue;
  FbtnCancel.Text:= FBtnCancelTitle;
end;

procedure TDialogAlert.SetBtnMiddleTitle(AValue: string);
begin
  FBtnMiddleTitle:=AValue;
  FBtnMiddle.Text:= FBtnMiddleTitle;
end;

procedure TDialogAlert.SetBtnOkTitle(AValue: string);
begin
  FBtnOkTitle:=AValue;
  FbtnOK.Text:= FBtnOkTitle;
end;

procedure TDialogAlert.SetButtonCount(AValue: Integer);
begin
  FButtonCount:=AValue;
  case FButtonCount of
  1:
    begin
      FBtnMiddle.Visible:= False;
      FbtnCancel.Visible:= False;
      FbtnOK.Corners:= [d2CornerTopLeft, d2CornerTopRight, d2CornerBottomLeft, d2CornerBottomRight];
    end;
  2:
    begin
      FBtnMiddle.Visible:= False;
      FbtnOK.Corners:= [d2CornerTopRight, d2CornerBottomRight];
      FbtnCancel.Corners:= [d2CornerTopLeft, d2CornerBottomLeft];
    end;
  3:
    begin
      FbtnOK.Corners:= [d2CornerTopRight, d2CornerBottomRight];
      FBtnMiddle.Corners:= [];
      FbtnCancel.Corners:= [d2CornerTopLeft, d2CornerBottomLeft];
    end;
  end;
end;

procedure TDialogAlert.SetInputHint(AValue: string);
begin
  FtxtInput.Hint:= AValue;
  FtxtInput.ShowHint:= AValue.Trim <> '';
end;

procedure TDialogAlert.SetInputText(AValue: string);
begin
  FtxtInput.Text:= AValue;
end;

procedure TDialogAlert.SetMessageText(AValue: string);
begin
  FMessageText:=AValue;
  FlblMessage.Text:= AValue;
  if (FMessageText = '') then begin
    Height:= Height - 32;
  end;
end;

procedure TDialogAlert.SetMode(AValue: TAlertMode);
begin
  FMode:=AValue;
  case FMode of
  amMessage:
    begin

    end;
  amConfirm:
    begin

    end;
  amSelect:
    begin

    end;
  amInput:
    begin

    end;
  end;
end;

procedure TDialogAlert.SetShowEdit(AValue: Boolean);
begin
  FShowEdit:=AValue;
  FtxtInput.Visible:= FShowEdit;
  if (FShowEdit) then begin
    Height:= Height + 32;
  end;
end;

procedure TDialogAlert.SetTitle(AValue: string);
begin
  FTitle:=AValue;
  Window.Text:= FTitle;
end;

procedure TDialogAlert.onBtnCancelClicked(Sender: TObject);
begin
  ModalResult:= mrCancel;
end;

function TDialogAlert.GetInputHint: string;
begin
  Exit(FtxtInput.Hint);
end;

function TDialogAlert.GetInputText: string;
begin
  Exit(FtxtInput.Text);
end;

procedure TDialogAlert.onBtnMiddleClicked(Sender: TObject);
begin
  ModalResult:= mrYes;
end;

end.

