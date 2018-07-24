unit threads;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, fphttpclient, threadqueue, fgl, Forms;

type

  TDownloadCallback = procedure(Sender: TObject; ASucc: Boolean; AFileName: string) of object;
  TDownloadProgressCallback = procedure (Sender: TObject; AProgress: Integer; ACount: Integer) of object;
  { TDownloadThread }

  TDownloadThread = class(TThread)
  private
    FCallback: TDownloadCallback;
    FProgressCallback: TDownloadProgressCallback;
    FUrl: string;
    FLocalFile: string;
    FSucc: Boolean;
    FProgress: Integer;
    FCount: Integer;
    procedure onDownloadData(Sender: TObject; const ContentLength,
      CurrentPos: Int64);
    procedure onThreadTerminated(Sender: TObject);
    procedure callbackProgress();
  protected
    procedure Execute; override;
  public
    constructor Create(AUrl: string; ALocalFile: string);
  public
    property Callback: TDownloadCallback read FCallback write FCallback;
    property ProgressCallback : TDownloadProgressCallback read FProgressCallback write FProgressCallback;
  end;

  THttpCallback = procedure(Sender: TObject; ACode: Integer; AResult: string; AError: string) of object;
  THttpMethod = (hmGet, hmPost);
  THttpParamMap = specialize TFPGMap<String, String>;

  { THttpThread }

  THttpThread = class(TThread)
  private
    FCode: Integer;
    FResult: string;
    FError: string;
    FCallback: THttpCallback;
    FFileParam: THttpParamMap;
    FGetParam: string;
    FHeader: TStringList;
    FPostParam: THttpParamMap;
    FUrl: string;
    FMethod: THttpMethod;
    procedure onThreadTerminated(Sender: TObject);
  protected
    procedure Execute; override;
  public
    constructor Create(AUrl: string; AMethod: THttpMethod = hmGet);
  public
    property GetParam: string read FGetParam write FGetParam;
    property PostParam: THttpParamMap read FPostParam write FPostParam;
    property FileParam: THttpParamMap read FFileParam write FFileParam;
    property Header: TStringList read FHeader write FHeader;
    property Callback: THttpCallback read FCallback write FCallback;
  end;

implementation

Const
  CRLF = #13#10;

{ THttpThread }

procedure THttpThread.onThreadTerminated(Sender: TObject);
begin
  if (Assigned(FCallback)) then begin
    FCallback(Self, FCode, FResult, FError);
  end;
end;

procedure THttpThread.Execute;
var
  s: string;
  geturl: string;
  sep: string;
  ss: TStringStream;
  i: Integer;
  fs: TFileStream;
begin
  // execute
  with TFPHTTPClient.Create(nil) do begin
    AllowRedirect:= True;
    if (FHeader.Count > 0) then for s in FHeader do Header.Add(s);
    case FMethod of
    hmGet:
      begin
        geturl := FUrl;
        if (FGetParam.Trim <> '') then geturl += '?' + FGetParam;
        try
          FResult:= Get(geturl);
        except
          on E: Exception do FError:= E.Message;
        end;
        FCode:= ResponseStatusCode;
      end;
    hmPost:
      begin
        sep:=Format('%.8x_multipart_boundary',[Random($ffffff)]);
        AddHeader('Content-Type','multipart/form-data; boundary=' + sep);
        ss := TStringStream.Create('');
        for i := 0 to FPostParam.Count - 1 do begin
          s := '--'+ sep + CRLF;
          s += Format('Content-Disposition: form-data; name="%s"' + CRLF + CRLF + '%s' + CRLF, [FPostParam.Keys[i], FPostParam.Data[i]]);
          ss.WriteBuffer(s[1],Length(s));
        end;

        for i := 0 to FFileParam.Count - 1 do begin
          s := '--' + sep + CRLF;
          s += Format('Content-Disposition: form-data; name="%s"; filename="%s"' + CRLF,[FFileParam.Keys[i],ExtractFileName(FFileParam.Data[i])]);
          s += 'Content-Type: application/octet-stream' + CRLF + CRLF;
          ss.WriteBuffer(s[1],Length(s));
          fs := TFileStream.Create(FFileParam.Data[i], fmOpenRead or fmShareDenyWrite);
          fs.Seek(0, soFromBeginning);
          ss.CopyFrom(fs, fs.Size);
          s := CRLF;
          ss.WriteBuffer(s[1], Length(s));
          fs.Free;
        end;
        s := CRLF + '--' + Sep + '--' + CRLF;
        ss.WriteBuffer(s[1], Length(s));
        ss.Position:= 0;
        RequestBody:= ss;
        try
          try
            FResult := Post(FUrl);
          except
            on E: Exception do FError:= E.Message;
          end;
          FCode:= ResponseStatusCode;
        finally
          RequestBody:=Nil;
          ss.Free;
        end;
      end;
    end;
    Free;
  end;
end;

constructor THttpThread.Create(AUrl: string; AMethod: THttpMethod);
begin
  inherited Create(True);
  FUrl:= AUrl;
  FMethod:= AMethod;
  FCode:= -1;
  FResult:= '';
  FError:= '';
  FGetParam:= '';
  FPostParam := THttpParamMap.Create;
  FFileParam := THttpParamMap.Create;
  FHeader := TStringList.Create;
  FreeOnTerminate:= True;
  OnTerminate:=@onThreadTerminated;
end;

{ TDownloadThread }

procedure TDownloadThread.onThreadTerminated(Sender: TObject);
begin
  if (Assigned(FCallback)) then begin
    FCallback(Self, FSucc, FLocalFile);
  end;
end;

procedure TDownloadThread.callbackProgress();
begin
  if (Assigned(FProgressCallback)) then begin
    FProgressCallback(Self, FProgress, FCount);
    Application.ProcessMessages;
  end;
end;

procedure TDownloadThread.onDownloadData(Sender: TObject; const ContentLength,
  CurrentPos: Int64);
begin
  if (Assigned(FProgressCallback)) then begin
    FProgress:= CurrentPos;
    FCount:= ContentLength;
    Synchronize(@callbackProgress);
  end;
end;

procedure TDownloadThread.Execute;
var
  path: string;
begin
  path := ExtractFilePath(FLocalFile);
  if (not DirectoryExists(path)) then begin
    ForceDirectories(path);
  end;
  with TFPHTTPClient.Create(nil) do begin
    AllowRedirect:= True;
    OnDataReceived:=@onDownloadData;
    try
      Get(FUrl, FLocalFile);
      FSucc:= True;
    except
      FSucc:= False;
    end;
    Free;
  end;
end;

constructor TDownloadThread.Create(AUrl: string; ALocalFile: string);
begin
  inherited Create(True);
  FUrl:= AUrl;
  FLocalFile:= ALocalFile;
  FSucc:= False;
  FreeOnTerminate:= True;
  OnTerminate:= @onThreadTerminated;
end;

end.

