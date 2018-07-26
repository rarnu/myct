unit configutils;

{$mode objfpc}{$H+}
{$modeswitch typehelpers}

interface

uses
  Classes, SysUtils, IniFiles;

type

  { TObjectHelper }

  TObjectHelper = type Helper for TObject
  public
    function readStringConfig(AKey: string; ADef: string): string;
    function readIntConfig(AKey: string; ADef: Integer): Integer;
    function readInt64Config(AKey: string; ADef: Int64): Int64;
    function readFloatConfig(AKey: string; ADef: Extended): Extended;
    function readBooleanConfig(AKey: string; ADef: Boolean): Boolean;
    procedure writeStringConfig(AKey: string; AValue: string);
    procedure writeIntConfig(Akey: string; AValue: Integer);
    procedure writeInt64Config(AKey: string; AValue: Int64);
    procedure writeFloatConfig(AKey: string; AValue: Extended);
    procedure writeBooleanConfig(AKey: string; AValue: Boolean);
  end;

implementation

{ TObjectHelper }

const
  SEC_CONFIG = 'Config';

function initCfg(): TIniFile;
begin
  Exit(TIniFile.Create(ChangeFileExt(ParamStr(0), '.cfg')));
end;

function TObjectHelper.readStringConfig(AKey: string; ADef: string): string;
var
  c: TIniFile;
begin
  c := initCfg();
  Result := c.ReadString(SEC_CONFIG, AKey, ADef);
  c.Free;
end;

function TObjectHelper.readIntConfig(AKey: string; ADef: Integer): Integer;
var
  c: TIniFile;
begin
  c := initCfg();
  Result := c.ReadInteger(SEC_CONFIG, AKey, ADef);
  c.Free;
end;

function TObjectHelper.readInt64Config(AKey: string; ADef: Int64): Int64;
var
  c: TIniFile;
begin
  c := initCfg();
  Result := c.ReadInt64(SEC_CONFIG, AKey, ADef);
  c.Free;
end;

function TObjectHelper.readFloatConfig(AKey: string; ADef: Extended): Extended;
var
  c: TIniFile;
begin
  c := initCfg();
  Result := c.ReadFloat(SEC_CONFIG, AKey, ADef);
  c.Free;
end;

function TObjectHelper.readBooleanConfig(AKey: string; ADef: Boolean): Boolean;
var
  c: TIniFile;
begin
  c := initCfg();
  Result := c.ReadBool(SEC_CONFIG, AKey, ADef);
  c.Free;
end;

procedure TObjectHelper.writeStringConfig(AKey: string; AValue: string);
var
  c: TIniFile;
begin
  c := initCfg();
  c.WriteString(SEC_CONFIG, AKey, AValue);
  c.Free;
end;

procedure TObjectHelper.writeIntConfig(Akey: string; AValue: Integer);
var
  c: TIniFile;
begin
  c := initCfg();
  c.WriteInteger(SEC_CONFIG, Akey, AValue);
  c.Free;
end;

procedure TObjectHelper.writeInt64Config(AKey: string; AValue: Int64);
var
  c: TIniFile;
begin
  c := initCfg();
  c.WriteInt64(SEC_CONFIG, AKey, AValue);
  c.Free;
end;

procedure TObjectHelper.writeFloatConfig(AKey: string; AValue: Extended);
var
  c: TIniFile;
begin
  c := initCfg();
  c.WriteFloat(SEC_CONFIG, AKey, AValue);
  c.Free;
end;

procedure TObjectHelper.writeBooleanConfig(AKey: string; AValue: Boolean);
var
  c: TIniFile;
begin
  c := initCfg();
  c.WriteBool(SEC_CONFIG, AKey, AValue);
  c.Free;
end;

end.

