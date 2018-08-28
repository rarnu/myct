{$I debug.inc}

{.$DEFINE FGL}

unit sqliteData;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqlite3conn, sqldb, db, worddb{$IFDEF FGL}, fgl{$ENDIF}{$IFDEF ANDROID}, android{$ENDIF};

type
  { TSQLite }

  TSQLite = class
  private
    FDatabase: TSQLite3Connection;
    FHasFullDict: Boolean;
    FQuery: TSQLQuery;
    FTransaction: TSQLTransaction;
    procedure _HasFullDict();
  public
    constructor Create;
    destructor Destroy; override;
    function Open(APath: string; var AError: string): Boolean;
    function Close(APath: string; var AError: string): Boolean;
    function ExecuteSQL(ASQL: string; var AError: string): Boolean; overload;
    function ExecuteSQL(ASQL: string; AParam: array of const; var AError: string): Boolean; overload;
    function Insert(ASQL: string; var AError: string): Boolean; overload;
    function Insert(ASQL: string; AParam: array of const; var AError: string): Boolean; overload;
    function Update(ASQL: string; var AError: string): Boolean; overload;
    function Update(ASQL: string; AParam: array of const; var AError: string): Boolean; overload;
    function Delete(ASQL: string; var AError: string): Boolean; overload;
    function Delete(ASQL: string; AParam: array of const; var AError: string): Boolean; overload;
    function Select(ASQL: string; var AError: string): Boolean; overload;
    function Select(ASQL: string; AParam: array of const; var AError: string): Boolean; overload;
    function FindId(ASQL: string): Int64;

    function Minimize(): Boolean;

    // database operation
    function MergeDatabase(APath: string): Boolean;
    // function AppendDatabase(APath: string; ADictId: Integer): Boolean;
    // function UpdateDatabase(APath: string; AFieldAppend: Boolean; ASkipEmpty: Boolean): Boolean;

    // freq operation
    function exportUserFreq(ASaveFilePath: string): Boolean;
    function importUserFreq(AFreqFilePath: string): Boolean;

    // performance test
    {$IFDEF PERFOEMANCE_TEST}
    procedure ImportData(AStringList: TStringList);
    {$ENDIF}
  published
    property Database: TSQLite3Connection read FDatabase write FDatabase;
    property Transaction: TSQLTransaction read FTransaction write FTransaction;
    property Query: TSQLQuery read FQuery write FQuery;
    property HasFullDict: Boolean read FHasFullDict write FHasFullDict;
  end;

  {$IFDEF FGL}
  TDatabaseList = specialize TFPGMap<string, TSQLite>;
  {$ENDIF}

var
  dbs: {$IFDEF FGL}TDatabaseList{$ELSE}TStringList{$ENDIF};
  lastError: string = '';

function DatabaseExists(APath: string): Boolean;
function GetDatabase(APath: string): TSQLite;

implementation

uses
  builder, constData, strutils, math;

const
  ERROR_COMMAND = 'Command error';

function DatabaseExists(APath: string): Boolean;
begin
  Result := dbs.IndexOf(APath) <> -1;
end;

function GetDatabase(APath: string): TSQLite;
begin
  Result := nil;
  if (DatabaseExists(APath)) then begin
    {$IFDEF FGL}
    Result := dbs.KeyData[APath];
    {$ELSE}
    Result := TSQLite(dbs.Objects[dbs.IndexOf(APath)]);
    {$ENDIF}
  end;
end;

{ TSQLite }

procedure TSQLite._HasFullDict;
begin
  FQuery.Close;
  FQuery.SQL.Text:= 'SELECT dict_id FROM offlineword WHERE dict_id = 1 LIMIT 0, 1';
  FQuery.Open;
  FHasFullDict:= FQuery.RecordCount > 0;
  FQuery.Close;
end;

constructor TSQLite.Create;
begin
  FDatabase := TSQLite3Connection.Create(nil);
  FDatabase.CharSet:= 'utf-8';
  FTransaction := TSQLTransaction.Create(nil);
  FQuery := TSQLQuery.Create(nil);
  FQuery.Options:= [sqoKeepOpenOnCommit, sqoAutoCommit];
  FDatabase.Transaction := FTransaction;
  FQuery.Transaction := FTransaction;
  FQuery.DataBase := FDatabase;
end;

destructor TSQLite.Destroy;
begin
  FQuery.Transaction := nil;
  FDatabase.Transaction := nil;
  FTransaction.Free;
  FQuery.Free;
  FDatabase.Free;
  inherited Destroy;
end;

function TSQLite.Open(APath: string; var AError: string): Boolean;
begin
  {$IFDEF ANDROID}
  LOGE(PChar('Open Database: ' + APath));
  {$ELSE}
  WriteLn('Open Database: ' + APath);
  {$ENDIF}
  FDatabase.DatabaseName:= APath;
  try
    FDatabase.Open;
    {$IFDEF ANDROID}
    LOGE(PChar('Opened Database: ' + APath));
    {$ELSE}
    WriteLn('Opened Database: ' + APath);
    {$ENDIF}
    _HasFullDict();
    {$IFDEF ANDROID}
    LOGE(PChar('Check Full: ' + APath));
    {$ELSE}
    WriteLn('Check Full: ' + APath);
    {$ENDIF}
    lastError := '';
    Exit(True);
  except
    on E: Exception do begin
      AError:= e.Message;
      lastError := AError;
      Exit(False);
    end;
  end;
end;

function TSQLite.Close(APath: string; var AError: string): Boolean;
begin
  try
    FDatabase.Close(True);
    lastError := '';
    Exit(True);
  except
    on E: Exception do begin
      AError:= e.Message;
      lastError := AError;
      Exit(False);
    end;
  end;
end;

function TSQLite.ExecuteSQL(ASQL: string; var AError: string): Boolean;
begin
  FQuery.Close;
  FQuery.SQL.Text:= ASQL;
  try
    FQuery.ExecSQL;
    lastError := '';
    Exit(True);
  except
    on E: Exception do begin
      AError:= e.Message;
      lastError := AError + Format('[%s]', [ASQL]);
      Exit(False);
    end;
  end;
end;

function TSQLite.ExecuteSQL(ASQL: string; AParam: array of const;
  var AError: string): Boolean;
begin
  Exit(ExecuteSQL(Format(ASQL, AParam), AError));
end;

function TSQLite.Insert(ASQL: string; var AError: string): Boolean;
begin
  if (not ASQL.ToLower.StartsWith('insert')) then begin
    AError:= ERROR_COMMAND;
    lastError := AError + Format('[%s]', [ASQL]);
    Exit(False);
  end;
  Exit(ExecuteSQL(ASQL, AError));
end;

function TSQLite.Insert(ASQL: string; AParam: array of const; var AError: string
  ): Boolean;
begin
  Exit(Insert(Format(ASQL, AParam), AError));
end;

function TSQLite.Update(ASQL: string; var AError: string): Boolean;
begin
  if (not ASQL.ToLower.StartsWith('update')) then begin
    AError:= ERROR_COMMAND;
    lastError := AError + Format('[%s]', [ASQL]);
    Exit(False);
  end;
  Exit(ExecuteSQL(ASQL, AError));
end;

function TSQLite.Update(ASQL: string; AParam: array of const; var AError: string
  ): Boolean;
begin
  Exit(Update(Format(ASQL, AParam), AError));
end;

function TSQLite.Delete(ASQL: string; var AError: string): Boolean;
begin
  if (not ASQL.ToLower.StartsWith('delete')) then begin
    AError:= ERROR_COMMAND;
    lastError := AError + Format('[%s]', [ASQL]);
    Exit(False);
  end;
  Exit(ExecuteSQL(ASQL, AError));
end;

function TSQLite.Delete(ASQL: string; AParam: array of const; var AError: string
  ): Boolean;
begin
  Exit(Delete(Format(ASQL, AParam), AError))
end;

function TSQLite.Select(ASQL: string; var AError: string): Boolean;
begin
  if (not ASQL.ToLower.StartsWith('select')) then begin
    AError:= ERROR_COMMAND;
    Exit(False);
  end;
  try
    FQuery.Active:= False;
    FQuery.SQL.Text:= ASQL;
    FQuery.Active:= True;
    lastError := '';
    Exit(True);
  Except
    on E: Exception do begin
      AError:= e.Message;
      lastError := AError + Format('[%s]', [ASQL]);
      Exit(False);
    end;
  end;
end;

function TSQLite.Select(ASQL: string; AParam: array of const; var AError: string
  ): Boolean;
begin
  Exit(Select(Format(ASQL, AParam), AError));
end;

function TSQLite.FindId(ASQL: string): Int64;
begin
  FQuery.Close;
  FQuery.SQL.Text:= ASQL;
  try
    FQuery.Open;
    if (FQuery.RecordCount = 0) then Exit(-1);
    lastError := '';
    FQuery.First;
    Exit(FQuery.FieldByName(TWordDB.FieldID.Name).AsLargeInt);
  except
    on E: Exception do begin
      lastError := e.Message + Format('[%s]', [ASQL]);
      Exit(-1);
    end;
  end;
end;

function TSQLite.Minimize: Boolean;
begin
  FQuery.Close;
  try
    FDatabase.ExecuteDirect('End Transaction');
    FDatabase.ExecuteDirect('Vacuum');
    FDatabase.ExecuteDirect('Begin Transaction');
    Exit(True);
  except
    on E: Exception do begin
      lastError := e.Message;
      Exit(False);
    end;
  end;
end;

function TSQLite.MergeDatabase(APath: string): Boolean;
var
  hasIndex: Boolean = False;
  needDrop: Boolean = False;
  tmpSQL: string;
begin
  FQuery.Close;
  try
    // make index
    FQuery.Close;
    // index|index_word_word_ext|offlineword|731|CREATE UNIQUE INDEX index_word_word_ext on offlineword(word, word_ext)
    FQuery.SQL.Text:='select * from sqlite_master where name=''idx_word_wordext''';
    FQuery.Open;
    FQuery.First;
    if (FQuery.RecordCount > 0) then hasIndex:= True;
    if (hasIndex) then begin
      tmpSQL:= FQuery.FieldByName('sql').AsString;
      needDrop:= (not tmpSQL.Contains('word,')) or (not tmpSQL.Contains('word_ext,')) or (not tmpSQL.Contains('dict_id'));
    end;
    if (needDrop) then begin
      FQuery.Close;
      FQuery.SQL.Text:= 'drop index idx_word_wordext';
      FQuery.ExecSQL;
      hasIndex:= False;
    end;
    if (not hasIndex) then begin
      FQuery.Close;
      FQuery.SQL.Text:= 'CREATE UNIQUE INDEX idx_word_wordext on offlineword(word, word_ext, dict_id)';
      FQuery.ExecSQL;
    end;
    FQuery.Close;
    FDatabase.ExecuteDirect('End Transaction');
    FDatabase.ExecuteDirect(Format('ATTACH DATABASE ''%s'' as new', [APath]));
    FDatabase.ExecuteDirect('Begin Transaction');
    FQuery.Options:= [sqoAutoApplyUpdates, sqoAutoCommit];
    FQuery.SQL.Text:= 'REPLACE INTO offlineword(word, explan, content, word_ext, kana, ipa, freq_common, freq_relative, dict_id, version, status, data1, data2, data3, data4, data5) SELECT word, explan, content, word_ext, kana, ipa, freq_common, freq_relative, dict_id, version, status, data1, data2, data3, data4, data5 FROM new.offlineword';
    FQuery.ExecSQL;
    FQuery.Close;
    FQuery.Options:= [sqoKeepOpenOnCommit, sqoAutoCommit];
    _HasFullDict();
    // detach
    FQuery.Close;
    try
      FDatabase.ExecuteDirect('End Transaction');
      FDatabase.ExecuteDirect('DETACH DATABASE new');
      FDatabase.ExecuteDirect('Begin Transaction');
    except
    end;
    Exit(True);
  except
    on E: Exception do begin
      lastError := e.Message;
      Exit(False);
    end;
  end;
end;

(*
function TSQLite.AppendDatabase(APath: string; ADictId: Integer): Boolean;
var
  AError: string;
  dbAdd: TSQLite;
  sql: string;
  cnt: Integer;
  m : Integer;
begin
  Result := True;
  lastError := '';
  // append database
  FQuery.Close;
  FTransaction.Active:= False;
  FQuery.Options:= [sqoKeepOpenOnCommit];
  FTransaction.StartTransaction;
  // ExecSQL
  cnt := 0;
  m := 0;
  dbAdd := TSQLite.Create;
  dbAdd.Open(APath, AError);
  dbAdd.Select(Format('select * from %s order by id asc limit %d, %d', [DBNAME, 0, OptDataBatchSize]), AError);
  dbAdd.Query.First;
  {$IFDEF DEBUG}
  WriteLn(Format('BATCH APPEND => %d', [0]));
  {$ENDIF}
  while (True) do begin
    if (dbAdd.Query.EOF) then Break;
    sql :=  TSQLBuilder.buildInsert(DBNAME,
      [TWordDB.FieldWord, TWordDB.FieldExplan,
      TWordDB.FieldContent, TWordDB.FieldWordExt, TWordDB.FieldKana, TWordDB.FieldIPA,
      TWordDB.FieldFreqCommon, TWordDB.FieldFreqRelative, TWordDB.FieldFreqUser,
      TWordDB.FieldDictId, TWordDB.FieldVersion, TWordDB.FieldStatus,
      TWordDB.FieldData1, TWordDB.FieldData2, TWordDB.FieldData3, TWordDB.FieldData4, TWordDB.FieldData5],
      [dbAdd.Query.FieldByName(TWordDB.FieldWord.Name).AsString, dbAdd.Query.FieldByName(TWordDB.FieldExplan.Name).AsString,
      dbAdd.Query.FieldByName(TWordDB.FieldContent.Name).AsString, dbAdd.Query.FieldByName(TWordDB.FieldWordExt.Name).AsString, dbAdd.Query.FieldByName(TWordDB.FieldKana.Name).AsString, dbAdd.Query.FieldByName(TWordDB.FieldIPA.Name).AsString,
      dbAdd.Query.FieldByName(TWordDB.FieldFreqCommon.Name).AsLargeInt, dbAdd.Query.FieldByName(TWordDB.FieldFreqRelative.Name).AsLargeInt, dbAdd.Query.FieldByName(TWordDB.FieldFreqUser.Name).AsLargeInt,
      dbAdd.Query.FieldByName(TWordDB.FieldDictId.Name).AsInteger, dbAdd.Query.FieldByName(TWordDB.FieldVersion.Name).AsInteger, dbAdd.Query.FieldByName(TWordDB.FieldStatus.Name).AsInteger,
      dbAdd.Query.FieldByName(TWordDB.FieldData1.Name).AsString, dbAdd.Query.FieldByName(TWordDB.FieldData2.Name).AsString, dbAdd.Query.FieldByName(TWordDB.FieldData3.Name).AsString, dbAdd.Query.FieldByName(TWordDB.FieldData4.Name).AsString, dbAdd.Query.FieldByName(TWordDB.FieldData5.Name).AsString]);
    dbAdd.Query.Next;
    ExecuteSQL(sql, AError);
    Inc(cnt);
    if (cnt > OptDataBatchSize - 1) then begin
      Inc(m);
      FTransaction.Commit;
      FTransaction.EndTransaction;
      dbAdd.Select(Format('select * from %s order by id asc limit %d, %d', [DBNAME, m * OptDataBatchSize, OptDataBatchSize]), AError);
      dbAdd.Query.First;
      {$IFDEF DEBUG}
      WriteLn(Format('BATCH APPEND => %d', [m * OptDataBatchSize]));
      {$ENDIF}
      FTransaction.StartTransaction;
      cnt:= 0;
    end;
  end;

  FTransaction.Commit;
  FTransaction.EndTransaction;
  dbAdd.Close(APath, AError);
  dbAdd.Free;
  FQuery.Options:= [sqoKeepOpenOnCommit, sqoAutoCommit];
  _HasFullDict();
  if (lastError.Trim <> '') then Result := False;
end;
*)

(*
function TSQLite.UpdateDatabase(APath: string; AFieldAppend: Boolean;
  ASkipEmpty: Boolean): Boolean;
var
  AError: string;
  dbUpdate: TSQLite;
  sql: string;
  sqlFindId: string;
  id: Int64;
  sqlPool: TStringList;
  i: Integer;
  cnt: Integer;
  m: Integer;
begin
  // TODO: batch
  Result := True;
  lastError := '';
  sqlPool := TStringList.Create;
  // update database
  dbUpdate := TSQLite.Create;
  dbUpdate.Open(APath, AError);
  dbUpdate.Select(Format('select * from %s order by id asc limit %d, %d', [DBNAME, 0, OptDataBatchSize]), AError);
  dbUpdate.Query.First;
  cnt := 0;
  m := 0;

  while (True) do begin
    if (dbUpdate.Query.EOF) then Break;
    sqlFindId:= TSQLBuilder.buildSelect(DBNAME, False, [TWordDB.FieldID], [TWordDB.FieldWord], [stEqual], [dbUpdate.Query.FieldByName(TWordDB.FieldWord.Name).AsString], '', sctAll, 0);
    id := FindId(sqlFindId);
    if (id = -1) then begin
      // insert
      sql :=  TSQLBuilder.buildInsert(DBNAME,
        [TWordDB.FieldWord, TWordDB.FieldExplan,
        TWordDB.FieldContent, TWordDB.FieldWordExt, TWordDB.FieldKana, TWordDB.FieldIPA,
        TWordDB.FieldFreqCommon, TWordDB.FieldFreqRelative, TWordDB.FieldFreqUser,
        TWordDB.FieldDictId, TWordDB.FieldVersion, TWordDB.FieldStatus,
        TWordDB.FieldData1, TWordDB.FieldData2, TWordDB.FieldData3, TWordDB.FieldData4, TWordDB.FieldData5],
        [dbUpdate.Query.FieldByName(TWordDB.FieldWord.Name).AsString, dbUpdate.Query.FieldByName(TWordDB.FieldExplan.Name).AsString,
        dbUpdate.Query.FieldByName(TWordDB.FieldContent.Name).AsString, dbUpdate.Query.FieldByName(TWordDB.FieldWordExt.Name).AsString, dbUpdate.Query.FieldByName(TWordDB.FieldKana.Name).AsString, dbUpdate.Query.FieldByName(TWordDB.FieldIPA.Name).AsString,
        dbUpdate.Query.FieldByName(TWordDB.FieldFreqCommon.Name).AsLargeInt, dbUpdate.Query.FieldByName(TWordDB.FieldFreqRelative.Name).AsLargeInt, dbUpdate.Query.FieldByName(TWordDB.FieldFreqUser.Name).AsLargeInt,
        dbUpdate.Query.FieldByName(TWordDB.FieldDictId.Name).AsInteger, dbUpdate.Query.FieldByName(TWordDB.FieldVersion.Name).AsInteger, dbUpdate.Query.FieldByName(TWordDB.FieldStatus.Name).AsInteger,
        dbUpdate.Query.FieldByName(TWordDB.FieldData1.Name).AsString, dbUpdate.Query.FieldByName(TWordDB.FieldData2.Name).AsString, dbUpdate.Query.FieldByName(TWordDB.FieldData3.Name).AsString, dbUpdate.Query.FieldByName(TWordDB.FieldData4.Name).AsString, dbUpdate.Query.FieldByName(TWordDB.FieldData5.Name).AsString]);
      sqlPool.Add(sql);
    end else begin
      // build update sql
      sql := Format('UPDATE %s SET ', [DBNAME]);
      if (ASkipEmpty) then begin
        if (dbUpdate.Query.FieldByName(TWordDB.FieldExplan.Name).AsString <> '') then sql += Format('%s = %s ''%s'', ', [TWordDB.FieldExplan.Name, ifthen(AFieldAppend, TWordDB.FieldExplan.Name + ' ||', ''), dbUpdate.Query.FieldByName(TWordDB.FieldExplan.Name).AsString]);
        if (dbUpdate.Query.FieldByName(TWordDB.FieldContent.Name).AsString <> '') then sql += Format('%s = %s ''%s'', ', [TWordDB.FieldContent.Name, ifthen(AFieldAppend, TWordDB.FieldContent.Name + ' ||', ''), dbUpdate.Query.FieldByName(TWordDB.FieldContent.Name).AsString]);
        if (dbUpdate.Query.FieldByName(TWordDB.FieldWordExt.Name).AsString <> '') then sql += Format('%s = %s ''%s'', ', [TWordDB.FieldWordExt.Name, ifthen(AFieldAppend, TWordDB.FieldWordExt.Name + ' ||', ''), dbUpdate.Query.FieldByName(TWordDB.FieldWordExt.Name).AsString]);
        if (dbUpdate.Query.FieldByName(TWordDB.FieldKana.Name).AsString <> '') then sql += Format('%s = %s ''%s'', ', [TWordDB.FieldKana.Name, ifthen(AFieldAppend, TWordDB.FieldKana.Name + ' ||', ''), dbUpdate.Query.FieldByName(TWordDB.FieldKana.Name).AsString]);
        if (dbUpdate.Query.FieldByName(TWordDB.FieldIPA.Name).AsString <> '') then sql += Format('%s = %s ''%s'', ', [TWordDB.FieldIPA.Name, ifthen(AFieldAppend, TWordDB.FieldIPA.Name + ' ||', ''), dbUpdate.Query.FieldByName(TWordDB.FieldIPA.Name).AsString]);
        if (dbUpdate.Query.FieldByName(TWordDB.FieldFreqCommon.Name).AsLargeInt <> 0) then sql += Format('%s = %d, ', [TWordDB.FieldFreqCommon.Name, dbUpdate.Query.FieldByName(TWordDB.FieldFreqCommon.Name).AsLargeInt]);
        if (dbUpdate.Query.FieldByName(TWordDB.FieldFreqRelative.Name).AsLargeInt <> 0) then sql += Format('%s = %d, ', [TWordDB.FieldFreqRelative.Name, dbUpdate.Query.FieldByName(TWordDB.FieldFreqRelative.Name).AsLargeInt]);
        if (dbUpdate.Query.FieldByName(TWordDB.FieldFreqUser.Name).AsLargeInt <> 0) then sql += Format('%s = %d, ', [TWordDB.FieldFreqUser.Name, dbUpdate.Query.FieldByName(TWordDB.FieldFreqUser.Name).AsLargeInt]);
        if (dbUpdate.Query.FieldByName(TWordDB.FieldDictId.Name).AsInteger <> 0) then sql += Format('%s = %d, ', [TWordDB.FieldDictId.Name, dbUpdate.Query.FieldByName(TWordDB.FieldDictId.Name).AsInteger]);
        if (dbUpdate.Query.FieldByName(TWordDB.FieldVersion.Name).AsInteger <> 0) then sql += Format('%s = %d, ', [TWordDB.FieldVersion.Name, dbUpdate.Query.FieldByName(TWordDB.FieldVersion.Name).AsInteger]);
        if (dbUpdate.Query.FieldByName(TWordDB.FieldStatus.Name).AsInteger <> 0) then sql += Format('%s = %d, ', [TWordDB.FieldStatus.Name, dbUpdate.Query.FieldByName(TWordDB.FieldStatus.Name).AsInteger]);
        if (dbUpdate.Query.FieldByName(TWordDB.FieldData1.Name).AsString <> '') then sql += Format('%s = %s ''%s'', ', [TWordDB.FieldData1.Name, ifthen(AFieldAppend, TWordDB.FieldData1.Name + ' ||', ''), dbUpdate.Query.FieldByName(TWordDB.FieldData1.Name).AsString]);
        if (dbUpdate.Query.FieldByName(TWordDB.FieldData2.Name).AsString <> '') then sql += Format('%s = %s ''%s'', ', [TWordDB.FieldData2.Name, ifthen(AFieldAppend, TWordDB.FieldData2.Name + ' ||', ''), dbUpdate.Query.FieldByName(TWordDB.FieldData2.Name).AsString]);
        if (dbUpdate.Query.FieldByName(TWordDB.FieldData3.Name).AsString <> '') then sql += Format('%s = %s ''%s'', ', [TWordDB.FieldData3.Name, ifthen(AFieldAppend, TWordDB.FieldData3.Name + ' ||', ''), dbUpdate.Query.FieldByName(TWordDB.FieldData3.Name).AsString]);
        if (dbUpdate.Query.FieldByName(TWordDB.FieldData4.Name).AsString <> '') then sql += Format('%s = %s ''%s'', ', [TWordDB.FieldData4.Name, ifthen(AFieldAppend, TWordDB.FieldData4.Name + ' ||', ''), dbUpdate.Query.FieldByName(TWordDB.FieldData4.Name).AsString]);
        if (dbUpdate.Query.FieldByName(TWordDB.FieldData5.Name).AsString <> '') then sql += Format('%s = %s ''%s'', ', [TWordDB.FieldData5.Name, ifthen(AFieldAppend, TWordDB.FieldData5.Name + ' ||', ''), dbUpdate.Query.FieldByName(TWordDB.FieldData5.Name).AsString]);
      end else begin
        sql += Format('%s = %s ''%s'', ', [TWordDB.FieldExplan.Name, ifthen(AFieldAppend, TWordDB.FieldExplan.Name + ' ||', ''), dbUpdate.Query.FieldByName(TWordDB.FieldExplan.Name).AsString]);
        sql += Format('%s = %s ''%s'', ', [TWordDB.FieldContent.Name, ifthen(AFieldAppend, TWordDB.FieldContent.Name + ' ||', ''), dbUpdate.Query.FieldByName(TWordDB.FieldContent.Name).AsString]);
        sql += Format('%s = %s ''%s'', ', [TWordDB.FieldWordExt.Name, ifthen(AFieldAppend, TWordDB.FieldWordExt.Name + ' ||', ''), dbUpdate.Query.FieldByName(TWordDB.FieldWordExt.Name).AsString]);
        sql += Format('%s = %s ''%s'', ', [TWordDB.FieldKana.Name, ifthen(AFieldAppend, TWordDB.FieldKana.Name + ' ||', ''), dbUpdate.Query.FieldByName(TWordDB.FieldKana.Name).AsString]);
        sql += Format('%s = %s ''%s'', ', [TWordDB.FieldIPA.Name, ifthen(AFieldAppend, TWordDB.FieldIPA.Name + ' ||', ''), dbUpdate.Query.FieldByName(TWordDB.FieldIPA.Name).AsString]);
        sql += Format('%s = %d, ', [TWordDB.FieldFreqCommon.Name, dbUpdate.Query.FieldByName(TWordDB.FieldFreqCommon.Name).AsLargeInt]);
        sql += Format('%s = %d, ', [TWordDB.FieldFreqRelative.Name, dbUpdate.Query.FieldByName(TWordDB.FieldFreqRelative.Name).AsLargeInt]);
        sql += Format('%s = %d, ', [TWordDB.FieldFreqUser.Name, dbUpdate.Query.FieldByName(TWordDB.FieldFreqUser.Name).AsLargeInt]);
        sql += Format('%s = %d, ', [TWordDB.FieldDictId.Name, dbUpdate.Query.FieldByName(TWordDB.FieldDictId.Name).AsInteger]);
        sql += Format('%s = %d, ', [TWordDB.FieldVersion.Name, dbUpdate.Query.FieldByName(TWordDB.FieldVersion.Name).AsInteger]);
        sql += Format('%s = %d, ', [TWordDB.FieldStatus.Name, dbUpdate.Query.FieldByName(TWordDB.FieldStatus.Name).AsInteger]);
        sql += Format('%s = %s ''%s'', ', [TWordDB.FieldData1.Name, ifthen(AFieldAppend, TWordDB.FieldData1.Name + ' ||', ''), dbUpdate.Query.FieldByName(TWordDB.FieldData1.Name).AsString]);
        sql += Format('%s = %s ''%s'', ', [TWordDB.FieldData2.Name, ifthen(AFieldAppend, TWordDB.FieldData2.Name + ' ||', ''), dbUpdate.Query.FieldByName(TWordDB.FieldData2.Name).AsString]);
        sql += Format('%s = %s ''%s'', ', [TWordDB.FieldData3.Name, ifthen(AFieldAppend, TWordDB.FieldData3.Name + ' ||', ''), dbUpdate.Query.FieldByName(TWordDB.FieldData3.Name).AsString]);
        sql += Format('%s = %s ''%s'', ', [TWordDB.FieldData4.Name, ifthen(AFieldAppend, TWordDB.FieldData4.Name + ' ||', ''), dbUpdate.Query.FieldByName(TWordDB.FieldData4.Name).AsString]);
        sql += Format('%s = %s ''%s'', ', [TWordDB.FieldData5.Name, ifthen(AFieldAppend, TWordDB.FieldData5.Name + ' ||', ''), dbUpdate.Query.FieldByName(TWordDB.FieldData5.Name).AsString]);

      end;
      sql := sql.Trim.TrimRight([',']);
      sql += Format(' WHERE %s = %d AND %s < %d', [TWordDB.FieldID.Name, id, TWordDB.FieldVersion.Name, dbUpdate.Query.FieldByName(TWordDB.FieldVersion.Name).AsInteger]);
      sqlPool.Add(sql);
    end;
    dbUpdate.Query.Next;
    Inc(cnt);
    if (cnt > OptDataBatchSize - 1) then begin
      {$IFDEF DEBUG}
      WriteLn(Format('BATCH UPDATE => %d', [m * OptDataBatchSize]));
      {$ENDIF}
      FQuery.Close;
      FTransaction.Active:= False;
      FQuery.Options:= [sqoKeepOpenOnCommit];
      FTransaction.StartTransaction;
      for i := 0 to sqlPool.Count - 1 do ExecuteSQL(sqlPool[i], AError);
      FTransaction.Commit;
      FTransaction.EndTransaction;
      sqlPool.Clear;
      Inc(m);
      dbUpdate.Select(Format('select * from %s order by id asc limit %d, %d', [DBNAME, m * OptDataBatchSize, OptDataBatchSize]), AError);
      dbUpdate.Query.First;
      cnt := 0;
    end;
  end;

  if (sqlPool.Count > 0) then begin
    {$IFDEF DEBUG}
    WriteLn(Format('BATCH UPDATE => %d', [m * OptDataBatchSize]));
    {$ENDIF}
    FQuery.Close;
    FTransaction.Active:= False;
    FQuery.Options:= [sqoKeepOpenOnCommit];
    FTransaction.StartTransaction;
    for i := 0 to sqlPool.Count - 1 do ExecuteSQL(sqlPool[i], AError);
    FTransaction.Commit;
    FTransaction.EndTransaction;
    sqlPool.Clear;
  end;
  dbUpdate.Close(APath, AError);
  dbUpdate.Free;
  FQuery.Options:= [sqoKeepOpenOnCommit, sqoAutoCommit];
  sqlPool.Free;
  _HasFullDict();
  if (lastError.Trim <> '') then Result := False;
end;
*)

function TSQLite.exportUserFreq(ASaveFilePath: string): Boolean;
begin
  FQuery.Close;
  FQuery.SQL.Text:= 'select word, user_freq from offlineword where user_freq > 0';
  try
    FQuery.Open;
    FQuery.First;
    with TStringList.Create do begin
      while (not FQuery.EOF) do begin
        Add(Format('%s=%d', [FQuery.FieldByName('word').AsString, FQuery.FieldByName('user_freq').AsLargeInt]));
        FQuery.Next;
      end;
      SaveToFile(ASaveFilePath);
      Free;
    end;
    lastError := '';
    Exit(True);
  Except
    on E: Exception do begin
      lastError := e.Message;
      Exit(False);
    end;
  end;
end;

function TSQLite.importUserFreq(AFreqFilePath: string): Boolean;
var
  i: Integer;
  sqlPool: TStringList;
  AError: string;
begin
  sqlPool := TStringList.Create;
  with TStringList.Create do begin
    LoadFromFile(AFreqFilePath);
    for i := 0 to Count - 1 do sqlPool.Add(Format('update offline set user_freq = %d where word = ''%s''', [Names[i], ValueFromIndex[i]]));
    Free;
  end;
  FQuery.Close;
  FTransaction.Active:= False;
  FQuery.Options:= [sqoKeepOpenOnCommit];
  FTransaction.StartTransaction;
  for i := 0 to sqlPool.Count - 1 do ExecuteSQL(sqlPool[i], AError);
  FTransaction.Commit;
  FTransaction.EndTransaction;
  FQuery.Options:= [sqoKeepOpenOnCommit, sqoAutoCommit];
  sqlPool.Free;
end;

{$IFDEF PERFOEMANCE_TEST}
procedure TSQLite.ImportData(AStringList: TStringList);
var
  i: Integer;
  sarr: TStringArray;
  s: string;
  AError: string;
begin
  FQuery.Close;
  FQuery.Options:= [sqoKeepOpenOnCommit];
  FTransaction.Active:= False;
  FTransaction.StartTransaction;
  for i := 0 to AStringList.Count - 1 do begin
    if (AStringList[i].Trim <> '') then begin
      sarr := AStringList[i].Split(['|']);
      s := TSQLBuilder.buildInsert(DBNAME, [TWordDB.FieldWord, TWordDB.FieldExplan], [sarr[0], sarr[1]]);
      ExecuteSQL(s, AError);
    end;
  end;
  FTransaction.Commit;
  FTransaction.EndTransaction;
  FQuery.Options:= [sqoKeepOpenOnCommit, sqoAutoCommit];
end;
{$ENDIF}

initialization
  dbs:= {$IFDEF FGL}TDatabaseList.Create{$ELSE}TStringList.Create{$ENDIF};

finalization
  dbs.Free;

end.

