unit Lb.SysUtils.Query;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  System.Variants,
  Data.DB,
  Lb.DataModuleDB,
  Lb.SysUtils.TypeQuery,
  Lb.SysUtils.DocFile;

{$IFDEF DEBUG}
  {$DEFINE DB_QUERY_DOC}
{$ENDIF}

type
  ///<summary>Общие запросы</summary>
  TQuery = record
  public const
    PATH_DELIM = '/';
  public
    ///<summary>Генератор ключа</summary>
    class function GetGeneratorID: String; static;
    ///<summary>Парсер - пути</summary>
    class function ParserStrings(const APath: String; const AStrings: TStrings): String; static;
  end;

  ///<summary>Запрос касательно папки</summary>
  TQueryFolder = record
  public
    ///<summary>Проверка наличие папки с таким именем</summary>
    class function IsFolder(const ADataModuleDB: TDataModuleDB; const ANameFolder: String; const AParentID: String = 'root'): Boolean; static;
    ///<summary>Проверки имени папку</summary>
    class function IsCheckNameFolder(const ANameFolder: String): Boolean; static;
    ///<summary>Добавить папку</summary>
    ///<returns>Возвращает уникальный ключ папки</returns>
    class function AddFolder(const ADataModuleDB: TDataModuleDB; const AParams: TParams): String; static;
    ///<summary>Переимеовать папку</summary>
    class function RenameFolder(const ADataModuleDB: TDataModuleDB; const AParams: TParams): Boolean; static;
    ///<summary>Удалить папку</summary>
    class function DeletedFolder(const ADataModuleDB: TDataModuleDB; const AID: String): Boolean; static;
    ///<summary>Получить имя папки, по ключу</summary>
    ///<remarks>Оставил вдруг, где еще использовал</remarks>
    class function FolderIDToName(const ADataModuleDB: TDataModuleDB; const AID: String): String; static;
    ///<summary>Запросить по ключу папку</summary>
    class function ParamsToFolderID(const ADataModuleDB: TDataModuleDB; const AID: String; const AParams: TParams): Boolean; static;
    ///<summary>По имени получаем ключ</summary>
    class function NameToFolderID(const ADataModuleDB: TDataModuleDB; const ANameFolder: String): String; static;
    ///<summary>Возвращаем ключ кому принадлежит объект</summary>
    class function ParentToFolderID(const ADataModuleDB: TDataModuleDB; const AID: String): String; static;
    ///<summary>По ключу возвращает полный путь до текущий папки</summary>
    class function PathToFolderID(const ADataModuleDB: TDataModuleDB; const AID: String): String; static;
    ///<summary>По ключу возвращает полный путь до текущий папки, по ключам</summary>
    class function PathToFolder(const ADataModuleDB: TDataModuleDB; const AID: String): String; static;
    ///<summary>Парсинг пути</summary>
    class function ParserFolders(const APath: String; const AStrings: TStrings): String; static;
  public
    ///<summary>Получить список папок</summary>
    class function GetLS(const ADataModuleDB: TDataModuleDB; const AParentID: String; const AFolders: TFolders): Boolean; static;
  end;

  ///<summary>Работа с документами</summary>
  TQueryDoc = record
  public

    {Проверка наличия поля в таблице значений}
    class function IsFieldNameText(const ADataModuleDB: TDataModuleDB; const ADocID, AName: String): Boolean; static;
    class function IsFieldNameNumber(const ADataModuleDB: TDataModuleDB; const ADocID, AName: String): Boolean; static;
    class function IsFieldNameBlob(const ADataModuleDB: TDataModuleDB; const ADocID, AName: String): Boolean; static;

    {Вставить запись}
    class function InsertFieldText(const ADataModuleDB: TDataModuleDB; const ADocID, AName, AValue: String; const ATypeValue: TParams.TTypeValue): Boolean; static;
    class function InsertFieldNumber(const ADataModuleDB: TDataModuleDB; const ADocID, AName: String; const AValue: Double; const ATypeValue: TParams.TTypeValue): Boolean; static;
    class function InsertFieldBlob(const ADataModuleDB: TDataModuleDB; const ADocID, AName: String; const AValue: TStream): Boolean; static;

    {Обновление записи}
    class function UpDateFieldText(const ADataModuleDB: TDataModuleDB; const ADocID, AName, AValue: String): Boolean; static;
    class function UpDateFieldNumber(const ADataModuleDB: TDataModuleDB; const ADocID, AName: String; const AValue: Double): Boolean; static;
    class function UpDateFieldBlob(const ADataModuleDB: TDataModuleDB; const ADocID, AName: String; const AValue: TStream): Boolean; static;

    {Запись значение, с проверкой наличие поля. insert или update}
    class function WriteFieldText(const ADataModuleDB: TDataModuleDB; const ADocID, AName, AValue: String; const ATypeValue: TParams.TTypeValue): Boolean; static;
    class function WriteFieldNumber(const ADataModuleDB: TDataModuleDB; const ADocID, AName: String; const AValue: Double; const ATypeValue: TParams.TTypeValue): Boolean; static;
    class function WriteFieldBlob(const ADataModuleDB: TDataModuleDB; const ADocID, AName: String; const AValue: TStream): Boolean; static;

    {Читаем записи}
    class procedure ReadFieldText(const ADataModuleDB: TDataModuleDB; const ADocID: String; const AParams: TParams); static;
    class procedure ReadFieldNumber(const ADataModuleDB: TDataModuleDB; const ADocID: String; const AParams: TParams); static;
    class procedure ReadFieldBlob(const ADataModuleDB: TDataModuleDB; const ADocID: String; const AParams: TParams); static;

  public
    ///<summary>Создаем новый документ</summary>
    class function NewDocument(const ADataModuleDB: TDataModuleDB; const AFolderID: String): String; static;
    ///<summary>Закрытие документа</summary>
    class function DocumentClose(const ADataModuleDB: TDataModuleDB; const ADocID: String): Boolean; static;
    ///<summary>Удаление документа</summary>
    class function DocumentDelete(const ADataModuleDB: TDataModuleDB; const ADocID: String): Boolean; static;
    ///<summary>Проверить наличие поля в документе</summary>
    class function IsFieldName(const ADataModuleDB: TDataModuleDB; const ADocID, AName: String): Boolean; static;
    ///<summary>Добавить поле и значение, text</summary>
    class function AddField(const ADataModuleDB: TDataModuleDB; const ADocID, AName, AValue: String; const ATypeValue: Integer): Boolean; overload; static;
    ///<summary>Добавить поле и значение, number</summary>
    class function AddField(const ADataModuleDB: TDataModuleDB; const ADocID, AName: String; const AValue: Double; const ATypeValue: Integer): Boolean; overload; static;
    ///<summary>Добавить поле и значение, blob</summary>
    class function AddField(const ADataModuleDB: TDataModuleDB; const ADocID, AName: String; const AValue: TStream): Boolean; overload; static;
  public
    ///<summary>Записать документ</summary>
    class function WriteDocument(const ADataModuleDB: TDataModuleDB; const ADocID: String; const AParams: TParams): Boolean; static;
    ///<summary>Читать документ</summary>
    class function ReadDocument(const ADataModuleDB: TDataModuleDB; const ADocID: String; const AParams: TParams): Boolean; static;
    ///<summary>Читаем список документов</summary>
    class function GetReadDocuments(const ADataModuleDB: TDataModuleDB; const AFolderID: String; const ADocuments: TDocuments): Boolean; static;
  end;

implementation

uses
{$IFDEF DEBUG}
  Lb.Logger,
{$ENDIF}
  FireDAC.Stan.Param,
  Lb.SysUtils.ISO860;

{ TQuery }

class function TQuery.GetGeneratorID: String;
var
  xS: String;
  xGUID: TGUID;
begin
  CreateGUID(xGUID);
  xS := GUIDToString(xGUID);
  Result := xS;
end;

class function TQuery.ParserStrings(const APath: String; const AStrings: TStrings): String;
var
  xS: String;
  xC: Char;
begin
  for xC in APath do
  begin
    if xC = TQuery.PATH_DELIM then
    begin
      if not xS.IsEmpty then
        AStrings.Add(xS);
      xS := '';
    end
    else
      xS := xS + xC;
  end;
  if not xS.IsEmpty then
    AStrings.Add(xS);
end;

{ TQueryFolder }

class function TQueryFolder.IsFolder(const ADataModuleDB: TDataModuleDB; const ANameFolder: String; const AParentID: String = 'root'): Boolean;
var
  xSQL: String;
  xValue: Variant;
begin
  Result := False;
  if Assigned(ADataModuleDB) then
  begin
    xSQL := 'select count(*) as cnt from folder f where f.name = :name and f.parent_id = :parent_id';
    xValue := ADataModuleDB.GetExecSQLScalar(xSQL,[ANameFolder,AParentID],[ftString,ftString]);
    if not VarIsNull(xValue) then
      Result := Integer(xValue) > 0
  end;
end;

class function TQueryFolder.IsCheckNameFolder(const ANameFolder: String): Boolean;
var
  xC: Char;
begin
  Result := False;
  for xC in ANameFolder do
    if CharInSet(xC,[TQuery.PATH_DELIM]) then
    begin
      Result := True;
      Break;
    end;
end;

class function TQueryFolder.AddFolder(const ADataModuleDB: TDataModuleDB; const AParams: TParams): String;
var
  xSQL: String;
  xID: String;
  xNameFolder: String;
  xResult: Boolean;
begin
  Result := '';
  xNameFolder := AParams.ValueByName['name'].AsString;
  {$IFDEF DB_QUERY_FOLDER}
  TLogger.Log('TQueryFolder.AddFolder: ');
  TLogger.LogText('folder_1');
  TLogger.LogText(' >> ' + AParams.ValueByName['parent_id'].AsString);
  TLogger.LogText('folder_2');
  TLogger.LogText(' >> ' + AParams.ValueByName['descript'].AsString);
  TLogger.LogText('folder_3');
  TLogger.LogText(' >> ' + AParams.ValueByName['name'].AsString);
  TLogger.LogText('folder_4');
  {$ENDIF}
  if not xNameFolder.IsEmpty then
    if Assigned(ADataModuleDB) then
    begin
      xID := TQuery.GetGeneratorID;
      xSQL := 'insert into folder(id,parent_id,name,descript) ' +
              'values(:id,:parent_id,:name,:descript)';
      xResult := ADataModuleDB.GetExecSQL(xSQL,
        [xID,
         AParams.ValueByName['parent_id'].AsString,
         xNameFolder,
         AParams.ValueByName['descript'].AsString]);
      if xResult then
        Result := xID;
    end;
end;

class function TQueryFolder.RenameFolder(const ADataModuleDB: TDataModuleDB; const AParams: TParams): Boolean;
var
  xSQL: String;
  xID: String;
  xNameFolder: String;
begin
  try
    Result := False;
    xNameFolder := AParams.ValueByName['name'].AsString;
    if not xNameFolder.IsEmpty then
      if Assigned(ADataModuleDB) then
      begin
        xID := TQuery.GetGeneratorID;
        xSQL := 'update folder ' +
                'set name = :name ' +
                'where id = :id';
        Result := ADataModuleDB.GetExecSQL(xSQL,[xNameFolder,xID]);
      end;
  except
    Result := False;
  end;
end;

class function TQueryFolder.DeletedFolder(const ADataModuleDB: TDataModuleDB; const AID: String): Boolean;
var
  xSQL: String;
begin
  Result := False;
  try
    if not AID.IsEmpty then
      if Assigned(ADataModuleDB) then
      begin
        xSQL := 'delete from folder where id = :id';
        Result := ADataModuleDB.GetExecSQL(xSQL,[AID]);
      end;
  except
      Result := False;
  end;
end;

class function TQueryFolder.FolderIDToName(const ADataModuleDB: TDataModuleDB; const AID: String): String;
var
  xSQL: String;
  xValue: Variant;
begin
  Result := '';
  if Assigned(ADataModuleDB) then
  begin
    xSQL := 'select f.name from folder f where f.id = :id';
    xValue := ADataModuleDB.GetExecSQLScalar(xSQL,[AID],[ftString]);
    if not VarIsNull(xValue) then
      Result := VarToStrDef(xValue,'')
  end;
end;

class function TQueryFolder.ParamsToFolderID(const ADataModuleDB: TDataModuleDB;
  const AID: String; const AParams: TParams): Boolean;
var
  xSQL: String;
  xDataSet: TDataSet;
begin
  Result := False;
  if Assigned(ADataModuleDB) then
  begin
    xSQL := 'select * from folder where id = :id';
    xDataSet := ADataModuleDB.GetSelectCreateDataSet(xSQL,[AID],[ftString]);
    if Assigned(xDataSet) then
    begin
      xDataSet.First;
      AParams.Clear;
      AParams.ValueByName['id'].AsString := xDataSet.FieldByName('id').AsString;
      AParams.ValueByName['parent_id'].AsString := xDataSet.FieldByName('parent_id').AsString;
      AParams.ValueByName['name'].AsString := xDataSet.FieldByName('name').AsString;
      AParams.ValueByName['descript'].AsString := xDataSet.FieldByName('descript').AsString;
      FreeAndNil(xDataSet);
    end;
  end;
end;


class function TQueryFolder.GetLS(const ADataModuleDB: TDataModuleDB; const AParentID: String; const AFolders: TFolders): Boolean;
var
  xSQL: String;
  xFolder: TFolder;
  xDataSet: TDataSet;
begin
  Result := False;
  if not Assigned(AFolders) then
    raise Exception.Create('Error Message: Список папок не определена');
  AFolders.Clear;
  if Assigned(ADataModuleDB) then
  begin
    xSQL := 'select * from folder where parent_id = :id';
    xDataSet := ADataModuleDB.GetSelectCreateDataSet(xSQL,[AParentID],[ftString]);
    if Assigned(xDataSet) then
    begin
      xDataSet.First;
      while not xDataSet.Eof do
      begin
        xFolder := AFolders.GetCreateFolder;
        xFolder.ID := xDataSet.FieldByName('id').AsString;
        xFolder.ParentID := xDataSet.FieldByName('parent_id').AsString;
        xFolder.Name := xDataSet.FieldByName('name').AsString;
        xFolder.Descript := xDataSet.FieldByName('descript').AsString;
        xDataSet.Next;
      end;
      FreeAndNil(xDataSet);
    end;
    Result := AFolders.Items.Count > 0;
  end;
end;

class function TQueryFolder.NameToFolderID(const ADataModuleDB: TDataModuleDB; const ANameFolder: String): String;
var
  xSQL: String;
  xValue: Variant;
begin
  Result := '';
  if Assigned(ADataModuleDB) then
  begin
    xSQL := 'select f.id from folder f where f.name = :name';
    xValue := ADataModuleDB.GetExecSQLScalar(xSQL,[ANameFolder],[ftString]);
    if not VarIsNull(xValue) then
      Result := VarToStrDef(xValue,'')
  end;
end;

class function TQueryFolder.ParentToFolderID(const ADataModuleDB: TDataModuleDB; const AID: String): String;
var
  xSQL: String;
  xValue: Variant;
begin
  Result := '';
  if Assigned(ADataModuleDB) then
  begin
    xSQL := 'select f.parent_id from folder f where f.id = :id';
    xValue := ADataModuleDB.GetExecSQLScalar(xSQL,[AID],[ftString]);
    if not VarIsNull(xValue) then
      Result := VarToStrDef(xValue,'')
  end;
end;

class function TQueryFolder.PathToFolderID(const ADataModuleDB: TDataModuleDB; const AID: String): String;
var
  xPath: String;
  xParentID: String;
begin
  xPath := '';
  if Assigned(ADataModuleDB) then
  begin
    xParentID := AID;
    while True do
    begin
      xPath := xParentID + TQuery.PATH_DELIM + xPath;
      xParentID := TQueryFolder.ParentToFolderID(ADataModuleDB,xParentID);
      if SameText(xParentID,'root') then
        Break;
    end;
  end;
  Result := 'root' + TQuery.PATH_DELIM + xPath;
end;

class function TQueryFolder.PathToFolder(const ADataModuleDB: TDataModuleDB; const AID: String): String;
var
  xPath: String;
  xNameFolder: String;
  xParentID: String;
begin
  xPath := '';
  if Assigned(ADataModuleDB) then
  begin
    xParentID := AID;
    while True do
    begin
      xNameFolder := TQueryFolder.FolderIDToName(ADataModuleDB,xParentID);
      xParentID := TQueryFolder.ParentToFolderID(ADataModuleDB,xParentID);
      xPath := xNameFolder + TQuery.PATH_DELIM + xPath;
      if SameText(xParentID,'root') then
        Break;
    end;
  end;
  Result := 'root' + TQuery.PATH_DELIM + xPath;
end;

class function TQueryFolder.ParserFolders(const APath: String; const AStrings: TStrings): String;
begin
  TQuery.ParserStrings(APath,AStrings);
end;

{ TQueryDoc }

class function TQueryDoc.NewDocument(const ADataModuleDB: TDataModuleDB; const AFolderID: String): String;
var
  xSQL: String;
  xID: String;
  xResult: Boolean;
begin
  Result := '';
  if Assigned(ADataModuleDB) then
  begin
    xID := TQuery.GetGeneratorID;
    xSQL := 'insert into doc(id,folder_id,status) values(:id,:folder_id,:status)';
    xResult := ADataModuleDB.GetExecSQL(xSQL,[xID,AFolderID,0],[ftString,ftString,ftInteger]);
    if xResult then
      Result := xID;
  end;
end;

class function TQueryDoc.DocumentClose(const ADataModuleDB: TDataModuleDB; const ADocID: String): Boolean;
var
  xSQL: String;
begin
  Result := False;
  if Assigned(ADataModuleDB) then
  begin
    xSQL := 'update doc set status = :status where id = :id';
    Result := ADataModuleDB.GetExecSQL(xSQL,[1,ADocID],[ftInteger,ftString]);
  end;
end;

class function TQueryDoc.DocumentDelete(const ADataModuleDB: TDataModuleDB; const ADocID: String): Boolean;
var
  xSQL: String;
begin
  Result := False;
  if Assigned(ADataModuleDB) then
  begin
    xSQL := 'delete from doc where id = :id';
    Result := ADataModuleDB.GetExecSQL(xSQL,[ADocID],[ftString]);
  end;
end;

class function TQueryDoc.IsFieldNameText(const ADataModuleDB: TDataModuleDB; const ADocID, AName: String): Boolean;
var
  xSQL: String;
  xValue: Variant;
begin
  Result := False;
  if Assigned(ADataModuleDB) then
  begin
    xSQL := 'select count(*) as cnt from doc_value_text t where t.doc_id = :doc_id and t.name = :name';
    xValue := ADataModuleDB.GetExecSQLScalar(xSQL,[ADocID,AName],[ftString,ftString]);
    if not VarIsNull(xValue) then
      Result := Integer(xValue) > 0
  end;
end;

class function TQueryDoc.IsFieldNameNumber(const ADataModuleDB: TDataModuleDB; const ADocID, AName: String): Boolean;
var
  xSQL: String;
  xValue: Variant;
begin
  Result := False;
  if Assigned(ADataModuleDB) then
  begin
    xSQL := 'select count(*) as cnt from doc_value_number t where t.doc_id = :doc_id and t.name = :name';
    xValue := ADataModuleDB.GetExecSQLScalar(xSQL,[ADocID,AName],[ftString,ftString]);
    if not VarIsNull(xValue) then
      Result := Integer(xValue) > 0
  end;
end;

class function TQueryDoc.IsFieldNameBlob(const ADataModuleDB: TDataModuleDB; const ADocID, AName: String): Boolean;
var
  xSQL: String;
  xValue: Variant;
begin
  Result := False;
  if Assigned(ADataModuleDB) then
  begin
    xSQL := 'select count(*) as cnt from doc_value_blob t where t.doc_id = :doc_id and t.name = :name';
    xValue := ADataModuleDB.GetExecSQLScalar(xSQL,[ADocID,AName],[ftString,ftString]);
    if not VarIsNull(xValue) then
      Result := Integer(xValue) > 0
  end;
end;

class function TQueryDoc.InsertFieldText(const ADataModuleDB: TDataModuleDB; const ADocID, AName, AValue: String; const ATypeValue: TParams.TTypeValue): Boolean;
var
  xSQL: String;
begin
  Result := False;
  if Assigned(ADataModuleDB) then
  begin
    xSQL := 'insert into doc_value_text(doc_id,name,type_value,value) values(:doc_id,:name,:type_value,:value)';
    Result := ADataModuleDB.GetExecSQL(xSQL,[ADocID,AName,Integer(ATypeValue),AValue],[ftString,ftString,ftInteger,ftString]);
  end;
end;

class function TQueryDoc.InsertFieldNumber(const ADataModuleDB: TDataModuleDB; const ADocID, AName: String; const AValue: Double; const ATypeValue: TParams.TTypeValue): Boolean;
var
  xSQL: String;
begin
  Result := False;
  if Assigned(ADataModuleDB) then
  begin
    xSQL := 'insert into doc_value_number(doc_id,name,type_value,value) values(:doc_id,:name,:type_value,:value)';
    Result := ADataModuleDB.GetExecSQL(xSQL,[ADocID,AName,Integer(ATypeValue),AValue],[ftString,ftString,ftInteger,ftFloat]);
  end;
end;

class function TQueryDoc.InsertFieldBlob(const ADataModuleDB: TDataModuleDB; const ADocID, AName: String; const AValue: TStream): Boolean;

{$IFDEF DB_QUERY_DOC}
  procedure SetQueryDocDebug;
  var
    xParam: TFDParam;
    i, iCount: Integer;
  begin
    with ADataModuleDB.FDCommand do
    begin
      iCount := Params.Count;
      if iCount > 0 then
        for i := 0 to iCount - 1 do
        begin
          xParam := Params[i];
          TLogger.Log('param_name: ' + IntToStr(i) + ' ' + xParam.Name);
        end;
    end;
  end;
{$ENDIF}

var
  xSQL: String;
  xParam: TFDParam;
  xMemory: TMemoryStream;
begin
  Result := False;
  if Assigned(ADataModuleDB) then
  begin
    try
      AValue.Position := 0;
      xMemory := TMemoryStream.Create;
      xMemory.LoadFromStream(AValue);
      xMemory.Position := 0;
      xSQL := 'insert into doc_value_blob(doc_id,name,value) values(:doc_id,:name,:value)';
      with ADataModuleDB.FDCommand do
      begin
        CommandText.Text := xSQL;
        {$IFDEF DB_QUERY_DOC}
        SetQueryDocDebug;
        {$ENDIF}
        // -------------------------
        xParam := Params[0];
        xParam.DataType := ftString;
        xParam.Value := ADocID;
        // -------------------------
        xParam := Params[1];
        xParam.DataType := ftString;
        xParam.Value := AName;
        // -------------------------
        //xParam := Params[2];
        //xParam.DataType := ftStream;
        xParam.LoadFromStream(xMemory,ftStream,2);
        // -------------------------
        Execute;
      end;
      Result := True;
    except on E: Exception do
      raise Exception.Create('Error Message: ' + E.Message);
    end;
  end;
end;

class function TQueryDoc.UpDateFieldText(const ADataModuleDB: TDataModuleDB; const ADocID, AName, AValue: String): Boolean;
var
  xSQL: String;
begin
  Result := False;
  if Assigned(ADataModuleDB) then
  begin
    xSQL := 'update doc_value_text set value = :value where doc_id = :doc_id and name = :name';
    Result := ADataModuleDB.GetExecSQL(xSQL,[AValue,ADocID,AName],[ftString,ftString,ftString]);
  end;
end;

class function TQueryDoc.UpDateFieldNumber(const ADataModuleDB: TDataModuleDB; const ADocID, AName: String; const AValue: Double): Boolean;
var
  xSQL: String;
begin
  Result := False;
  if Assigned(ADataModuleDB) then
  begin
    xSQL := 'update doc_value_number set value = :value where doc_id = :doc_id and name = :name';
    Result := ADataModuleDB.GetExecSQL(xSQL,[AValue,ADocID,AName],[ftFloat,ftString,ftString]);
  end;
end;

class function TQueryDoc.UpDateFieldBlob(const ADataModuleDB: TDataModuleDB; const ADocID, AName: String; const AValue: TStream): Boolean;
var
  xSQL: String;
  xParam: TFDParam;
  xMemory: TMemoryStream;
begin
  Result := False;
  if Assigned(ADataModuleDB) then
  begin
    try
      AValue.Position := 0;
      xMemory := TMemoryStream.Create;
      xMemory.LoadFromStream(AValue);
      xMemory.Position := 0;
      xSQL := 'update doc_value_blob set value = :value where doc_id = :doc_id and name = :name';
      with ADataModuleDB.FDCommand do
      begin
        CommandText.Text := xSQL;
        xParam := Params[0];
        xParam.DataType := ftStream;
        xParam.AsStream := xMemory;
        // -------------------------
        xParam := Params[1];
        xParam.DataType := ftString;
        xParam.Value := ADocID;
        // -------------------------
        xParam := Params[2];
        xParam.DataType := ftString;
        xParam.Value := AName;
      end;
      Result := True;
    except
      Result := False;
    end;
  end;
end;

class function TQueryDoc.WriteFieldText(const ADataModuleDB: TDataModuleDB; const ADocID, AName, AValue: String; const ATypeValue: TParams.TTypeValue): Boolean;
begin
  if TQueryDoc.IsFieldNameText(ADataModuleDB,ADocID,AName) then
    Result := TQueryDoc.UpDateFieldText(ADataModuleDB,ADocID,AName,AValue)
  else
    Result := TQueryDoc.InsertFieldText(ADataModuleDB,ADocID,AName,AValue,ATypeValue)
end;

class function TQueryDoc.WriteFieldNumber(const ADataModuleDB: TDataModuleDB; const ADocID, AName: String; const AValue: Double; const ATypeValue: TParams.TTypeValue): Boolean;
begin
  if TQueryDoc.IsFieldNameNumber(ADataModuleDB,ADocID,AName) then
    Result := TQueryDoc.UpDateFieldNumber(ADataModuleDB,ADocID,AName,AValue)
  else
    Result := TQueryDoc.InsertFieldNumber(ADataModuleDB,ADocID,AName,AValue,ATypeValue)
end;

class function TQueryDoc.WriteFieldBlob(const ADataModuleDB: TDataModuleDB; const ADocID, AName: String; const AValue: TStream): Boolean;
begin
  if TQueryDoc.IsFieldNameBlob(ADataModuleDB,ADocID,AName) then
    Result := TQueryDoc.UpDateFieldBlob(ADataModuleDB,ADocID,AName,AValue)
  else
    Result := TQueryDoc.InsertFieldBlob(ADataModuleDB,ADocID,AName,AValue)
end;

class procedure TQueryDoc.ReadFieldText(const ADataModuleDB: TDataModuleDB; const ADocID: String; const AParams: TParams);
var
  xSQL: String;
  xDataSet: TDataSet;
  xName: String;
  xTypeValue: Integer;
  xValue: String;
  xParamValue: TParams.TValue;
begin
  xSQL := 'select * from doc_value_text where doc_id = :doc_id';
  if Assigned(ADataModuleDB) then
  begin
    xDataSet := ADataModuleDB.GetSelectCreateDataSet(xSQL,[ADocID],[ftString]);
    if Assigned(xDataSet) then
    begin
      xDataSet.First;
      while not xDataSet.Eof do
      begin
        xName := xDataSet.FieldByName('name').AsString;
        xTypeValue := xDataSet.FieldByName('type_value').AsInteger;
        xValue := xDataSet.FieldByName('value').AsString;
        // ---------------------------------------------------------
        xParamValue := AParams.ValueByName[xName];
        xParamValue.SetValueString(xValue,TParams.TTypeValue(xTypeValue));
        // ---------------------------------------------------------
        xDataSet.Next;
      end;
      FreeAndNil(xDataSet);
    end;
  end;
end;

class procedure TQueryDoc.ReadFieldNumber(const ADataModuleDB: TDataModuleDB; const ADocID: String; const AParams: TParams);
var
  xSQL: String;
  xDataSet: TDataSet;
  xName: String;
  xTypeValue: Integer;
  xValue: Double;
  xParamValue: TParams.TValue;
begin
  xSQL := 'select * from doc_value_number where doc_id = :doc_id';
  if Assigned(ADataModuleDB) then
  begin
    xDataSet := ADataModuleDB.GetSelectCreateDataSet(xSQL,[ADocID],[ftString]);
    if Assigned(xDataSet) then
    begin
      xDataSet.First;
      while not xDataSet.Eof do
      begin
        xName := xDataSet.FieldByName('name').AsString;
        xTypeValue := xDataSet.FieldByName('type_value').AsInteger;
        xValue := xDataSet.FieldByName('value').AsFloat;
        // ---------------------------------------------------------
        xParamValue := AParams.ValueByName[xName];
        xParamValue.SetValueNumber(xValue,TParams.TTypeValue(xTypeValue));
        // ---------------------------------------------------------
        xDataSet.Next;
      end;
      FreeAndNil(xDataSet);
    end;
  end;
end;

class procedure TQueryDoc.ReadFieldBlob(const ADataModuleDB: TDataModuleDB; const ADocID: String; const AParams: TParams);
var
  xSQL: String;
  xDataSet: TDataSet;
  xName: String;
  xValueStream: TStream;
  xField: TField;
  xParamValue: TParams.TValue;
begin
  xSQL := 'select * from doc_value_blob where doc_id = :doc_id';
  if Assigned(ADataModuleDB) then
  begin
    xDataSet := ADataModuleDB.GetSelectCreateDataSet(xSQL,[ADocID],[ftString]);
    if Assigned(xDataSet) then
    begin
      xDataSet.First;
      while not xDataSet.Eof do
      begin
        xField := xDataSet.FieldByName('name');
        xName := xField.AsString;
        xValueStream := xDataSet.CreateBlobStream(xField,TBlobStreamMode.bmRead);
        if Assigned(xValueStream) then
        begin
          xParamValue := AParams.ValueByName[xName];
          xParamValue.LoadFromStream(xValueStream);
        end;
        xDataSet.Next;
      end;
      FreeAndNil(xDataSet);
    end;
  end;
end;

class function TQueryDoc.IsFieldName(const ADataModuleDB: TDataModuleDB; const ADocID, AName: String): Boolean;
{$DEFINE ALL_IS_FIELD_NAME}

{$IFNDEF ALL_IS_FIELD_NAME}
var
  xValue: Variant;
  xStr: TStrings;
{$ENDIF}
begin
  Result := False;
  {$IFDEF ALL_IS_FIELD_NAME}
  if TQueryDoc.IsFieldNameText(ADataModuleDB,ADocID, AName) then
    Result := True
  else if TQueryDoc.IsFieldNameNumber(ADataModuleDB,ADocID, AName) then
    Result := True
  else if TQueryDoc.IsFieldNameBlob(ADataModuleDB,ADocID, AName) then
    Result := True;
  {$ELSE}
  if Assigned(ADataModuleDB) then
  begin
    xStr := TStringList.Create;
    try
      xStr.Add('with');
      xStr.Add('    vt as (select t.doc_id,t.name from doc_value_text t where t.doc_id = :doc_id1),');
      xStr.Add('    vn as (select t.doc_id,t.name from doc_value_number t where t.doc_id = :doc_id2),');
      xStr.Add('    vb as (select t.doc_id,t.name from doc_value_blob t where t.doc_id = :doc_id3),');
      xStr.Add('    r as (');
      xStr.Add('        select * from vt');
      xStr.Add('        union all');
      xStr.Add('        select * from vn');
      xStr.Add('        union all');
      xStr.Add('        select * from vb');
      xStr.Add('    )');
      xStr.Add('select count(*) as cnt from r where r.name = :name');
      xValue := ADataModuleDB.GetExecSQLScalar(xStr.Text,[ADocID,ADocID,ADocID,AName]);
      if not VarIsNull(xValue) then
        Result := Integer(xValue) > 0
    finally
      FreeAndNil(xStr);
    end;
  end;
  {$ENDIF}
end;

class function TQueryDoc.AddField(const ADataModuleDB: TDataModuleDB; const ADocID, AName, AValue: String; const ATypeValue: Integer): Boolean;
var
  xSQL: String;
begin
  Result := False;
  if Assigned(ADataModuleDB) then
  begin
    xSQL := 'insert into doc_value_text(doc_id,name,type_value,value) ' +
            'values(:doc_id,:name,:type_value,:value)';
    Result := ADataModuleDB.GetExecSQL(xSQL,
      [ADocID,AName,ATypeValue,AValue],
      [ftString,ftString,ftInteger,ftString]);

  end;
end;

class function TQueryDoc.AddField(const ADataModuleDB: TDataModuleDB; const ADocID, AName: String; const AValue: Double; const ATypeValue: Integer): Boolean;
var
  xSQL: String;
begin
  Result := False;
  if Assigned(ADataModuleDB) then
  begin
    xSQL := 'insert into doc_value_number(doc_id,name,type_value,value) ' +
            'values(:doc_id,:name,:type_value,:value)';
    Result := ADataModuleDB.GetExecSQL(xSQL,
      [ADocID,AName,ATypeValue,AValue],
      [ftString,ftString,ftInteger,ftFloat]);
  end;
end;

class function TQueryDoc.AddField(const ADataModuleDB: TDataModuleDB; const ADocID, AName: String; const AValue: TStream): Boolean;
var
  xSQL: String;
begin
  Result := False;
  if Assigned(ADataModuleDB) then
  begin
    xSQL := 'insert into doc_value_blob(doc_id,name,value) ' +
            'values(:doc_id,:name,:value)';
    try
      with ADataModuleDB do
      begin
        FDCommand.Params.Clear;
        FDCommand.CommandText.Text := xSQL;
        FDCommand.ParamByName('doc_id').AsString := ADocID;
        FDCommand.ParamByName('name').AsString := AName;
        FDCommand.ParamByName('value').LoadFromStream(AValue,ftBlob);
        FDCommand.Execute();
      end;
    except
      Result := False;
    end;
  end;
end;

class function TQueryDoc.WriteDocument(const ADataModuleDB: TDataModuleDB; const ADocID: String; const AParams: TParams): Boolean;
var
  i, Count: Integer;
  xValue: TParams.TValue;
begin
  Result := False;
  Count := AParams.Count;
  if Count > 0 then
    for i := 0 to Count - 1 do
    begin
      xValue := AParams.Items[i];
      case xValue.TypeValue of
        tvString: TQueryDoc.WriteFieldText(ADataModuleDB,ADocID,xValue.Name,xValue.ValueString,xValue.TypeValue);
        tvInteger: TQueryDoc.WriteFieldNumber(ADataModuleDB,ADocID,xValue.Name,xValue.ValueNumber,xValue.TypeValue);
        tvInt64: TQueryDoc.WriteFieldNumber(ADataModuleDB,ADocID,xValue.Name,xValue.ValueNumber,xValue.TypeValue);
        tvDouble: TQueryDoc.WriteFieldNumber(ADataModuleDB,ADocID,xValue.Name,xValue.ValueNumber,xValue.TypeValue);
        tvBoolean: TQueryDoc.WriteFieldNumber(ADataModuleDB,ADocID,xValue.Name,xValue.ValueNumber,xValue.TypeValue);
        tvDate: TQueryDoc.WriteFieldText(ADataModuleDB,ADocID,xValue.Name,xValue.ValueString,xValue.TypeValue);
        tvTime: TQueryDoc.WriteFieldText(ADataModuleDB,ADocID,xValue.Name,xValue.ValueString,xValue.TypeValue);
        tvDateTime: TQueryDoc.WriteFieldText(ADataModuleDB,ADocID,xValue.Name,xValue.ValueString,xValue.TypeValue);
        tvStream: TQueryDoc.WriteFieldBlob(ADataModuleDB,ADocID,xValue.Name,xValue.Stream);
      else
        // tvNull - Если не известнный тип или null
      end;
    end;
end;

class function TQueryDoc.ReadDocument(const ADataModuleDB: TDataModuleDB; const ADocID: String; const AParams: TParams): Boolean;
begin
  AParams.Clear;
  try
    TQueryDoc.ReadFieldText(ADataModuleDB,ADocID,AParams);
    TQueryDoc.ReadFieldNumber(ADataModuleDB,ADocID,AParams);
    TQueryDoc.ReadFieldBlob(ADataModuleDB,ADocID,AParams);
    Result := True;
  except
    Result := False;
  end;
end;

class function TQueryDoc.GetReadDocuments(const ADataModuleDB: TDataModuleDB; const AFolderID: String; const ADocuments: TDocuments): Boolean;
var
  xSQL: String;
  xDataSet: TDataSet;
  xDocument: TDocument;
begin
  Result := False;
  ADocuments.Clear;
  if Assigned(ADataModuleDB) then
  begin
    xSQL := 'select * from doc d where folder_id = :forlder_id';
    xDataSet := ADataModuleDB.GetSelectCreateDataSet(xSQL,[AFolderID],[ftString]);
    if Assigned(xDataSet) then
    begin
      xDataSet.First;
      while not xDataSet.Eof do
      begin
        xDocument := ADocuments.GetCreateDocument;
        xDocument.ID := xDataSet.FieldByName('id').AsString;
        xDocument.FolderID := xDataSet.FieldByName('folder_id').AsString;
        xDocument.Status := xDataSet.FieldByName('status').AsInteger;
        xDataSet.Next;
      end;
      FreeAndNil(xDataSet);
    end;
  end;
end;

end.
