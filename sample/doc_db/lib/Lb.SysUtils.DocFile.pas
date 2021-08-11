unit Lb.SysUtils.DocFile;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  System.Variants,
  Data.DB,
  Lb.SysUtils.TypeQuery,
  Lb.DataModuleDB;

const
  ///<summary>Наименование базы данных</summary>
  DEFAULT_NAME_DATABASE = 'data';
  DEFAULT_NAME_FOLDER = 'system';

type
  ///<summary>Объект работы, для контроля выделение памяти</summary>
  TDfObject = class(TObject)
  private
  public
    constructor Create;
    destructor Destroy; override;
  end;

type
  TDataBase = class;

  ///<summary>Дата базы</summary>
  TDfObjectDataBase = class(TDfObject)
  private
    FDataBase: TDataBase;
    FParams: TParams;
  public
    constructor Create(ADataBase: TDataBase);
    destructor Destroy; override;
    property DataBase: TDataBase read FDataBase;
  end;

  ///<summary>Документ</summary>
  TDocument = class(TDfObjectDataBase)
  private
    FID: String;
    FFolderID: String;
    FStatus: Integer;
    FParams: TQueryParams;
  public
    constructor Create(ADataBase: TDataBase);
    destructor Destroy; override;
    property ID: String read FID write FID;
    property FolderID: String read FFolderID write FFolderID;
    property Status: Integer read FStatus write FStatus;
  public
    procedure Read;
    procedure Write;
    property Params: TQueryParams read FParams;
  end;

  ///<summary>Список документов</summary>
  TDocumentList = TObjectList<TDocument>;

  ///<summary>Список документов, в папке</summary>
  TDocuments = class(TDfObjectDataBase)
  private
    FFolderID: String;
    FItems: TDocumentList;
  public
    constructor Create(ADataBase: TDataBase);
    destructor Destroy; override;
    procedure Clear;
    function GetCreateDocument: TDocument;
    property Items: TDocumentList read FItems;
    property FolderID: String read FFolderID write FFolderID;
  end;

  ///<summary>Папки</summary>
  TFolder = class(TDfObjectDataBase)
  private
    FID: String;
    FParentID: String;
    FName: String;
    FDescript: String;
    FDocuments: TDocuments;
    procedure SetID(const Value: String);
  public
    constructor Create(ADataBase: TDataBase);
    destructor Destroy; override;
    property ID: String read FID write SetID;
    property ParentID: String read FParentID write FParentID;
    property Name: String read FName write FName;
    property Descript: String read FDescript write FDescript;
    property Documents: TDocuments read FDocuments;
  public
    ///<summary>Создание нового документа</summary>
    function NewDocument: String;
  end;
  TFolderList = TObjectList<TFolder>;

  ///<summary>Список папок, содержащих в папке</summary>
  TFolders = class(TDfObjectDataBase)
  private
    FParentID: String;
    FItems: TFolderList;
    procedure SetParentID(const Value: String);
    function GetPath: String;
    function GetPathFolderID: String;
    procedure SetPath(const Value: String);
    procedure SetPathFolderID(const Value: String);
  protected
    function GetFolderParentID: String;
    procedure SetSelected(const AParentID: String = 'root');
  public
    constructor Create(ADataBase: TDataBase);
    destructor Destroy; override;
    procedure Clear;
    function GetCreateFolder: TFolder;
    property Items: TFolderList read FItems;
    property ParentID: String read FParentID write SetParentID;
  public
    ///<summary>Дабовляет папку</summary>
    ///<returns>Возрощает ключ</returns>
    function AddFolder(const AName: String): String;
    ///<summary>Переменовать папку по ключу</summary>
    function RenameFolder(const AID, ANewName: String): Boolean;
    ///<summary>Удаление папку</summary>
    function DeleteFolder(const AID: String): Boolean;
    ///<summary>Путь текущий папке</summary>
    property PathFolderID: String read GetPathFolderID write SetPathFolderID;
    ///<summary>Путь текущий папке</summary>
    property Path: String read GetPath write SetPath;
  end;

  ///<summary>Открываем папку, где будут хранить файлы<summary>
  TDataBase = class(TDfObject)
  private
    FNameDataBase: String;
    FDataModuleDB: TDataModuleDB;
  protected
    procedure SetCreateDataBase;
  public
    constructor Create(const ANameDataBase: String = DEFAULT_NAME_DATABASE); 
    destructor Destroy; override;
    property DataModuleDB: TDataModuleDB read FDataModuleDB;
  end;

type
  TDocFile = record
  public
    // **********************************************************************
    ///<summary>Открыть файл базы данных</summary>
    class function GetOpen(ANameDataBase: String = DEFAULT_NAME_DATABASE): TDataBase; static;
    ///<summary>Закрыть файл</summary>
    class procedure SetClose(ADataBase: TDataBase); static;
  public
    // ***********************************************************************
    ///<summary>Открыть папку<summary>
    class function GetOpenFolder(ANameFolder: String): TFolder; static;
    ///<summary>Закрыть папку</summary>
    class procedure SetCloseFolder(AFolder: TFolder); static;
    ///<summary>Проверить существование папки</summary>
    class function IsOpenFolder(ANameFolder: String): Boolean; static;
    ///<summary>Удалить папку</summary>
    class procedure SetDeleteFolder(ANameFolder: String); static;
  end;

implementation

uses 
  Lb.Resource.Script,
  Lb.SysUtils.Query;

(******************************************************************************)
(* Объект и процедуры управление выделяймой паматью                           *)
(******************************************************************************)

type
  TDfObjects = class(TObject)
  public type
    TDfObjectList = TObjectList<TDfObject>;
  private
    FList: TDfObjectList; 
  protected
    procedure SetClear;
    function GetIndexOfDfObject(const ADfObject: TDfObject): Integer;
    function GetAddDfObject(const ADfObject: TDfObject): Integer;
    procedure SetDeleteDfObject(const ADfObject: TDfObject);
  public
    constructor Create; virtual;
    destructor Destroy; override;  
  end;

var
  localDfObjects: TDfObjects = nil;

function GetDfObjects: TDfObjects;
begin
  if not Assigned(localDfObjects) then
    localDfObjects := TDfObjects.Create;
  Result := localDfObjects;
end;

procedure SetFreeDfObjects;
begin
  if Assigned(localDfObjects) then
  begin
    localDfObjects.SetClear;
    FreeAndNil(localDfObjects);
  end;
  localDfObjects := nil;
end;

procedure SetExecutionScript(const ADataModuleDB: TDataModuleDB);
var
  xSQL: String;
begin
  // Инициировать базу данных 
  if Assigned(ADataModuleDB) then
  begin
    xSQL := GetResourceScritp('DB_SQL');
    ADataModuleDB.GetExecSQL(xSQL);
  end;
end;

{ TDfObjects }

constructor TDfObjects.Create;
begin
  FList := TDfObjectList.Create(False);
end;

destructor TDfObjects.Destroy;
begin
  FreeAndNil(FList);
  inherited;
end;

procedure TDfObjects.SetClear;
var
  xObject: TDfObject;
  i, Count: Integer;
begin
  Count := FList.Count;
  if Count > 0 then
    for i := Count - 1 downto 0 do
    begin
      xObject := FList[i];
      FreeAndNil(xObject);
    end;
  FList.Clear;  
end;

function TDfObjects.GetIndexOfDfObject(const ADfObject: TDfObject): Integer;
begin
  Result := FList.IndexOf(ADfObject);
end;

function TDfObjects.GetAddDfObject(const ADfObject: TDfObject): Integer;
begin
  Result := FList.Add(ADfObject);  
end;

procedure TDfObjects.SetDeleteDfObject(const ADfObject: TDfObject);
var
  xIndex: Integer;
begin
  if Assigned(ADfObject) then
  begin
    xIndex := Self.GetIndexOfDfObject(ADfObject);
    FList.Delete(xIndex);
  end;
end;

{ TDfObject }

constructor TDfObject.Create;
begin
  GetDfObjects.GetAddDfObject(Self);
end;

destructor TDfObject.Destroy;
begin
  GetDfObjects.SetDeleteDfObject(Self);
  inherited;
end;

{ TDfObjectDataBase }

constructor TDfObjectDataBase.Create(ADataBase: TDataBase);
begin
  inherited Create;
  FDataBase := ADataBase;
end;

destructor TDfObjectDataBase.Destroy;
begin
  FreeAndNil(FParams);
  inherited;
end;

{ TDocument }

constructor TDocument.Create(ADataBase: TDataBase);
begin
  inherited Create(ADataBase);
  FID := '';
  FFolderID := '';
  FParams := TQueryParams.Create;
end;

destructor TDocument.Destroy;
begin
  FreeAndNil(FParams);
  inherited;
end;

procedure TDocument.Read;
begin
  TQueryDoc.ReadDocument(FDataBase.DataModuleDB,FID,TQueryParams(FParams));
end;

procedure TDocument.Write;
begin
  TQueryDoc.WriteDocument(FDataBase.DataModuleDB,FID,TQueryParams(FParams));
end;

{ TDocuments }

constructor TDocuments.Create(ADataBase: TDataBase);
begin
  inherited Create(ADataBase);
  FItems := TDocumentList.Create;
end;

destructor TDocuments.Destroy;
begin
  FreeAndNil(FItems);
  inherited;
end;

procedure TDocuments.Clear;
begin
  FItems.Clear;
end;

function TDocuments.GetCreateDocument: TDocument;
var
  xDocument: TDocument;
begin
  xDocument := TDocument.Create(FDataBase);
  FItems.Add(xDocument);
  Result := xDocument;
end;


{ TFolder }

constructor TFolder.Create(ADataBase: TDataBase);
begin
  inherited Create(ADataBase);
  FID := '';
  FParentID := '';
  FName := '';
  FDescript := '';
  FDocuments := TDocuments.Create(ADataBase);
end;

destructor TFolder.Destroy;
begin
  FreeAndNil(FDocuments);
  inherited;
end;

function TFolder.NewDocument: String;
begin
  Result := TQueryDoc.NewDocument(FDataBase.DataModuleDB,Self.ID);
end;

procedure TFolder.SetID(const Value: String);
begin
  FID := Value;
  FDocuments.FolderID := FID;
  TQueryDoc.GetReadDocuments(FDataBase.DataModuleDB,FID,FDocuments);
end;

{ TFolders }

constructor TFolders.Create(ADataBase: TDataBase);
begin
  inherited Create(ADataBase);
  FItems := TFolderList.Create;
end;

destructor TFolders.Destroy;
begin
  FreeAndNil(FItems);
  inherited;
end;

procedure TFolders.Clear;
begin
  Fitems.Clear;
end;

function TFolders.GetFolderParentID: String;
begin
  if FParentID.IsEmpty then
    Result := 'root'
  else
    Result := FParentID;
end;

function TFolders.GetCreateFolder: TFolder;
var
  xFolder: TFolder;
begin
  xFolder := TFolder.Create(FDataBase);
  FItems.Add(xFolder);
  Result := xFolder;
end;

procedure TFolders.SetParentID(const Value: String);
begin
  FParentID := Value;
  Self.SetSelected(FParentID);
end;

procedure TFolders.SetSelected(const AParentID: String);
begin
  TQueryFolder.GetLS(FDataBase.DataModuleDB,AParentID,Self);
end;

function TFolders.AddFolder(const AName: String): String;
var
  xParams: TQueryParams;
  xParentID: String;
begin
  Result := '';

  if TQueryFolder.IsCheckNameFolder(AName) then
  begin
    raise Exception.Create('Error Message: Не допустимое имя папки');
    Exit;
  end;

  xParentID := GetFolderParentID;
  if not TQueryFolder.IsFolder(FDataBase.DataModuleDB,AName,xParentID) then
  begin
    xParams := TQueryParams.Create;
    try
      xParams.ValueByName['parent_id'].AsString := xParentID;
      xParams.ValueByName['name'].AsString := AName;
      Result := TQueryFolder.AddFolder(FDataBase.DataModuleDB,xParams);
    finally
      FreeAndNil(xParams);
    end;
  end;
end;

function TFolders.RenameFolder(const AID, ANewName: String): Boolean;
var
  xParams: TQueryParams;
begin
  if TQueryFolder.IsCheckNameFolder(ANewName) then
  begin
    raise Exception.Create('Error Message: Не допустимое имя папки');
    Exit;
  end;

  Result := False;
  if not AID.IsEmpty then
  begin
    xParams := TQueryParams.Create;
    try
      xParams.ValueByName['id'].AsString := AID;
      xParams.ValueByName['name'].AsString := ANewName;
      Result := TQueryFolder.RenameFolder(FDataBase.DataModuleDB,xParams);
    finally
      FreeAndNil(xParams);
    end;
  end;
end;


function TFolders.DeleteFolder(const AID: String): Boolean;
begin
  Result := False;
  if not AID.IsEmpty then
    Result := TQueryFolder.DeletedFolder(FDataBase.DataModuleDB,AID);
end;

function TFolders.GetPath: String;
begin
  if SameText(FParentID,'root') then
    Result := FParentID
  else
    Result := TQueryFolder.PathToFolder(FDataBase.DataModuleDB,Self.ParentID);
end;

procedure TFolders.SetPath(const Value: String);
var
  xStr: TStrings;
begin
  xStr := TStringList.Create;
  try
    TQueryFolder.ParserFolders(Value,xStr);
  finally
    FreeAndNil(xStr);
  end;
end;

function TFolders.GetPathFolderID: String;
begin
  if SameText(FParentID,'root') then
    Result := FParentID
  else
    Result := TQueryFolder.PathToFolderID(FDataBase.DataModuleDB,Self.ParentID);
end;

procedure TFolders.SetPathFolderID(const Value: String);
var
  xStr: TStrings;
begin
  xStr := TStringList.Create;
  try
    TQueryFolder.ParserFolders(Value,xStr);
  finally
    FreeAndNil(xStr);
  end;
end;

{ TDataBase }

constructor TDataBase.Create(const ANameDataBase: String);
begin
  inherited Create;
  FNameDataBase := ANameDataBase;
  SetCreateDataBase;
end;

destructor TDataBase.Destroy;
begin
  if Assigned(FDataModuleDB) then
    FDataModuleDB.Close;
  FreeAndNil(FDataModuleDB);
  inherited;
end;

procedure TDataBase.SetCreateDataBase;
var
  xFileExists: Boolean;
  xFileNameDB: String;
begin
  xFileNameDB := ExtractFilePath(ParamStr(0)) + FNameDataBase;
  xFileExists := FileExists(xFileNameDB);
  FDataModuleDB := TDataModuleDB.Create(nil);
  FDataModuleDB.DefaultConnection(xFileNameDB);
  // Если файлы базы нет, то нужно его создать
  if not xFileExists then
    SetExecutionScript(FDataModuleDB);
end;

{ TDocFile }

class function TDocFile.GetOpen(ANameDataBase: String): TDataBase;
begin
  try
    Result := TDataBase.Create(ANameDataBase);
  except
    Result := nil;
  end;
end;

class procedure TDocFile.SetClose(ADataBase: TDataBase);
begin
  if Assigned(ADataBase) then
    FreeAndNil(ADataBase);
end;

class function TDocFile.GetOpenFolder(ANameFolder: String): TFolder;
begin
  Result := nil;
end;

class procedure TDocFile.SetCloseFolder(AFolder: TFolder);
begin

end;

class function TDocFile.IsOpenFolder(ANameFolder: String): Boolean;
begin
  Result := False;
end;

class procedure TDocFile.SetDeleteFolder(ANameFolder: String);
begin

end;



initialization


finalization
  SetFreeDfObjects;
  
end.
