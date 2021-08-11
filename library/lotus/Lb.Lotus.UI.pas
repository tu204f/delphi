(*******************************************************************************
  Доработать для редактироваие данных, с визуальной часть Lotus Document - а
*******************************************************************************)
unit Lb.Lotus.UI;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Generics.Collections;


type
  TCustomObjectLotus = class(TObject)
  private
    FLotusObject: OleVariant;
  protected
    property LotusObject: OleVariant read FLotusObject write FLotusObject;
  public
    constructor Create; virtual;
    destructor Destroy; override;
  end;

  TCustomUI = class(TCustomObjectLotus)
  protected
    function GetClassNameLotus: String; virtual;
    procedure SetInitializationLotusObject;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

type
  TSessionUI = class;
  TWorkspaceUI = class;
  TDatabaseUI = class;
  TViewUI = class;
  TDocumentUI = class;

  TSessionUI = class(TCustomUI)
  private
    function GetPathDB(NameDB: String): String;
    function GetServer(NameDB: String): String;
  protected
    function GetClassNameLotus: String; override;
    function GetEnvironmentString(const AName: String; bisSystem: Boolean = False): String;
  public
    constructor Create; override;
    destructor Destroy; override;
    function GetCreateDatabase(const AServer, APathDB: String): TDatabaseUI;
    property Server[NameDB: String]: String read GetServer;
    property PathDB[NameDB: String]: String read GetPathDB;
  end;

  TWorkspaceUI = class(TCustomUI)
  protected
    function GetClassNameLotus: String; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure EditDocument(AEditMode: Boolean; ADocumentUI: TDocumentUI);
  end;

  TDatabaseUI = class(TCustomObjectLotus)
  private
    FServer: String;
    FPathDB: String;
  public
    constructor Create; override;
    destructor Destroy; override;
    function GetCreateView(const AName: String): TViewUI;
    property Server: String read FServer write FServer;
    property PathDB: String read FPathDB write FPathDB;
  end;

  TViewUI = class(TCustomObjectLotus)
  private
    FName: String;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Refresh;
    function GetCreateDocumentByKey(const AKey: String; AExactMatch: Boolean = True): TDocumentUI;
    property Name: String read FName write FName;
  end;

  TDocumentUI = class(TCustomObjectLotus)
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

implementation

uses
  System.Win.ComObj;

{ TCustomObjectLotus }

constructor TCustomObjectLotus.Create;
begin

end;

destructor TCustomObjectLotus.Destroy;
begin
  FLotusObject := Unassigned;
  inherited;
end;

{ TCustomUI }

constructor TCustomUI.Create;
begin
  inherited Create;
  SetInitializationLotusObject;
end;

destructor TCustomUI.Destroy;
begin

  inherited;
end;

function TCustomUI.GetClassNameLotus: String;
begin
  Result := '';
end;

procedure TCustomUI.SetInitializationLotusObject;
var
  xClassLotus: String;
begin
  xClassLotus := GetClassNameLotus;
  FLotusObject := CreateOleObject(xClassLotus);
end;

{ TSessionUI }

constructor TSessionUI.Create;
begin
  inherited Create;
end;

destructor TSessionUI.Destroy;
begin

  inherited;
end;

function TSessionUI.GetClassNameLotus: String;
begin
  Result := 'Notes.NotesSession';
end;

function TSessionUI.GetEnvironmentString(const AName: String; bisSystem: Boolean): String;
begin
  try
    Result := LotusObject.GetEnvironmentString(AName);
  except
    on E: Exception do
      raise Exception.Create('Error Message:' + E.Message);
  end;
end;

function TSessionUI.GetPathDB(NameDB: String): String;
begin
  Result := Self.GetEnvironmentString('DB' + NameDB)
end;

function TSessionUI.GetServer(NameDB: String): String;
begin
  Result := Self.GetEnvironmentString('SRV' + NameDB);
end;

function TSessionUI.GetCreateDatabase(const AServer, APathDB: String): TDatabaseUI;
var
  xDatabaseUI: TDatabaseUI;
begin
  try
    xDatabaseUI := TDatabaseUI.Create;
    xDatabaseUI.Server := AServer;
    xDatabaseUI.PathDB := APathDB;
    xDatabaseUI.LotusObject := Self.LotusObject.GetDatabase(AServer,APathDB);
    Result := xDatabaseUI;
  except
    on E: Exception do
      raise Exception.Create('Error Message:' + E.Message);
  end;
end;

{ TWorkspaceUI }

constructor TWorkspaceUI.Create;
begin
  inherited;
end;

destructor TWorkspaceUI.Destroy;
begin

  inherited;
end;

function TWorkspaceUI.GetClassNameLotus: String;
begin
  Result := 'Notes.NotesUIWorkspace';
end;

procedure TWorkspaceUI.EditDocument(AEditMode: Boolean; ADocumentUI: TDocumentUI);
begin
  try
    Self.LotusObject.EditDocument(AEditMode,ADocumentUI.LotusObject);
  except
    on E: Exception do
      raise Exception.Create('Error Message:' + E.Message);
  end;
end;

{ TDatabaseUI }

constructor TDatabaseUI.Create;
begin
  inherited;

end;

destructor TDatabaseUI.Destroy;
begin

  inherited;
end;

function TDatabaseUI.GetCreateView(const AName: String): TViewUI;
var
  xViewUI: TViewUI;
begin
  try
    xViewUI := TViewUI.Create;
    xViewUI.Name := AName;
    xViewUI.LotusObject := Self.LotusObject.GetView(AName);
    Result := xViewUI;
  except
    on E: Exception do
      raise Exception.Create('Error Message:' + E.Message);
  end;
end;

{ TViewUI }

constructor TViewUI.Create;
begin
  inherited;

end;

destructor TViewUI.Destroy;
begin

  inherited;
end;

procedure TViewUI.Refresh;
begin
  try
    FLotusObject.Refresh;
  except
    on E: Exception do
      raise Exception.Create('Error Message:' + E.Message);
  end;
end;

function TViewUI.GetCreateDocumentByKey(const AKey: String; AExactMatch: Boolean): TDocumentUI;
var
  xDocument: TDocumentUI;
begin
  try
    xDocument := TDocumentUI.Create;
    xDocument.LotusObject := Self.LotusObject.GetDocumentByKey(AKey, AExactMatch);
    Result := xDocument;
  except
    on E: Exception do
      raise Exception.Create('Error Message:' + E.Message);
  end;
end;

{ TDocumentUI }

constructor TDocumentUI.Create;
begin
  inherited;

end;

destructor TDocumentUI.Destroy;
begin

  inherited;
end;

end.
