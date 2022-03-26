unit Lb.Document.DataBase;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  System.Variants;

type
  ///<summary>Базовый объект</summary>
  TCustomObjectDataBase = class(TObject)
  private
    FObjectID: String;
    FParentID: String;
  protected
    property ObjectID: String read FObjectID write FObjectID;
    property ParentID: String read FParentID write FParentID;
  public
    constructor Create;
    destructor Destroy; override;
  end;


  TDocument = class;
  TFolder = class;

  TDocumentList = TObjectList<TDocument>;
  TFolderList = TObjectList<TFolder>;

  ///<summary>Документ</summary>
  TDocument = class(TCustomObjectDataBase)
  public type

    // Базовый объект, для все типов
    TValue<T> = class(TObject)
    private
      FValue: T;
    public
      property Value: T read FValue write FValue;
    end;

    TStringValue    = TValue<String>;
    TIntegerValue   = TValue<Integer>;
    TInt64Value     = TValue<Int64>;
    TDoubleValue    = TValue<Double>;
    TBooleanValue   = TValue<Boolean>;
    TDateTimeValue  = TValue<TDateTime>;
    TDocumentValue  = TValue<TDocument>;
    TDocumentsValue = TValue<TDocumentList>;

    TParam = class(TObject)
    private
      FName: String;
      FValueObject: TObject;
    private
      function GetAsString: String;
      procedure SetAsString(const Value: String);
      function GetAsInteger: Integer;
      procedure SetAsInteger(const Value: Integer);
      function GetAsInt64: Int64;
      procedure SetAsInt64(const Value: Int64);
      function GetAsDouble: Double;
      procedure SetAsDouble(const Value: Double);
      function GetAsBoolean: Boolean;
      procedure SetAsBoolean(const Value: Boolean);
      function GetAsDateTime: TDateTime;
      procedure SetAsDateTime(const Value: TDateTime);
      function GetAsDate: TDateTime;
      procedure SetAsDate(const Value: TDateTime);
      function GetAsTime: TDateTime;
      procedure SetAsTime(const Value: TDateTime);
      function GetAsDocument: TDocument;
      procedure SetAsDocument(const Value: TDocument);
      function GetAsDocuments: TDocumentList;
      procedure SetAsDocuments(const Value: TDocumentList);
    private
      function GetIsNull: Boolean;
    public
      constructor Create;
      destructor Destroy; override;
      property Name: String read FName write FName;
      property IsNull: Boolean read GetIsNull;
    public
      property AsString: String read GetAsString write SetAsString;
      property AsInteger: Integer read GetAsInteger write SetAsInteger;
      property AsInt64: Int64 read GetAsInt64 write SetAsInt64;
      property AsDouble: Double read GetAsDouble write SetAsDouble;
      property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;
      property AsDateTime: TDateTime read GetAsDateTime write SetAsDateTime;
      property AsDate: TDateTime read GetAsDate write SetAsDate;
      property AsTime: TDateTime read GetAsTime write SetAsTime;
      property AsDocument: TDocument read GetAsDocument write SetAsDocument;
      property AsDocuments: TDocumentList read GetAsDocuments write SetAsDocuments;
    end;

    TParamList = TObjectList<TParam>;

  private
  public
    constructor Create;
    destructor Destroy; override;
  end;

  ///<summary>Папка</summary>
  TFolder = class(TCustomObjectDataBase)
  private
    FDocuments: TDocumentList;
    FFolders: TFolderList;
  public
    constructor Create;
    destructor Destroy; override;
    property Documents: TDocumentList read FDocuments;
    property Folders: TFolderList read FFolders;
  end;

implementation

{ TCustomObjectDataBase }

constructor TCustomObjectDataBase.Create;
begin
  FObjectID := '';
  FParentID := '';
end;

destructor TCustomObjectDataBase.Destroy;
begin

  inherited;
end;

{ TDocument.TParam }

constructor TDocument.TParam.Create;
begin
  FName := '';
  FValueObject := nil;
end;

destructor TDocument.TParam.Destroy;
begin

  inherited;
end;

function TDocument.TParam.GetIsNull: Boolean;
begin
  Result := not Assigned(FValueObject);
end;

function TDocument.TParam.GetAsString: String;
begin

end;

procedure TDocument.TParam.SetAsString(const Value: String);
begin

end;

function TDocument.TParam.GetAsInteger: Integer;
begin

end;

procedure TDocument.TParam.SetAsInteger(const Value: Integer);
begin

end;

function TDocument.TParam.GetAsInt64: Int64;
begin

end;

procedure TDocument.TParam.SetAsInt64(const Value: Int64);
begin

end;

function TDocument.TParam.GetAsDouble: Double;
begin

end;

procedure TDocument.TParam.SetAsDouble(const Value: Double);
begin

end;

function TDocument.TParam.GetAsBoolean: Boolean;
begin

end;

procedure TDocument.TParam.SetAsBoolean(const Value: Boolean);
begin

end;

function TDocument.TParam.GetAsDateTime: TDateTime;
begin

end;

procedure TDocument.TParam.SetAsDateTime(const Value: TDateTime);
begin

end;

function TDocument.TParam.GetAsDate: TDateTime;
begin

end;

procedure TDocument.TParam.SetAsDate(const Value: TDateTime);
begin

end;

function TDocument.TParam.GetAsTime: TDateTime;
begin

end;


procedure TDocument.TParam.SetAsTime(const Value: TDateTime);
begin

end;

function TDocument.TParam.GetAsDocument: TDocument;
begin

end;

procedure TDocument.TParam.SetAsDocument(const Value: TDocument);
begin

end;

function TDocument.TParam.GetAsDocuments: TDocumentList;
begin

end;

procedure TDocument.TParam.SetAsDocuments(const Value: TDocumentList);
begin

end;

{ TDocument }

constructor TDocument.Create;
begin
  inherited Create;

end;

destructor TDocument.Destroy;
begin

  inherited;
end;

{ TFolder }

constructor TFolder.Create;
begin
  inherited Create;
  FDocuments := TDocumentList.Create;
  FFolders := TFolderList.Create;
end;

destructor TFolder.Destroy;
begin
  FreeAndNil(FFolders);
  FreeAndNil(FDocuments);
  inherited;
end;



end.
