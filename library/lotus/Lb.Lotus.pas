(*******************************************************************************
  ������������ ��� ������ � �������������� ������
*******************************************************************************)
unit Lb.Lotus;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Generics.Collections,
  Domino_TLB,
  Lotus_TLB;

type
  TDatabase = class;
  TDocumentCollection = class;
  TDocument = class;

  /// <summary>
  /// ������������� ���������� � ��������
  /// </summary>
  TSession = class(TObject)
  private
    FConnected: Boolean;
    FLotusSession: ISession;
    FPassword: String;
    function GetUserName: String;
    function GetServer(Name: String): String;
    function GetPathDB(Name: String): String;
  protected
    function GetEnvironmentString(const AName: String; bisSystem: Boolean = False): String;
    property Session: ISession read FLotusSession;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure SetInitialization;
    /// <summary>
    /// �� ����� ���� ������, ������ ������
    /// </summary>
    property Server[NameDB: String]: String read GetServer;
    /// <summary>
    /// ���� � ���� ������
    /// </summary>
    property PathDB[NameDB: String]: String read GetPathDB;
    property Password: String read FPassword write FPassword;
    property UserName: String read GetUserName;
    property Connected: Boolean read FConnected;
    property LotusSession: ISession read FLotusSession;
  end;

(*******************************************************************************

  ��� ������ ��� ������ �������� ���������� ��� ������ ��������:
  |��� ��������               | ��� ������ ��������
  -----------------------------------------------------------------------------
  |Rich text                  | String. ����� � ����, �������������� � ����
  |                           | �������� ������.
  -----------------------------------------------------------------------------
  |Text or text list (includes| Array of String. ��������� �������� ������������
  |Names, Authors, and Readers| ����� ������ �� 1 ��������.
  |item types)                |
  -----------------------------------------------------------------------------
  |Number or number list      | Array of Doubles. ��������� ��������
  |                           | ������������ ����� ������ �� 1 ��������.
  -----------------------------------------------------------------------------
  |Date-time or range of      | Array of Variant of type DATE.
  |date-time values           | ��������� �������� ������������
  |                           | ����� ������ �� 1 ��������.
  -----------------------------------------------------------------------------
  |Attachment                 | Array of String. ��� �������� � ������ ��������.
  -----------------------------------------------------------------------------

  ��� ������ ��� ������ �������� ���������� ��� ������ ��������:
  |��� ������ ��������        | �������������� NotesItem
  -----------------------------------------------------------------------------
  |String                     | ��������� �������, ���������� ��������
  -----------------------------------------------------------------------------
  |Array of String            | ��������� �������, ����������
  |                           | ������ ������� ��������
  -----------------------------------------------------------------------------
  |Long, Integer, Double,     | �������� �������, ���������� ��������
  |Single, or Currency        |
  -----------------------------------------------------------------------------
  |Array of Long, Integer,    | �������� �������, ���������� ������
  |Double, Single, or Currency| ������� ��������
  -----------------------------------------------------------------------------
  |Variant of type DATE,      | ������� ���� � �������, ���������� ��������
  |NotesDateTime, or          |
  |NotesDateRange.            |
  -----------------------------------------------------------------------------
  |Array of Variant of type   | ������� ���� � �������, ����������
  |DATE,                      | ������ ������� ��������
  |array of NotesDateTime, or |
  |array of NotesDateRange    |
  -----------------------------------------------------------------------------
  |NotesItem                  | �������, ��� ������ �������� �������������
  |                           | ���� NotesItem � ��� �������� �������������
  |                           | ��������� NotesItem
  -----------------------------------------------------------------------------

  ACTIONCD (16) means saved action CD records; non-Computable; canonical form.
  ASSISTANTINFO (17) means saved assistant information; non-Computable; canonical form.
  ATTACHMENT (1084) means file attachment.
  AUTHORS (1076) means authors.
  COLLATION (2).

  Note This value is new with Release 6.

  DATETIMES (1024) means date-time value or range of date-time values.
  EMBEDDEDOBJECT (1090) means embedded object.
  ERRORITEM (256) means an error occurred while accessing the type.
  FORMULA (1536) means Notes formula.
  HTML (21) means HTML source text.
  ICON (6) means icon.
  LSOBJECT (20) means saved LotusScript Object code for an agent.
  MIME_PART (25) means MIME support.
  NAMES (1074) means names.
  NOTELINKS (7) means link to a database, view, or document.
  NOTEREFS (4) means reference to the parent document.
  NUMBERS (768) means number or number list.
  OTHEROBJECT (1085) means other object.
  QUERYCD (15) means saved query CD records; non-Computable; canonical form.
  READERS (1075) means readers.
  RFC822Text (1282) means RFC822 Internet mail text.
  RICHTEXT (1) means rich text.
  SIGNATURE (8) means signature.
  TEXT (1280) means text or text list.
  UNAVAILABLE (512) means the item type isn't available.
  UNKNOWN (0) means the item type isn't known.
  USERDATA (14) means user data.
  USERID (1792) means user ID name.
  VIEWMAPDATA (18) means saved ViewMap dataset; non-Computable; canonical form.
  VIEWMAPLAYOUT (19) means saved ViewMap layout; non-Computable; canonical form.

*******************************************************************************)

  /// <summary>
  /// ������������ ������������ �������� ������ � ���������
  /// </summary>
  TItem = class(TObject)
  public const
    // �������� ����������� ������ �������� CD; ������������; ������������ �����.
    VT_ACTIONCD = 16;
    // �������� ����������� ���������� ���������; ������������; ������������ �����.
    VT_ASSISTANTINFO = 17;
    // �������� �������� �����.
    VT_ATTACHMENT = 1084;
    // ������ ������.
    VT_AUTHORS = 1076;
    // ???
    VT_COLLATION = 2;
    // ����������. ��� ����� �������� � ������ 6.
    // �������� �������� ���� � ������� ��� �������� �������� ���� � �������.
    VT_DATETIMES = 1024;
    // �������� ��������� ������.
    VT_EMBEDDEDOBJECT = 1090;
    // ��������, ��� ��������� ������ ��� ������� � ����.
    VT_ERRORITEM = 256;
    // �������� ���������� �������.
    VT_FORMULA = 1536;
    // �������� �������� ����� HTML.
    VT_HTML = 21;
    // �������� ������.
    VT_ICON = 6;
    // �������� ����������� ��� ������� LotusScript ��� ������.
    VT_LSOBJECT = 20;
    // �������� ��������� MIME.
    VT_MIME_PART = 25;
    // �������� �����.
    VT_NAMES = 1074;
    // �������� ������ �� ���� ������, ������������� ��� ��������.
    VT_NOTELINKS = 7;
    // �������� ������ �� ������������ ��������.
    VT_NOTEREFS = 4;
    // �������� ����� ��� ������ �������.
    VT_NUMBERS = 768;
    // �������� ������ ������.
    VT_OTHEROBJECT = 1085;
    // �������� ����������� ������ ������� CD; ������������; ������������ �����.
    VT_QUERYCD = 15;
    // ������ ��������.
    VT_READERS = 1075;
    // �������� RFC822 ����� ��������-�����.
    VT_RFC822Text = 1282;
    // �������� ������� �����.
    VT_RICHTEXT = 1;
    // �������� �������.
    VT_SIGNATURE = 8;
    // �������� ����� ��� ��������� ������.
    VT_TEXT = 1280;
    // ��������, ��� ��� �������� ����������.
    VT_UNAVAILABLE = 512;
    // ��������, ��� ��� �������� �� ��������.
    VT_UNKNOWN = 0;
    // �������� ���������������� ������.
    VT_USERDATA = 14;
    // �������� ��� ������������.
    VT_USERID = 1792;
    // �������� ����������� ����� ������ ViewMap; ������������; ������������ �����.
    VT_VIEWMAPDATA = 18;
    // �������� ����������� ����� ViewMap; ������������; ������������ �����.
    VT_VIEWMAPLAYOUT = 19;
  private
    FLotusItem: IItem;
    function GetIsEncrypted: Boolean;
    procedure SetIsEncrypted(const Value: Boolean);
    function GetIsNames: Boolean;
    procedure SetIsNames(const Value: Boolean);
    function GetIsSigned: Boolean;
    procedure SetIsSigned(const Value: Boolean);
    function GetIsSummary: Boolean;
    procedure SetIsSummary(const Value: Boolean);
    function GetLastModified: TDateTime;
    function GetName: String;
    function GetText: String;
    function GetValueLength: Integer;
    function GetValues: Variant;
    procedure SetValues(const Value: Variant);
    function GetValueType: Integer;
    function GetParent: IDocument;
  protected
    procedure SetInitilization(const ALotusItem: IItem);
    /// <summary>
    /// ��������, ������� �������� �������
    /// </summary>
    property Parent: IDocument read GetParent;
  public
    constructor Create; virtual;
    destructor Destroy; override;
  public
    /// <summary>
    /// ���������, ���������� �� �������
    /// </summary>
    /// <returns>
    /// ������ ��������, ��� ������� ����������
    /// </returns>
    property IsEncrypted: Boolean read GetIsEncrypted write SetIsEncrypted;
    /// <summary>
    /// ���������, �������� �� ������� ��������� ������.
    /// ������� Names �������� ������ ���� ������������� Notes.
    /// </summary>
    /// <returns>
    /// True ��������, ��� ������� �������� ��������� ����.
    /// False ���������, ��� ������� �� �������� ��������� ����.
    /// </returns>
    property IsNames: Boolean read GetIsNames write SetIsNames;
    /// <summary>
    /// ���������, �������� �� ������� ������� ��� ��������� ������� ���������
    /// </summary>
    /// <remarks>
    /// True ��������, ��� ������� �������� ��� ��������� ������� ���������.
    /// False ���������, ��� ������� �� �������� ��� ��������� ������� ���������.
    /// </remarks>
    property IsSigned: Boolean read GetIsSigned write SetIsSigned;
    /// <summary>
    /// ���������, �������� �� ������� ������� ��� �� ������� ������.
    /// </summary>
    /// <returns>
    /// True ��������, ��� ������� �������� ������� ������.
    /// False ���������, ��� ������� �������� �� ������� ������.
    /// </returns>
    /// <remarks>
    /// �������� �������� ��� ���������� ������� ��� �� ������� ������.
    /// ������� ������ ����� ���������� � �������������� � ������;
    /// �� ������� ������ �� �����. ��� �������, ��������, ���������
    /// � ������� ����������������� ����������, ���������� ��� �� �������,
    /// ���� ��� �������� ����������� ����� ��� ����� ����� ������� ������
    /// </remarks>
    property IsSummary: Boolean read GetIsSummary write SetIsSummary;
    /// <summary>
    /// ���� ���������� ��������� ��������
    /// </summary>
    property LastModified: TDateTime read GetLastModified;
    /// <summary>
    /// �������� ��������
    /// </summary>
    /// <remarks>
    /// � ��������� ����� ���� ��������� ��������� � ���������� ������
    /// </remarks>
    property Name: String read GetName;
    /// <summary>
    /// ��������� ������������� �������� ��������
    /// </summary>
    /// <remarks>
    /// <para>
    /// ��������� �������� � ������ ����������� ������ � ������� �
    /// ������������ ������. ���� �������� �������� �������, ������������
    /// ������ ����� ���� �������.
    /// </para>
    /// <para>
    /// ��� ��������� ������������ ������ ��� �������� ���������� �����������
    /// ������, ����� ��� ��������� ����������� � �������� ������.
    /// </para>
    /// <para>
    /// ��� ��������� HTML ��� �������� ���������� �������� Null
    /// </para>
    /// </remarks>
    property Text: String read GetText;
    /// <summary>
    /// ���������� ������ � ���� ������, ������������ ��� ��������
    /// �������� ��������. ���� ����� �������� ���������� ��������� �������
    /// </summary>
    property ValueLength: Integer read GetValueLength;
    /// <summary>
    /// ��������, ������� �������� �������
    /// </summary>
    /// <remarks>
    /// ��� ������, �����, ��������� ���� � ������� � �������� ��� ��������
    /// ������ ���������� ������, ���� ���� � �������� ���� ������ ����
    /// ��������. ���� �� ������, ��� ������� �������� ������ ���� ��������,
    /// ���������� � ������� �������� � ������� � �������� 0. ���� �� ������,
    /// ��� ������� �������� ��������� ��������, �� �� ������, ������� ��,
    /// ��������� �������� �� ������� � ������� Forall ���������.
    /// </remarks>
    property Values: Variant read GetValues write SetValues;
    /// <summary>
    /// ��� ������ ��������
    /// </summary>
    property ValueType: Integer read GetValueType;
  public
    /// <summary>
    /// ��� ������
    /// </summary>
    class function GetValueTypeToStr(const ValueType: Integer): String;
    class function GetStrToValueType(const Value: String): Integer;
  end;
  
  /// <summary>
  /// ������ � ���������
  /// </summary>
  TDocument = class(TObject)
  public type
    TArrayIItem = TArray<IItem>;
  private
    FItem: TItem;
    FLotusDocument: IDocument;
    FItems: TArrayIItem;
    //FIsInitilization: Boolean;
    function GetIsDeleted: Boolean;
    procedure CleaeItems;
    function GetCount: Integer;
    function GetItems(Index: Integer): TItem;
    function GetValues(Name: String): String;
    function GetUniversalID: String;
    function GetLastModified: TDateTime;
    function GetVarValues(Name: String): Variant;
    //procedure SetIsInitilization(const Value: Boolean);
  protected
    procedure SetInitilization(const ALotusDocument: IDocument);

  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure SetFileObject(const AFileName: String; const AFieldName: String);
    procedure SetFieldNames(const AStrings: TStrings);
    procedure SetFieldValues(const AStrings: TStrings);
    property Values[Name: String]: String read GetValues;
    property VarValues[Name: String]: Variant read GetVarValues;
    property Items[Index: Integer]: TItem read GetItems;
    property Count: Integer read GetCount;
    property UniversalID: String read GetUniversalID;
    property LotusDocument: IDocument read FLotusDocument;
  public
    function GetIsInitilization: Boolean;
    procedure Save;
    /// <summary>
    ///  ���������, ��� �� �������� ������ ��� ���
    /// </summary>
    /// <returns>
    /// True ��������, ��� �������� �������� ��������� ��� ��������.
    /// False ���������, ��� �������� ����������.
    /// </returns>
    property IsDeleted: Boolean read GetIsDeleted;
    /// <summary>
    /// ��������� �����������
    /// </summary>
    property LastModified: TDateTime read GetLastModified;
  public
    procedure ReplaceItemValue(const AName: String; const AValue: String); overload;
    procedure ReplaceItemValue(const AName: String; const AValue: Integer); overload;
    procedure ReplaceItemValue(const AName: String; const AValue: Double); overload;
  end;

  /// <summary>
  /// ������������ ��������� ���������� �� ���� ������, ���������
  /// � ������������ � ������������� ����������
  /// </summary>
  /// <remarks>
  /// �������� ����������� ��������� �����������:
  /// https://www.ibm.com/support/knowledgecenter/en/SSVRGU_9.0.1/basic/H_NOTESDOCUMENTCOLLECTION_CLASS.html
  /// </remarks>
  TDocumentCollection = class(TObject)
  private
    FEOF: Boolean;
    FBOF: Boolean;
    FDatabase: TDatabase;
    FDocument: TDocument;
    FLotusDocumentCollection: IDocumentCollection;
    function GetCount: Integer;
    function GetIsSorted: Boolean;
    function GetParent: IDatabase;
    function GetQuery: String;
  protected
    property LotusDocumentCollection: IDocumentCollection read FLotusDocumentCollection;
  public
    constructor Create(const ADatabase: TDatabase); reintroduce;
    destructor Destroy; override;
    /// <summary>
    /// �������� ��� �������
    /// </summary>
    procedure AllSelectDocumentCollection;
    /// <summary>
    /// �������� �� ������ �����������
    /// </summary>
    procedure LastModifiedDocumentCollection(const ALastModified: String);
    /// <summary>
    /// ������ ����������, ���������� ����������
    /// </summary>
    procedure SearchDocumentCollection(const ASearch: String);
  public
    /// <summary>
    /// ���������� ���������� � ���������
    /// </summary>
    property Count: Integer read GetCount;
    /// <summary>
    /// ���������, ������������� �� ��������� � ���������.
    /// ��������� ����������� ������ �����, ����� ��� �������� �����������
    /// ��������������� ������ � ���� ������
    /// </summary>
    /// <remarks>
    /// ������ ��������, ��� ��������� � ��������� �������������
    /// False ���������, ��� ��������� � ��������� �� �������������
    /// </remarks>
    property IsSorted: Boolean read GetIsSorted;
    /// <summary>
    /// ���� ������, ������� �������� ��������� ����������
    /// </summary>
    property Parent: IDatabase read GetParent;
    property Database: TDatabase read FDatabase;
    /// <summary>
    /// ����� �������, ���������������� �������� ����������,
    /// ���� �������� �������� � ���������� ��������������� ��� ������� ������
    /// </summary>
    /// <remarks>
    /// ��� ���������, ��������� ��� ������ (��������, � ��������������
    /// �������� AllDocuments � NotesDatabase), ��� ��������
    /// ���������� ������ ������ ("")
    /// </remarks>
    property Query: String read GetQuery;
  public
    /// <summary>
    /// �������� ������ �������� � ���������
    /// </summary>
    /// <remarks>
    /// ������ �������� � ���������. ���� � ��������� ��� ����������,
    /// ������ �� ������������
    /// </remarks>
    procedure First;
    /// <summary>
    /// �������� ��������� �������� � ���������
    /// </summary>
    /// <remarks>
    /// ��������� �������� � ���������. ���� � ��������� ��� ����������,
    /// ������ �� ������������.
    /// </remarks>
    procedure Last;
    /// <summary>
    /// ������ �������� ������� ��������, ��������� ����� �� ��� � ���������.
    /// </summary>
    /// <returns>
    /// ��������, ��������� �� ���������� notesDocument � ���������.
    /// ���� ���������� ��������� ���, ������ �� ������������.
    /// </returns>
    procedure Next;
    /// <summary>
    /// ������� ��������, ������� ��������, ���������������
    /// �������������� ���, � ���������.
    /// </summary>
    /// <returns>
    /// ��������, �������������� ���������� ��������� � ���������.
    /// ���� ����������� ��������� ���, ������ �� ������������.
    /// </returns>
    procedure Prev;
    property EOF: Boolean read FEOF;
    property BOF: Boolean read FBOF;
    property Document: TDocument read FDocument;
  end;

  /// <summary>
  /// ������ ��� ������
  /// </summary>
  TView = class(TObject)
  private
    FDatabase: TDatabase;
    FLotusView: IView;
    FName: String;
    function GetIsOpen: Boolean;
  public
    constructor Create(const ADatabase: TDatabase); reintroduce;
    destructor Destroy; override;
    procedure Open;
    function GetDocumentByKey(const AFind: String): TDocument;
    property IsOpen: Boolean read GetIsOpen;
    property Name: String read FName write FName;
    property LotusView: IView read FLotusView;
  end;

  /// <summary>
  /// ������������ � ����
  /// </summary>
  TDatabase = class(TObject)
  private
    FServer, FPathDB: String;
    FSession: TSession;
    FNameDataBase: String;
    FLotusDataBase: IDatabase;
    FDocument: TDocument;
    FDocuments: TDocumentCollection;
    function GetReplicaID: String;
  public
    constructor Create(const ASession: TSession; const ANameDataBase: String); overload;
    constructor Create(const ASession: TSession; const ANameDataBase, AServer, APathDB: String); overload;
    destructor Destroy; override;
    property Session: TSession read FSession;
    property NameDataBase: String read FNameDataBase;
    function GetDocumentByUNID(const AUnid: WideString): TDocument;
    property Documents: TDocumentCollection read FDocuments;
    property LotusDataBase: IDatabase read FLotusDataBase;
    property Server: String read FServer;
    property PathDB: String read FPathDB;
    property ReplicaID: String read GetReplicaID;
  end;

implementation

uses
  Lb.Logger;

{ TSession }

constructor TSession.Create;
begin
  FLotusSession := CoNotesSession.Create;
  FPassword := '';
  FConnected := False;
end;

destructor TSession.Destroy;
begin
  FLotusSession := nil;
  inherited;
end;

function TSession.GetUserName: String;
begin
  Result := '';
  if FConnected then
    Result := FLotusSession.UserName;
end;

procedure TSession.SetInitialization;
begin
  if Assigned(FLotusSession) then
  begin
    try
      if FPassword.IsEmpty then
        FLotusSession.Initialize('')
      else
        FLotusSession.Initialize(WideString(FPassword));
      FConnected := True;
    except
      on E: Exception do
      begin
        raise Exception.Create('Error Message: [' + E.ClassName + ']' + E.Message);
        FConnected := False;
      end;
    end;
  end;
end;

function TSession.GetEnvironmentString(const AName: String; bisSystem: Boolean): String;
begin
  Result := '';
  if FConnected then
    Result := FLotusSession.GetEnvironmentString(WideString(AName),WordBool(bisSystem));
end;


function TSession.GetServer(Name: String): String;
begin
  Result := Self.GetEnvironmentString('SRV' + Name);
end;

function TSession.GetPathDB(Name: String): String;
begin
  Result := Self.GetEnvironmentString('DB' + Name);
end;

{ TDatabase }

constructor TDatabase.Create(const ASession: TSession; const ANameDataBase: String);
begin
  FSession := ASession;
  FNameDataBase := ANameDataBase;
  if FSession.Connected then
  begin
    FServer := FSession.Server[FNameDataBase];
    FPathDB := FSession.PathDB[FNameDataBase];
    FLotusDataBase := FSession.Session.GetDatabase(WideString(FServer), WideString(FPathDB),False);
    if not Assigned(FLotusDataBase) then
      raise Exception.Create('Error Message: ��� ���������� � Lotus ����');
  end
  else
    raise Exception.Create('Error Message: ���������� �� ����������� � Lotus');
  FDocuments := TDocumentCollection.Create(Self);
  FDocument := TDocument.Create;
end;

constructor TDatabase.Create(const ASession: TSession; const ANameDataBase, AServer, APathDB: String);
begin
  FServer := AServer;
  FPathDB := APathDB;

  FSession := ASession;
  FNameDataBase := ANameDataBase;
  if FSession.Connected then
  begin
    FLotusDataBase := FSession.Session.GetDatabase(WideString(AServer), WideString(APathDB),False);
    if not Assigned(FLotusDataBase) then
      raise Exception.Create('Error Message: ��� ���������� � Lotus ����');
  end
  else
    raise Exception.Create('Error Message: ���������� �� ����������� � Lotus');
  FDocuments := TDocumentCollection.Create(Self);
  FDocument := TDocument.Create;
end;

destructor TDatabase.Destroy;
begin
  FreeAndNil(FDocument);
  FreeAndNil(FDocuments);
  FLotusDataBase := nil;
  inherited;
end;

function TDatabase.GetDocumentByUNID(const AUnid: WideString): TDocument;
var
  xLotusDocument: IDocument;
begin
  xLotusDocument := FLotusDataBase.GetDocumentByUNID(AUnid);
  FDocument.SetInitilization(xLotusDocument);
  if FDocument.GetIsInitilization then
    Result := FDocument
  else
    Result := nil;
end;

function TDatabase.GetReplicaID: String;
begin
  Result := '';
  if Assigned(FLotusDataBase) then
    Result := FLotusDataBase.ReplicaID;
end;

{ TItem }

constructor TItem.Create;
begin
  FLotusItem := nil;
end;

destructor TItem.Destroy;
begin
  FLotusItem := nil;
  inherited;
end;

procedure TItem.SetInitilization(const ALotusItem: IItem);
begin
  if Assigned(FLotusItem) then FLotusItem := nil;
  FLotusItem := ALotusItem;
end;

function TItem.GetIsEncrypted: Boolean;
begin
  if Assigned(FLotusItem) then
    Result := FLotusItem.IsEncrypted
  else
    raise Exception.Create('Error Message: IsEncrypted');
end;

procedure TItem.SetIsEncrypted(const Value: Boolean);
begin
  if Assigned(FLotusItem) then
    FLotusItem.IsEncrypted := Value
  else
    raise Exception.Create('Error Message: IsEncrypted');
end;

function TItem.GetIsNames: Boolean;
begin
  if Assigned(FLotusItem) then
    Result := FLotusItem.IsNames
  else
    raise Exception.Create('Error Message: IsNames');
end;

procedure TItem.SetIsNames(const Value: Boolean);
begin
  if Assigned(FLotusItem) then
    FLotusItem.IsNames := Value
  else
    raise Exception.Create('Error Message: IsNames');
end;

function TItem.GetIsSigned: Boolean;
begin
  if Assigned(FLotusItem) then
    Result := FLotusItem.IsSigned
  else
    raise Exception.Create('Error Message: IsSigned');
end;

procedure TItem.SetIsSigned(const Value: Boolean);
begin
  if Assigned(FLotusItem) then
    FLotusItem.IsSigned := Value
  else
    raise Exception.Create('Error Message: IsNames');
end;

function TItem.GetIsSummary: Boolean;
begin
  if Assigned(FLotusItem) then
    Result := FLotusItem.IsSummary
  else
    raise Exception.Create('Error Message: IsSummary');
end;

procedure TItem.SetIsSummary(const Value: Boolean);
begin
  if Assigned(FLotusItem) then
    FLotusItem.IsSummary := Value
  else
    raise Exception.Create('Error Message: IsSummary');
end;

function TItem.GetLastModified: TDateTime;
begin
  if Assigned(FLotusItem) then
    Result := FLotusItem.LastModified
  else
    raise Exception.Create('Error Message: IsSummary');
end;

function TItem.GetName: String;
var
  xS: String;
begin
  if Assigned(FLotusItem) then
  begin
    xS := FLotusItem.Name;
    Result := AnsiLowerCase(xS);
  end
  else
    raise Exception.Create('Error Message: Name');
end;

function TItem.GetParent: IDocument;
begin
  if Assigned(FLotusItem) then
    Result := FLotusItem.Parent
  else
    raise Exception.Create('Error Message: Parent');
end;


function TItem.GetText: String;
begin
  if Assigned(FLotusItem) then
    Result := FLotusItem.Text
  else
    raise Exception.Create('Error Message: Text');
end;

function TItem.GetValueLength: Integer;
begin
  if Assigned(FLotusItem) then
    Result := FLotusItem.ValueLength
  else
    raise Exception.Create('Error Message: ValueLength');
end;

function TItem.GetValues: Variant;
begin
  if Assigned(FLotusItem) then
    Result := FLotusItem.Values
  else
    raise Exception.Create('Error Message: Values');
end;

procedure TItem.SetValues(const Value: Variant);
begin
  if Assigned(FLotusItem) then
    FLotusItem.Values := Value
  else
    raise Exception.Create('Error Message: Values');
end;

function TItem.GetValueType: Integer;
begin
  if Assigned(FLotusItem) then
    Result := FLotusItem.type_
  else
    raise Exception.Create('Error Message: ValueType');
end;

class function TItem.GetValueTypeToStr(const ValueType: Integer): String;
begin
  case ValueType of
    TItem.VT_ACTIONCD: Result := 'vt_actioncd';
    TItem.VT_ASSISTANTINFO: Result := 'vt_assistantinfos';
    TItem.VT_ATTACHMENT: Result := 'vt_attachment';
    TItem.VT_AUTHORS: Result := 'vt_authors';
    TItem.VT_COLLATION: Result := 'vt_collation';
    TItem.VT_DATETIMES: Result := 'vt_datetimes';
    TItem.VT_EMBEDDEDOBJECT: Result := 'vt_embeddedobject';
    TItem.VT_ERRORITEM: Result := 'vt_erroritem';
    TItem.VT_FORMULA: Result := 'vt_formula';
    TItem.VT_HTML: Result := 'vt_html';
    TItem.VT_ICON: Result := 'vt_icon';
    TItem.VT_LSOBJECT: Result := 'vt_lsobject';
    TItem.VT_MIME_PART: Result := 'vt_mime_part';
    TItem.VT_NAMES: Result := 'vt_names';
    TItem.VT_NOTELINKS: Result := 'vt_notelinks';
    TItem.VT_NOTEREFS: Result := 'vt_noterefs';
    TItem.VT_NUMBERS: Result := 'vt_numbers';
    TItem.VT_OTHEROBJECT: Result := 'vt_otherobject';
    TItem.VT_QUERYCD: Result := 'vt_querycd';
    TItem.VT_READERS: Result := 'vt_readers';
    TItem.VT_RFC822Text: Result := 'vt_rfc822text';
    TItem.VT_RICHTEXT: Result := 'vt_richtext';
    TItem.VT_SIGNATURE: Result := 'vt_signature';
    TItem.VT_TEXT: Result := 'vt_text';
    TItem.VT_UNAVAILABLE: Result := 'vt_unavailable';
    TItem.VT_UNKNOWN: Result := 'vt_unknown';
    TItem.VT_USERDATA: Result := 'vt_userdata';
    TItem.VT_USERID: Result := 'vt_userid';
    TItem.VT_VIEWMAPDATA: Result := 'vt_viewmapdata';
    TItem.VT_VIEWMAPLAYOUT: Result := 'vt_viewmaplayout';
  else
    Result := 'vt_null';
  end;
end;

class function TItem.GetStrToValueType(const Value: String): Integer;
begin
  if SameText(Value,'vt_actioncd') then Result := TItem.VT_ACTIONCD
  else if SameText(Value,'vt_assistantinfos') then Result := TItem.VT_ASSISTANTINFO
  else if SameText(Value,'vt_attachment') then Result :=  TItem.VT_ATTACHMENT
  else if SameText(Value,'vt_authors') then Result := TItem.VT_AUTHORS
  else if SameText(Value,'vt_collation') then Result := TItem.VT_COLLATION
  else if SameText(Value,'vt_datetimes') then Result := TItem.VT_DATETIMES
  else if SameText(Value,'vt_embeddedobject') then Result := TItem.VT_EMBEDDEDOBJECT
  else if SameText(Value,'vt_erroritem') then Result := TItem.VT_ERRORITEM
  else if SameText(Value,'vt_formula') then Result := TItem.VT_FORMULA
  else if SameText(Value,'vt_html') then Result := TItem.VT_HTML
  else if SameText(Value,'vt_icon') then Result := TItem.VT_ICON
  else if SameText(Value,'vt_lsobject') then Result := TItem.VT_LSOBJECT
  else if SameText(Value,'vt_mime_part') then Result := TItem.VT_MIME_PART
  else if SameText(Value,'vt_names') then Result := TItem.VT_NAMES
  else if SameText(Value,'vt_notelinks') then Result := TItem.VT_NOTELINKS
  else if SameText(Value,'vt_noterefs') then Result := TItem.VT_NOTEREFS
  else if SameText(Value,'vt_numbers') then Result := TItem.VT_NUMBERS
  else if SameText(Value,'vt_otherobject') then Result := TItem.VT_OTHEROBJECT
  else if SameText(Value,'vt_querycd') then Result := TItem.VT_QUERYCD
  else if SameText(Value,'vt_readers') then Result := TItem.VT_READERS
  else if SameText(Value,'vt_rfc822text') then Result := TItem.VT_RFC822Text
  else if SameText(Value,'vt_richtext') then Result := TItem.VT_RICHTEXT
  else if SameText(Value,'vt_signature') then Result := TItem.VT_SIGNATURE
  else if SameText(Value,'vt_text') then Result := TItem.VT_TEXT
  else if SameText(Value,'vt_unavailable') then Result := TItem.VT_UNAVAILABLE
  else if SameText(Value,'vt_unknown') then Result := TItem.VT_UNKNOWN
  else if SameText(Value,'vt_userdata') then Result := TItem.VT_USERDATA
  else if SameText(Value,'vt_userid') then Result := TItem.VT_USERID
  else if SameText(Value,'vt_viewmapdata') then Result :=  TItem.VT_VIEWMAPDATA
  else if SameText(Value,'vt_viewmaplayout') then Result := TItem.VT_VIEWMAPLAYOUT
  else Result := -1;
end;


{ TDocument }

constructor TDocument.Create;
begin
  SetLength(FItems,0);
  FLotusDocument := nil;
  FItem := TItem.Create;
end;

destructor TDocument.Destroy;
begin
  FLotusDocument := nil;
  Self.CleaeItems;
  FreeAndNil(FItem);
  inherited;
end;

procedure TDocument.SetFileObject(const AFileName, AFieldName: String);
var
  xLotusRichTextItem: IRichTextItem;
begin
  try
    //FLotusDocument.ReplaceItemValue(AFieldName,AFileName);
    xLotusRichTextItem := IRichTextItem(FLotusDocument.GetFirstItem(AFieldName));
    xLotusRichTextItem.EmbedObject(EMBED_ATTACHMENT,'',AFileName,'');
  finally
    xLotusRichTextItem := nil;
  end;
end;

procedure TDocument.SetInitilization(const ALotusDocument: IDocument);
begin
  if Assigned(FLotusDocument) then
  begin
    FLotusDocument := nil;
    Self.CleaeItems;
  end;
  FLotusDocument := ALotusDocument;
  FItems := TArrayIItem(FLotusDocument.Items);
end;

procedure TDocument.Save;
begin
  if Assigned(FLotusDocument) then
    FLotusDocument.Save(False,True,True)
end;

procedure TDocument.SetFieldNames(const AStrings: TStrings);
var
  xItem: TItem;
  i, Count: Integer;
begin
  if Assigned(AStrings) then
  begin
    Count := Self.Count;
    if Count > 0 then
      for i := 0 to Count - 1 do
      begin
        xItem := Self.Items[i];
        AStrings.Add(xItem.Name);
      end;
  end;
end;

procedure TDocument.SetFieldValues(const AStrings: TStrings);
var
  xItem: TItem;
  i, Count: Integer;
begin
  if Assigned(AStrings) then
  begin
    AStrings.Clear;
    Count := Self.Count;
    if Count > 0 then
      for i := 0 to Count - 1 do
      begin
        xItem := Self.Items[i];
        AStrings.Add(xItem.Name + '=' + xItem.Text);
      end;
  end;
end;

//procedure TDocument.SetIsInitilization(const Value: Boolean);
//begin
//  FIsInitilization := Value;
//end;

procedure TDocument.CleaeItems;
var
  i: Integer;
begin
  for i := Low(FItems) to High(FItems) do
    FItems[i] := nil;
  SetLength(FItems,0);  
end;

function TDocument.GetIsDeleted: Boolean;
begin
  Result := False;
  if Assigned(FLotusDocument) then
    Result := FLotusDocument.IsDeleted;
end;

function TDocument.GetIsInitilization: Boolean;
begin
  Result := Assigned(FLotusDocument);
end;

function TDocument.GetCount: Integer;
begin
  Result := Length(FItems);
end;

function TDocument.GetItems(Index: Integer): TItem;
begin
  FItem.SetInitilization(FItems[Index]);
  Result := FItem;
end;

function TDocument.GetLastModified: TDateTime;
var
  // xDateTime: IDateTime;
  xV: OleVariant;
begin
  Result := 0;
  if Assigned(FLotusDocument) then
  begin
    xV := FLotusDocument.LastModified;
    Result := xV;
  end;
end;

function TDocument.GetUniversalID: String;
begin
  Result := '';
  if Assigned(FLotusDocument) then
    Result := FLotusDocument.UniversalID;
end;

function TDocument.GetValues(Name: String): String;
var
  xNameValue: String;
  i, Count: Integer;
begin
  Result := '';
  Count := Self.Count;
  if Count > 0 then
    for i := 0 to Count - 1 do
    begin
      xNameValue := Self.Items[i].Name;
      if SameText(Name,xNameValue) then
      begin
        if Self.Items[i].GetValueType = TItem.VT_TEXT then
          Result := Self.Items[i].Values[0]
        else
          Result := Self.Items[i].Text;
        Break;
      end;
    end;
end;

function TDocument.GetVarValues(Name: String): Variant;
var
  xNameValue: String;
  i, Count: Integer;
begin
  Result := varNull;
  Count := Self.Count;
  if Count > 0 then
    for i := 0 to Count - 1 do
    begin
      xNameValue := Self.Items[i].Name;
      if SameText(Name,xNameValue) then
      begin
        Result := Self.Items[i].Values;
        Break;
      end;
    end;
end;

procedure TDocument.ReplaceItemValue(const AName, AValue: String);
begin
  if Assigned(FLotusDocument) then
    FLotusDocument.ReplaceItemValue(AName,AValue);
end;

procedure TDocument.ReplaceItemValue(const AName: String; const AValue: Double);
begin
  if Assigned(FLotusDocument) then
    FLotusDocument.ReplaceItemValue(AName,AValue);
end;

procedure TDocument.ReplaceItemValue(const AName: String; const AValue: Integer);
begin
  if Assigned(FLotusDocument) then
    FLotusDocument.ReplaceItemValue(AName,AValue);
end;

{ TDocumentCollection }

constructor TDocumentCollection.Create(const ADatabase: TDatabase);
begin
  FEOF := False;
  FBOF := False;
  FDatabase := ADatabase;
  FDocument := TDocument.Create;
end;

procedure TDocumentCollection.AllSelectDocumentCollection;
begin
  FLotusDocumentCollection := nil;
  if Assigned(FDatabase) then
    FLotusDocumentCollection := FDatabase.LotusDataBase.AllDocuments
  else
    raise Exception.Create('Error Message: ���� �� �����������');
end;

procedure TDocumentCollection.LastModifiedDocumentCollection(const ALastModified: String);
var
//  xS: String;
  xDateTime: IDateTime;
begin
  // @LastModified > @TextToTime("10.10.2019")
  FLotusDocumentCollection := nil;
  if Assigned(FDatabase) then
  begin
    xDateTime := FDatabase.Session.Session.CreateDateTime(ALastModified);
    try
      FLotusDocumentCollection := FDatabase.LotusDataBase.Search('@Modified >= @TextToTime("'+ ALastModified + '")',xDateTime,0);
    finally
      xDateTime := nil;
    end;
  end
  else
    raise Exception.Create('Error Message: ���� �� �����������');
end;

procedure TDocumentCollection.SearchDocumentCollection(const ASearch: String);
begin
  FLotusDocumentCollection := nil;
  if Assigned(FDatabase) then
  begin
    FLotusDocumentCollection := FDatabase.LotusDataBase.Search(ASearch,nil,0);
  end
  else
    raise Exception.Create('Error Message: ���� �� �����������');
end;

destructor TDocumentCollection.Destroy;
begin
  FreeAndNil(FDocument);
  inherited;
end;

function TDocumentCollection.GetCount: Integer;
begin
  Result := 0;
  if Assigned(FLotusDocumentCollection) then
    Result := FLotusDocumentCollection.Count;
end;

function TDocumentCollection.GetIsSorted: Boolean;
begin
  Result := False;
  if Assigned(FLotusDocumentCollection) then
    Result := FLotusDocumentCollection.IsSorted;
end;

function TDocumentCollection.GetParent: IDatabase;
begin
  Result := nil;
  if Assigned(FLotusDocumentCollection) then
    Result := FLotusDocumentCollection.Parent;
end;

function TDocumentCollection.GetQuery: String;
begin
  Result := '';
  if Assigned(FLotusDocumentCollection) then
    Result := FLotusDocumentCollection.Query;
end;

procedure TDocumentCollection.First;
begin
  if Assigned(FLotusDocumentCollection) then
  begin
    FEOF := False;
    FBOF := True;
    FDocument.SetInitilization(FLotusDocumentCollection.GetFirstDocument);
  end;
end;

procedure TDocumentCollection.Last;
begin
  if Assigned(FLotusDocumentCollection) then
  begin
    FEOF := True;
    FBOF := False;
    FDocument.SetInitilization(FLotusDocumentCollection.GetLastDocument);
  end;
end;


procedure TDocumentCollection.Next;
var
  xDocument: IDocument;
begin
  if Assigned(FLotusDocumentCollection) then
  begin
    xDocument := FLotusDocumentCollection.GetNextDocument(FDocument.LotusDocument);
    if Assigned(xDocument) then
    begin
      FEOF := False;
      FBOF := False;
      FDocument.SetInitilization(xDocument);
    end
    else
    begin
      FEOF := True;
      FBOF := False;
    end;
  end;
end;

procedure TDocumentCollection.Prev;
var
  xDocument: IDocument;
begin
  if Assigned(FLotusDocumentCollection) then
  begin
    xDocument := FLotusDocumentCollection.GetPrevDocument(FDocument.LotusDocument);
    if Assigned(xDocument) then
    begin
      FEOF := False;
      FBOF := False;
      FDocument.SetInitilization(xDocument);
    end
    else
    begin
      FEOF := False;
      FBOF := True;
    end;
  end;
end;

{ TView }

constructor TView.Create(const ADatabase: TDatabase);
begin
  FDatabase := ADatabase;
end;

destructor TView.Destroy;
begin
  FDatabase := nil;
  FLotusView := nil;
  inherited;
end;

function TView.GetIsOpen: Boolean;
begin
  Result := Assigned(FLotusView);
end;

procedure TView.Open;
begin
  if not FName.IsEmpty then
    FLotusView := FDatabase.LotusDataBase.GetView(FName);
end;

function TView.GetDocumentByKey(const AFind: String): TDocument;
var
  xDocument: TDocument;
  xLotusDocumnet: IDocument;
begin
  Result := nil;
  if Self.IsOpen then
  begin
    xDocument := TDocument.Create;
    xLotusDocumnet := FLotusView.GetDocumentByKey(AFind,True);
    xDocument.SetInitilization(xLotusDocumnet);
    Result := xDocument;
  end;
end;

end.
