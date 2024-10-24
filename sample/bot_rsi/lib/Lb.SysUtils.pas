unit Lb.SysUtils;

interface

{$i platform.inc}

uses
  System.Classes,
  System.SysUtils,
  System.math,
  System.Threading,
  System.DateUtils,
  System.SyncObjs,
  System.Generics.Collections,
  System.Generics.Defaults,
{$IFDEF QUIK}
{$IFDEF BYBIT}
  System.IniFiles,
  Lb.Bybit.SysUtils;
{$ELSE}
  System.IniFiles;
{$ENDIF}
{$ENDIF}

const
  ///<summary>
  /// �������� ����������
  ///</summary>
  VALUE_TRADE_RSI = 50;

{$IFDEF QUIK}
{$IFDEF BYBIT}
type
  ///<summary>
  /// ����������� �������� ��������
  ///</summary>
  TQBTypeSide = Lb.Bybit.SysUtils.TTypeSide;
{$ELSE}
type
  ///<summary>
  /// ����������� �������� ��������
  ///</summary>
  TQBTypeSide = (
    tsNull,
    tsBuy,
    tsSell
  );
{$ENDIF}
{$ENDIF}


type
  ///<summary>
  /// ���������
  ///</summary>
  TTypePlatform = (tpBybit = 0, tpQuik);

  ///<summary>��� ������</summary>
  TTypeTrade = (toLong, toShort);

  ///<summary>������� 3 �� ��������, 1 �� �������� </summary>
  TTypeLine = (tlOpen1, tlOpen2, tlOpen3, tlOpen4, tlClose);


  IMainApp = interface
    ///<summary>
    /// ������� �������� TabControl, ��������� Main
    ///</summary>
    procedure EventCloseTabControl;

    ///<summary>
    /// ������������ ������� �����
    ///</summary>
    procedure SetHeaderCaption(const ACaption: String);
  end;

  ///<summary>
  /// ������ ��������
  ///</summary>
  IOrderLog = interface
    procedure BeginOrder;
    procedure EndOrder;
  end;

  ///<summary>
  /// �������� ����������
  ///</summary>
  TParamApplication = class(TObject)
  private const
    SECTION_SYS  = 'sys';
    IDENT_HEIGHT = 'height';
    IDENT_WIDTH  = 'width';
  private
    FHeight: Integer;
    FWidth: Integer;
  protected
    FIniFile: TIniFile;
    function GetFileName: String;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Load;
    procedure Save;
    property Height: Integer read FHeight write FHeight;
    property Width: Integer read FWidth write FWidth;
  end;

  ///<summary>
  /// ��������� ������ ��������� � ���������
  ///</summary>
  TParamPlatform = class(TObject)
  private const
    SECTION_PARAM        = 'param';
{$IFDEF QUIK}
    IDENT_QUIK_TABLE_RSI = 'quik_table_rsi';
    IDENT_CLASS_CODE     = 'class_code';
    IDENT_SEC_CODE       = 'sec_code';
    IDENT_TRDACC_ID      = 'trdacc_id';
    IDENT_PATH_QUIK      = 'path_quik';
{$ENDIF}
{$IFDEF BYBIT}
    IDENT_SYMBLE         = 'symble';
    IDENT_API_KEY        = 'api_key';
    IDENT_API_SECRET     = 'api_secret';
    IDENT_CATEGORY       = 'category';
    IDENT_INTERVAL       = 'interval';
{$ENDIF}
    IDENT_IS_LOG_TRADE   = 'is_log_trade';
    IDENT_VIRTUAL        = 'is_virtual_checked';
    IDENT_PLATFORM       = 'platform';
    IDENT_IS_TREND       = 'is_trend';
    IDENT_IS_NEW_CANDEL  = 'is_new_candel';

    IDENT_TIME_BEGIN     = 'time_begin';
    IDENT_TIME_END       = 'time_end';
    IDENT_GLOBAL_TK      = 'global_tk';
    IDENT_GLOBAL_SL      = 'global_sl';
    IDENT_IS_LINIT_TIME  = 'is_limit_time';
  private const
    SECTION_PARAM_LIEN   = 'line_%d_%d';
    IDENT_ACTIVE         = 'active';
    IDENT_RE_ACTIVE      = 're_active';
    IDENT_ACTIVE_RSI     = 'active_rsi';
    IDENT_RE_ACTIVE_RSI  = 're_active_rsi';
    IDENT_QTY            = 'qty';
    IDENT_REVERS_QTY     = 'revers_qry';
  private
{$IFDEF QUIK}
    FQuikTableRSI: String;
    FClassCode: String;
    FSecCode: String;
    FTrdaccID: String;
    FPathQuik: String;
    FIsLogTrade: Boolean;
{$ENDIF}
    FTimeBegin: TDateTime;
    FTimeEnd: TDateTime;
    FGlobal_TK: Double;
    FGlobal_SL: Double;
    FIsLimitTime: Boolean;
{$IFDEF BYBIT}
    FSymble: String;
    FApiKey: String;
    FApiSecret: String;
    FCategory: TTypeCategory;
    FInterval: TTypeInterval;
{$ENDIF}
    FTypePlatform: TTypePlatform;
    FIsVirtualChecked: Boolean;
    FIsTrend: Boolean;
    FIsNewCandel: Boolean;
  private
    FHeight: Integer;
    FWidth: Integer;
  private
    function GetSectionParamLINE(ATrade: TTypeTrade; ALine: TTypeLine): String;

    function GetActive(ATrade: TTypeTrade; ALine: TTypeLine): Boolean;
    function GetActiveRSI(ATrade: TTypeTrade; ALine: TTypeLine): Double;
    function GetQty(ATrade: TTypeTrade; ALine: TTypeLine): Double;
    function GetReActive(ATrade: TTypeTrade; ALine: TTypeLine): Boolean;
    function GetReActiveRSI(ATrade: TTypeTrade; ALine: TTypeLine): Double;
    procedure SetActive(ATrade: TTypeTrade; ALine: TTypeLine; const Value: Boolean);
    procedure SetActiveRSI(ATrade: TTypeTrade; ALine: TTypeLine; const Value: Double);
    procedure SetQty(ATrade: TTypeTrade; ALine: TTypeLine; const Value: Double);
    procedure SetReActive(ATrade: TTypeTrade; ALine: TTypeLine; const Value: Boolean);
    procedure SetReActiveRSI(ATrade: TTypeTrade; ALine: TTypeLine; const Value: Double);
    function GetReversQty(ATrade: TTypeTrade; ALine: TTypeLine): Boolean;
    procedure SetReversQty(ATrade: TTypeTrade; ALine: TTypeLine; const Value: Boolean);
  protected
    FIniFile: TIniFile;
    function GetFileName: String;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Load;
    procedure Save;
  public
    property TypePlatform: TTypePlatform read FTypePlatform write FTypePlatform;
{$IFDEF BYBIT}
    ///<summary>
    /// ��������� ������
    ///</summary>
    property Symble: String read FSymble write FSymble;
    ///<summary>
    /// API ���� (��������)
    ///</summary>
    property ApiKey: String read FApiKey write FApiKey;
    ///<summary>
    /// API ������ (��������� ����)
    ///</summary>
    property ApiSecret: String read FApiSecret write FApiSecret;
    ///<summary>
    /// ��������� ����������� ���������� �� bybit: IDENT_CATEGORY
    ///</summary>
    property Category: TTypeCategory read FCategory write FCategory;
    ///<summary>
    /// �������� � ������ ����� �����������
    ///</summary>
    property Interval: TTypeInterval read FInterval write FInterval;
{$ENDIF}
{$IFDEF QUIK}
    property QuikTableRSI: String read FQuikTableRSI write FQuikTableRSI;
    property ClassCode: String read FClassCode write FClassCode;
    property SecCode: String read FSecCode write FSecCode;
    property TrdaccID: String read FTrdaccID write FTrdaccID;
    property PathQuik: String read FPathQuik write FPathQuik;
    property IsLogTrade: Boolean read FIsLogTrade write FIsLogTrade;

    property TimeBegin: TDateTime read FTimeBegin write FTimeBegin;
    property TimeEnd: TDateTime read FTimeEnd write FTimeEnd;
    property Global_TK: Double read FGlobal_TK write FGlobal_TK;
    property Global_SL: Double read FGlobal_SL write FGlobal_SL;
    property IsLimitTime: Boolean read FIsLimitTime write FIsLimitTime;

    property Height: Integer read FHeight write FHeight;
    property Width: Integer read FWidth write FWidth;
{$ENDIF}
    ///<summary>
    /// ��������� �������� �������� � � ����������� ����
    ///</summary>
    property IsVirtualChecked: Boolean read FIsVirtualChecked write FIsVirtualChecked;
    property IsTrend: Boolean read FIsTrend write FIsTrend;
    property IsNewCandel: Boolean read FIsNewCandel write FIsNewCandel;
  public
    property Active[ATrade: TTypeTrade; ALine: TTypeLine]: Boolean     read GetActive      write SetActive;
    property ReActive[ATrade: TTypeTrade; ALine: TTypeLine]: Boolean   read GetReActive    write SetReActive;
    property ActiveRSI[ATrade: TTypeTrade; ALine: TTypeLine]: Double   read GetActiveRSI   write SetActiveRSI;
    property ReActiveRSI[ATrade: TTypeTrade; ALine: TTypeLine]: Double read GetReActiveRSI write SetReActiveRSI;
    property Qty[ATrade: TTypeTrade; ALine: TTypeLine]: Double         read GetQty         write SetQty;
    property ReversQty[ATrade: TTypeTrade; ALine: TTypeLine]: Boolean  read GetReversQty   write SetReversQty;
  end;



  ///<summary>�������� ��������� �����, � ���� �� �������� �������</summary>
  TSituationParam = record
    FastRSI: Double;      // ������� RSI
    SlowRSI: Double;      // ��������� RSI
    Bid, Ask: Double;     // ������� ����
    Qty: Double;          // ������ �������
    Side: TQBTypeSide;    // ����������� �������
    IsNewCandel: Boolean; // ������� ����� �����
  public
    function ToString: String;
  end;

  ///<summary>�������� ������</sammry>
  TTradeParam = record
    Price: Double;
    Qty: Double;
    Side: TQBTypeSide;
    TypeTrade: TTypeTrade;
    TypeLine: TTypeLine;
  end;
  TOnEventSendTarde = procedure(Sender: TObject; ATradeParam: TTradeParam) of object;

function ParamPlatform: TParamPlatform;
function ParamApplication: TParamApplication;

function GetStrToTypeLine(ALine: TTypeLine): String;
function GetStrToTypeTrade(ATrade: TTypeTrade): String;
function GetStrToTypePlatform(APlatform: TTypePlatform): String;

{$IFDEF QUIK}
function GetStrToTypeSide(const ASide: TQBTypeSide): String;
{$ENDIF}

implementation

var
  localParamApplication: TParamApplication = nil;
  localParamPlatform: TParamPlatform = nil;

function ParamPlatform: TParamPlatform;
begin
  if not Assigned(localParamPlatform) then
  begin
    localParamPlatform := TParamPlatform.Create;
    localParamPlatform.Load;
  end;
  Result := localParamPlatform;
end;

function ParamApplication: TParamApplication;
begin
  if not Assigned(localParamApplication) then
  begin
    localParamApplication := TParamApplication.Create;
    localParamApplication.Load;
  end;
  Result := localParamApplication;
end;

function GetStrToTypeLine(ALine: TTypeLine): String;
begin
  case ALine of
    tlOpen1: Result := 'Open1';
    tlOpen2: Result := 'Open2';
    tlOpen3: Result := 'Open3';
    tlOpen4: Result := 'Open4';
    tlClose: Result := 'Close';
  else
    Result := 'not_type_line';
  end;
end;

function GetStrToTypeTrade(ATrade: TTypeTrade): String;
begin
  case ATrade of
    toLong : Result := 'long';
    toShort: Result := 'short';
  else
    Result := 'non_type_trade';
  end;
end;

function GetStrToTypePlatform(APlatform: TTypePlatform): String;
begin
  case APlatform of
    tpBybit: Result := 'bybit';
    tpQuik: Result := 'quik';
  else
    Result := 'non_type_platform';
  end;
end;


function IsCheckPlatforma: Boolean;
var
  xCnt: Integer;
begin
  // ��������� �� ����� ��������� �������� ���������
  xCnt := 0;
{$IFDEF QUIK}
  Inc(xCnt);
{$ENDIF}
{$IFDEF BYBIT}
  Inc(xCnt);
{$ENDIF}
  Result := xCnt = 1;
end;

function GetStrToTypeSide(const ASide: TQBTypeSide): String;
begin
  case ASide of
    tsBuy : Result := 'Buy';
    tsSell: Result := 'Sell';
  else
    Result := 'Nun';
  end;
end;

{ TParamApplication }

constructor TParamApplication.Create;
begin
  var xFN := GetFileName;
  FIniFile := TIniFile.Create(xFN);
end;

destructor TParamApplication.Destroy;
begin
  FreeAndNil(FIniFile);
  inherited;
end;

function TParamApplication.GetFileName: String;
begin
  var xFileName := ExtractFilePath(ParamStr(0)) + 'config.ini';
  Result := xFileName;
end;

procedure TParamApplication.Load;
begin
  FHeight := FIniFile.ReadInteger(SECTION_SYS,IDENT_HEIGHT,460);
  FWidth  := FIniFile.ReadInteger(SECTION_SYS,IDENT_WIDTH,740);
end;

procedure TParamApplication.Save;
begin
  FIniFile.WriteInteger(SECTION_SYS,IDENT_HEIGHT,FHeight);
  FIniFile.WriteInteger(SECTION_SYS,IDENT_WIDTH,FWidth);
  FIniFile.UpdateFile;
end;

{ TParamPlatform }

constructor TParamPlatform.Create;
begin
  FIsNewCandel := False;
  var xFN := GetFileName;
  FIniFile := TIniFile.Create(xFN);
end;

destructor TParamPlatform.Destroy;
begin
  FreeAndNil(FIniFile);
  inherited;
end;

function TParamPlatform.GetFileName: String;
var
  xFileName: String;
begin
  xFileName := ExtractFilePath(ParamStr(0)) + 'config.ini';
  Result := xFileName;
end;

procedure TParamPlatform.Load;
begin
  FTypePlatform := TTypePlatform(FIniFile.ReadInteger(SECTION_PARAM,IDENT_PLATFORM,0));
{$IFDEF QUIK}
  FQuikTableRSI := FIniFile.ReadString(SECTION_PARAM,IDENT_QUIK_TABLE_RSI,'');
  FClassCode    := FIniFile.ReadString(SECTION_PARAM,IDENT_CLASS_CODE,'');
  FSecCode      := FIniFile.ReadString(SECTION_PARAM,IDENT_SEC_CODE,'');
  FTrdaccID     := FIniFile.ReadString(SECTION_PARAM,IDENT_TRDACC_ID,'');
  FPathQuik     := FIniFile.ReadString(SECTION_PARAM,IDENT_PATH_QUIK,'');
{$ENDIF}
{$IFDEF BYBIT}
  FSymble       := FIniFile.ReadString(SECTION_PARAM,IDENT_SYMBLE,'');
  FApiKey       := FIniFile.ReadString(SECTION_PARAM,IDENT_API_KEY,'');
  FApiSecret    := FIniFile.ReadString(SECTION_PARAM,IDENT_API_SECRET,'');
  FCategory     := TTypeCategory(FIniFile.ReadInteger(SECTION_PARAM,IDENT_CATEGORY,1));
  FInterval     := TTypeInterval(FIniFile.ReadInteger(SECTION_PARAM,IDENT_INTERVAL,3));
{$ENDIF}
  FIsLogTrade       := FIniFile.ReadBool(SECTION_PARAM,IDENT_IS_LOG_TRADE,False);
  FIsVirtualChecked := FIniFile.ReadBool(SECTION_PARAM,IDENT_VIRTUAL,False);
  FIsTrend          := FIniFile.ReadBool(SECTION_PARAM,IDENT_IS_TREND,False);
  FIsNewCandel      := FIniFile.ReadBool(SECTION_PARAM,IDENT_IS_NEW_CANDEL,False);

  FTimeBegin    := FIniFile.ReadTime(SECTION_PARAM,IDENT_TIME_BEGIN,StrToTime('10:00:00'));
  FTimeEnd      := FIniFile.ReadTime(SECTION_PARAM,IDENT_TIME_END,  StrToTime('15:00:00'));
  FGlobal_TK    := FIniFile.ReadFloat(SECTION_PARAM,IDENT_GLOBAL_TK,0);
  FGlobal_SL    := FIniFile.ReadFloat(SECTION_PARAM,IDENT_GLOBAL_SL,0);
  FIsLimitTime  := FIniFile.ReadBool(SECTION_PARAM,IDENT_IS_LINIT_TIME,False);
end;

procedure TParamPlatform.Save;
begin
  FIniFile.WriteInteger(SECTION_PARAM,IDENT_PLATFORM,Integer(FTypePlatform));
{$IFDEF QUIK}
  FIniFile.WriteString(SECTION_PARAM,IDENT_QUIK_TABLE_RSI,FQuikTableRSI);
  FIniFile.WriteString(SECTION_PARAM,IDENT_CLASS_CODE,FClassCode);
  FIniFile.WriteString(SECTION_PARAM,IDENT_SEC_CODE,FSecCode);
  FIniFile.WriteString(SECTION_PARAM,IDENT_TRDACC_ID,FTrdaccID);
  FIniFile.WriteString(SECTION_PARAM,IDENT_PATH_QUIK,FPathQuik);
{$ENDIF}
{$IFDEF BYBIT}
  FIniFile.WriteString(SECTION_PARAM,IDENT_SYMBLE,FSymble);
  FIniFile.WriteString(SECTION_PARAM,IDENT_API_KEY,FApiKey);
  FIniFile.WriteString(SECTION_PARAM,IDENT_API_SECRET,FApiSecret);
  FIniFile.WriteInteger(SECTION_PARAM,IDENT_CATEGORY,Integer(FCategory));
  FIniFile.WriteInteger(SECTION_PARAM,IDENT_INTERVAL,Integer(FInterval));
{$ENDIF}
  FIniFile.WriteBool(SECTION_PARAM,IDENT_IS_LOG_TRADE,FIsLogTrade);
  FIniFile.WriteBool(SECTION_PARAM,IDENT_VIRTUAL,FIsVirtualChecked);
  FIniFile.WriteBool(SECTION_PARAM,IDENT_IS_TREND,FIsTrend);
  FIniFile.WriteBool(SECTION_PARAM,IDENT_IS_NEW_CANDEL,FIsNewCandel);

  FIniFile.WriteTime(SECTION_PARAM,IDENT_TIME_BEGIN,FTimeBegin);
  FIniFile.WriteTime(SECTION_PARAM,IDENT_TIME_END,FTimeEnd);
  FIniFile.WriteFloat(SECTION_PARAM,IDENT_GLOBAL_TK,FGlobal_TK);
  FIniFile.WriteFloat(SECTION_PARAM,IDENT_GLOBAL_SL,FGlobal_SL);
  FIniFile.WriteBool(SECTION_PARAM,IDENT_IS_LINIT_TIME,FIsLimitTime);

  FIniFile.UpdateFile;
end;

function TParamPlatform.GetSectionParamLINE(ATrade: TTypeTrade; ALine: TTypeLine): String;
begin
  Result := Format(SECTION_PARAM_LIEN,[Integer(ATrade),Integer(ALine)]);
end;

function TParamPlatform.GetActive(ATrade: TTypeTrade; ALine: TTypeLine): Boolean;
var
  xS: String;
begin
  xS := GetSectionParamLINE(ATrade,ALine);
  Result := FIniFile.ReadBool(xS,IDENT_ACTIVE,False);
end;

function TParamPlatform.GetActiveRSI(ATrade: TTypeTrade; ALine: TTypeLine): Double;
var
  xS: String;
begin
  xS := GetSectionParamLINE(ATrade,ALine);
  Result := FIniFile.ReadFloat(xS,IDENT_ACTIVE_RSI,0);
end;

function TParamPlatform.GetQty(ATrade: TTypeTrade; ALine: TTypeLine): Double;
var
  xS: String;
begin
  xS := GetSectionParamLINE(ATrade,ALine);
  Result := FIniFile.ReadFloat(xS,IDENT_QTY,0);
end;

function TParamPlatform.GetReActive(ATrade: TTypeTrade; ALine: TTypeLine): Boolean;
begin
  var xS := GetSectionParamLINE(ATrade,ALine);
  Result := FIniFile.ReadBool(xS,IDENT_RE_ACTIVE,False);
end;

function TParamPlatform.GetReActiveRSI(ATrade: TTypeTrade; ALine: TTypeLine): Double;
begin
  var xS := GetSectionParamLINE(ATrade,ALine);
  Result := FIniFile.ReadFloat(xS,IDENT_RE_ACTIVE_RSI,0);
end;


procedure TParamPlatform.SetActive(ATrade: TTypeTrade; ALine: TTypeLine; const Value: Boolean);
begin
  var xS := GetSectionParamLINE(ATrade,ALine);
  FIniFile.WriteBool(xS,IDENT_ACTIVE,Value);
  FIniFile.UpdateFile;
end;

procedure TParamPlatform.SetActiveRSI(ATrade: TTypeTrade; ALine: TTypeLine; const Value: Double);
begin
  var xS := GetSectionParamLINE(ATrade,ALine);
  FIniFile.WriteFloat(xS,IDENT_ACTIVE_RSI,Value);
  FIniFile.UpdateFile;
end;

procedure TParamPlatform.SetQty(ATrade: TTypeTrade; ALine: TTypeLine; const Value: Double);
begin
  var xS := GetSectionParamLINE(ATrade,ALine);
  FIniFile.WriteFloat(xS,IDENT_QTY,Value);
  FIniFile.UpdateFile;
end;

procedure TParamPlatform.SetReActive(ATrade: TTypeTrade; ALine: TTypeLine; const Value: Boolean);
begin
  var xS := GetSectionParamLINE(ATrade,ALine);
  FIniFile.WriteBool(xS,IDENT_RE_ACTIVE,Value);
  FIniFile.UpdateFile;
end;

procedure TParamPlatform.SetReActiveRSI(ATrade: TTypeTrade; ALine: TTypeLine; const Value: Double);
begin
  var xS := GetSectionParamLINE(ATrade,ALine);
  FIniFile.WriteFloat(xS,IDENT_RE_ACTIVE_RSI,Value);
  FIniFile.UpdateFile;
end;

function TParamPlatform.GetReversQty(ATrade: TTypeTrade; ALine: TTypeLine): Boolean;
begin
  var xS := GetSectionParamLINE(ATrade,ALine);
  Result := FIniFile.ReadBool(xS,IDENT_REVERS_QTY,False);
end;

procedure TParamPlatform.SetReversQty(ATrade: TTypeTrade; ALine: TTypeLine; const Value: Boolean);
begin
  var xS := GetSectionParamLINE(ATrade,ALine);
  FIniFile.WriteBool(xS,IDENT_REVERS_QTY,Value);
  FIniFile.UpdateFile;
end;

{ TSituationParam }

function TSituationParam.ToString: String;
var
  xS: String;
begin
  xS :=
    FastRSI.ToString + '/' +
    SlowRSI.ToString + '/' +
    Bid.ToString + '/' +
    Ask.ToString + '/' +
    GetStrToTypeSide(Side) + '/';
  if IsNewCandel then
    xS := xS + 'true'
  else
    xS := xS + 'false';
  Result := xS;
end;

initialization

finalization
  if Assigned(localParamPlatform) then
    FreeAndNil(localParamPlatform);

  if Assigned(localParamPlatform) then
    FreeAndNil(localParamPlatform);

end.
