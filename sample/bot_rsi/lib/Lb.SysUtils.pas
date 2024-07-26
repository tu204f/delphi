unit Lb.SysUtils;

interface

uses
  System.Classes,
  System.SysUtils,
  System.math,
  System.Threading,
  System.DateUtils,
  System.SyncObjs,
  System.Generics.Collections,
  System.Generics.Defaults,
  System.IniFiles,
  Lb.Bybit.SysUtils;


type
  ///<summary>Тип ордера</summary>
  TTypeTrade = (toLong, toShort);

  ///<summary>Уровень 3 на открытие, 1 на закртиые </summary>
  TTypeLine = (tlOpen1, tlOpen2, tlOpen3, tlOpen4, tlClose);


  IMainApp = interface
    ///<summary>
    /// Событие закрытие TabControl, открываем Main
    ///</summary>
    procedure EventCloseTabControl;
  end;

  ///<summary>
  /// Параметры работы программы
  ///</summary>
  TParamApplication = class(TObject)
  public const
    SECTION_PARAM    = 'param';

    IDENT_SYMBLE     = 'symble';
    IDENT_API_KEY    = 'api_key';
    IDENT_API_SECRET = 'api_secret';
    IDENT_CATEGORY   = 'category';
    IDENT_INTERVAL   = 'interval';

    SECTION_PARAM_LIEN = 'line_%d_%d';

    IDENT_ACTIVE        = 'active';
    IDENT_RE_ACTIVE     = 're_active';
    IDENT_ACTIVE_RSI    = 'active_rsi';
    IDENT_RE_ACTIVE_RSI = 're_active_rsi';
    IDENT_QTY           = 'qty';

  private
    FSymble: String;
    FApiKey: String;
    FApiSecret: String;
    FCategory: TTypeCategory;
    FInterval: TTypeInterval;
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
  protected
    FIniFile: TIniFile;
    function GetFileName: String;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Load;
    procedure Save;
  public
    ///<summary>
    /// Торгуемый символ
    ///</summary>
    property Symble: String read FSymble write FSymble;
    ///<summary>
    /// API Ключ (пуличный)
    ///</summary>
    property ApiKey: String read FApiKey write FApiKey;
    ///<summary>
    /// API секрет (секретный ключ)
    ///</summary>
    property ApiSecret: String read FApiSecret write FApiSecret;
    ///<summary>
    /// Категория инструмента торгуемого на bybit: IDENT_CATEGORY
    ///</summary>
    property Category: TTypeCategory read FCategory write FCategory;
    ///<summary>
    /// Интервал — размер свечи загружаемой
    ///</summary>
    property Interval: TTypeInterval read FInterval write FInterval;
  public
    property Active[ATrade: TTypeTrade; ALine: TTypeLine]: Boolean     read GetActive      write SetActive;
    property ReActive[ATrade: TTypeTrade; ALine: TTypeLine]: Boolean   read GetReActive    write SetReActive;
    property ActiveRSI[ATrade: TTypeTrade; ALine: TTypeLine]: Double   read GetActiveRSI   write SetActiveRSI;
    property ReActiveRSI[ATrade: TTypeTrade; ALine: TTypeLine]: Double read GetReActiveRSI write SetReActiveRSI;
    property Qty[ATrade: TTypeTrade; ALine: TTypeLine]: Double         read GetQty         write SetQty;
  end;

  ///<summary>Параметр состояние рынка, и есть ли открытая позиция</summary>
  TSituationParam = record
    ValueRSI: Double;  // Состояние рынка
    Bid, Ask: Double;  // Лучьшие цены
    Qty: Double;       // Отрыта позиция
    Side: TTypeSide;   // Напровления позиции
  end;

  ///<summary>Параметр сделки</sammry>
  TTradeParam = record
    Price: Double;
    Qty: Double;
    Side: TTypeSide;
    TypeTrade: TTypeTrade;
    TypeLine: TTypeLine;
  end;
  TOnEventSendTarde = procedure(Sender: TObject; ATradeParam: TTradeParam) of object;

function ParamApplication: TParamApplication;
function GetStrToTypeLine(ALine: TTypeLine): String;


implementation

var
  localParamApplication: TParamApplication = nil;


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
  Result := '';
  case ALine of
    tlOpen1: Result := 'Open1';
    tlOpen2: Result := 'Open2';
    tlOpen3: Result := 'Open3';
    tlOpen4: Result := 'Open4';
    tlClose: Result := 'Close';
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
var
  xFileName: String;
begin
  xFileName := ExtractFilePath(ParamStr(0)) + 'config.ini';
  Result := xFileName;
end;

procedure TParamApplication.Load;
begin
  FSymble    := FIniFile.ReadString(SECTION_PARAM,IDENT_SYMBLE,'');
  FApiKey    := FIniFile.ReadString(SECTION_PARAM,IDENT_API_KEY,'');
  FApiSecret := FIniFile.ReadString(SECTION_PARAM,IDENT_API_SECRET,'');
  FCategory  := TTypeCategory(FIniFile.ReadInteger(SECTION_PARAM,IDENT_CATEGORY,1));
  FInterval  := TTypeInterval(FIniFile.ReadInteger(SECTION_PARAM,IDENT_INTERVAL,1));
end;

procedure TParamApplication.Save;
begin
  FIniFile.WriteString(SECTION_PARAM,IDENT_SYMBLE,FSymble);
  FIniFile.WriteString(SECTION_PARAM,IDENT_API_KEY,FApiKey);
  FIniFile.WriteString(SECTION_PARAM,IDENT_API_SECRET,FApiSecret);
  FIniFile.WriteInteger(SECTION_PARAM,IDENT_CATEGORY,Integer(FCategory));
  FIniFile.WriteInteger(SECTION_PARAM,IDENT_INTERVAL,Integer(FInterval));
  FIniFile.UpdateFile;
end;

function TParamApplication.GetSectionParamLINE(ATrade: TTypeTrade; ALine: TTypeLine): String;
begin
  Result := Format(SECTION_PARAM_LIEN,[Integer(ATrade),Integer(ALine)]);
end;

function TParamApplication.GetActive(ATrade: TTypeTrade; ALine: TTypeLine): Boolean;
var
  xS: String;
begin
  xS := GetSectionParamLINE(ATrade,ALine);
  Result := FIniFile.ReadBool(xS,IDENT_ACTIVE,False);
end;

function TParamApplication.GetActiveRSI(ATrade: TTypeTrade; ALine: TTypeLine): Double;
var
  xS: String;
begin
  xS := GetSectionParamLINE(ATrade,ALine);
  Result := FIniFile.ReadFloat(xS,IDENT_ACTIVE_RSI,0);
end;

function TParamApplication.GetQty(ATrade: TTypeTrade; ALine: TTypeLine): Double;
var
  xS: String;
begin
  xS := GetSectionParamLINE(ATrade,ALine);
  Result := FIniFile.ReadFloat(xS,IDENT_QTY,0);
end;

function TParamApplication.GetReActive(ATrade: TTypeTrade; ALine: TTypeLine): Boolean;
begin
  var xS := GetSectionParamLINE(ATrade,ALine);
  Result := FIniFile.ReadBool(xS,IDENT_RE_ACTIVE,False);
end;

function TParamApplication.GetReActiveRSI(ATrade: TTypeTrade; ALine: TTypeLine): Double;
begin
  var xS := GetSectionParamLINE(ATrade,ALine);
  Result := FIniFile.ReadFloat(xS,IDENT_RE_ACTIVE_RSI,0);
end;


procedure TParamApplication.SetActive(ATrade: TTypeTrade; ALine: TTypeLine; const Value: Boolean);
begin
  var xS := GetSectionParamLINE(ATrade,ALine);
  FIniFile.WriteBool(xS,IDENT_ACTIVE,Value);
  FIniFile.UpdateFile;
end;

procedure TParamApplication.SetActiveRSI(ATrade: TTypeTrade; ALine: TTypeLine; const Value: Double);
begin
  var xS := GetSectionParamLINE(ATrade,ALine);
  FIniFile.WriteFloat(xS,IDENT_ACTIVE_RSI,Value);
  FIniFile.UpdateFile;
end;

procedure TParamApplication.SetQty(ATrade: TTypeTrade; ALine: TTypeLine; const Value: Double);
begin
  var xS := GetSectionParamLINE(ATrade,ALine);
  FIniFile.WriteFloat(xS,IDENT_QTY,Value);
  FIniFile.UpdateFile;
end;

procedure TParamApplication.SetReActive(ATrade: TTypeTrade; ALine: TTypeLine; const Value: Boolean);
begin
  var xS := GetSectionParamLINE(ATrade,ALine);
  FIniFile.WriteBool(xS,IDENT_RE_ACTIVE,Value);
  FIniFile.UpdateFile;
end;

procedure TParamApplication.SetReActiveRSI(ATrade: TTypeTrade; ALine: TTypeLine; const Value: Double);
begin
  var xS := GetSectionParamLINE(ATrade,ALine);
  FIniFile.WriteFloat(xS,IDENT_RE_ACTIVE_RSI,Value);
  FIniFile.UpdateFile;
end;

initialization

finalization
  if Assigned(localParamApplication) then
    FreeAndNil(localParamApplication);

end.
