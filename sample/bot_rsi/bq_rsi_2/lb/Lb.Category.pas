unit Lb.Category;

interface

{$I debug.inc}

uses
  System.Classes,
  System.SysUtils,
  System.math,
  System.Threading,
  System.DateUtils,
  System.SyncObjs,
  System.Generics.Collections,
  Lb.SysUtils,
  Lb.Level;

type
  ///<summary>
  /// Тип уровня верхний или нижний
  ///</summary>
  TTypeTradeLevel = (
    tlNull,   // Нейтральное состояние
    tlTop,    // верхний
    tlBottom  // нижний
  );

  ///<summary>
  /// Событие пересечение уровня
  ///</summary>
  TEventOnSideLevel = procedure(
    ASender: TObject;
    ATypeTradeLevel: TTypeTradeLevel
  ) of object;

  ///<summary>
  /// Торгуются две пары
  ///</summary>
  TTradeLevel = class(TObject)
  private
    FOnSideLevel: TEventOnSideLevel;
    FTopLevel: TOneEventLevel;
    FBottomLevel: TOneEventLevel;
    function GetValueBottom: Double;
    function GetValueTop: Double;
    procedure SetValueBottom(const Value: Double);
    procedure SetValueTop(const Value: Double);
  protected
    procedure DoSideLevel(const ATypeTradeLevel: TTypeTradeLevel); virtual;
    procedure TopLevelOnIntersection(Sender: TObject);
    procedure BottomLevelOnIntersection(Sender: TObject);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure SetUpDateValue(const AValueRSI: Double);
    ///<summary>Значение верхний границы</summary>
    property ValueTop: Double read GetValueTop write SetValueTop;
    ///<summary>Заначенеи нижний границы</summary>
    property ValueBottom: Double read GetValueBottom write SetValueBottom;
    ///<summary>Событие пересечение уровня</summary>
    property OnSideLevel: TEventOnSideLevel write FOnSideLevel;
  end;

type
  TTradeBox = class;

  ///<summary>Состояние сделки</summary>
  TTypeTrade = (
    ttNull,
    ttOpen,
    ttClose
  );

  ///<summary>Напровление сделки</summary>
  TTypeDirection = (
    tdNull,
    tdLong,
    tdShort
  );

  ///<summary>Событие коробки</summary>
  TEventTradeBox = procedure(
    ASender: TObject;
    ATypeDirection: TTypeDirection;
    ATypeTrade: TTypeTrade
  ) of object;

  ///<summary>Блок подает сигналы в длинную</summary>
  TLongTradeLevel = class(TTradeLevel)
  private
    FTradeBox: TTradeBox;
  protected
    procedure DoSideLevel(const ATypeTradeLevel: TTypeTradeLevel); override;
    property TradeBox: TTradeBox write FTradeBox;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

  ///<summary>Блок подает сигналы в короткую</summary>
  TShortTradeLevel = class(TTradeLevel)
  private
    FTradeBox: TTradeBox;
  protected
    procedure DoSideLevel(const ATypeTradeLevel: TTypeTradeLevel); override;
    property TradeBox: TTradeBox write FTradeBox;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

  ///<summary>Коробка</summary>
  TTradeBox = class(TObject)
  private
    FEventTradeBox: TEventTradeBox;
  private
    FDirection: TTypeDirection;
    FTypeTrade: TTypeTrade;
    FLongTradeLevel: TLongTradeLevel;
    FShortTradeLevel: TShortTradeLevel;
    function GetOpenLong: Double;
    function GetOpenShort: Double;
    function GetCloseLong: Double;
    function GetCloseShort: Double;
    procedure SetOpenLong(const Value: Double);
    procedure SetOpenShort(const Value: Double);
    procedure SetCloseLong(const Value: Double);
    procedure SetCloseShort(const Value: Double);
  protected
    procedure DoTrade(const ADirection: TTypeDirection; const ATypeTrade: TTypeTrade);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    ///<summary>Передаем значение опорного индикатора</summary>
    procedure SetUpDateValue(const AValueRSI: Double);
    ///<summary>Открываем длиную позицию</summary>
    property OpenLong: Double read GetOpenLong write SetOpenLong;
    ///<summary>Закрываем длиную позцию</summary>
    property CloseLong: Double read GetCloseLong write SetCloseLong;
    ///<summary>Открываем короткую позцию</summary>
    property OpenShort: Double read GetOpenShort write SetOpenShort;
    ///<summary>Закрываем короткую позциию</summary>
    property CloseShort: Double read GetCloseShort write SetCloseShort;
    property OnTradeBox: TEventTradeBox write FEventTradeBox;
  end;

function GetStrToTypeTradeLevel(ATypeTradeLevel: TTypeTradeLevel): String;

function GetStrToTypeDirection(const ADirection: TTypeDirection): String;
function GetStrToTypeTrade(const ATypeTrade: TTypeTrade): String;

implementation

{$IFDEF DEBUG}
uses
  Lb.Logger;
{$ENDIF}

function GetStrToTypeTradeLevel(ATypeTradeLevel: TTypeTradeLevel): String;
begin
  case ATypeTradeLevel of
    tlTop: Result := 'top';
    tlBottom: Result := 'bottom';
  else
    Result := 'null';
  end;
end;

function GetStrToTypeDirection(const ADirection: TTypeDirection): String;
begin
  case ADirection of
    tdLong: Result := 'Long';
    tdShort: Result := 'Short';
  else
    Result := 'non';
  end;
end;

function GetStrToTypeTrade(const ATypeTrade: TTypeTrade): String;
begin
  case ATypeTrade of
    ttOpen: Result := 'open';
    ttClose: Result := 'close';
  else
    Result := 'non';
  end;
end;

{ TTradeLevel }

constructor TTradeLevel.Create;
begin
  FTopLevel := TOneEventLevel.Create;
  FTopLevel.OnIntersectionLevel := TopLevelOnIntersection;
  FTopLevel.WorkLevel := TIntersectionLevel.tlUpDown;
  FTopLevel.IsRepeat := True;
  {todo: нужно переделать, что работа все подругому}
  FTopLevel.RepeatCount := 65000;

  FBottomLevel := TOneEventLevel.Create;
  FBottomLevel.OnIntersectionLevel := BottomLevelOnIntersection;
  FBottomLevel.WorkLevel := TIntersectionLevel.tlDownUp;
  FBottomLevel.IsRepeat := True;
  FBottomLevel.RepeatCount := 65000;
end;

destructor TTradeLevel.Destroy;
begin
  FreeAndNil(FBottomLevel);
  FreeAndNil(FTopLevel);
  inherited;
end;

procedure TTradeLevel.DoSideLevel(const ATypeTradeLevel: TTypeTradeLevel);
begin
  if Assigned(FOnSideLevel) then
    FOnSideLevel(Self, ATypeTradeLevel);
end;

procedure TTradeLevel.TopLevelOnIntersection(Sender: TObject);
begin
  DoSideLevel(TTypeTradeLevel.tlTop);
  FBottomLevel.WorkLevel := TIntersectionLevel.tlDownUp;
end;

procedure TTradeLevel.BottomLevelOnIntersection(Sender: TObject);
begin
  DoSideLevel(TTypeTradeLevel.tlBottom);
  FTopLevel.WorkLevel := TIntersectionLevel.tlUpDown;
end;

procedure TTradeLevel.SetUpDateValue(const AValueRSI: Double);
begin
  FTopLevel.SetUpDate(AValueRSI);
  FBottomLevel.SetUpDate(AValueRSI);
end;

function TTradeLevel.GetValueBottom: Double;
begin
  Result := FBottomLevel.Value;
end;

function TTradeLevel.GetValueTop: Double;
begin
  Result := FTopLevel.Value;
end;

procedure TTradeLevel.SetValueBottom(const Value: Double);
begin
  FBottomLevel.Value := Value;
end;

procedure TTradeLevel.SetValueTop(const Value: Double);
begin
  FTopLevel.Value := Value;
end;

{ TLongTradeLevel }

constructor TLongTradeLevel.Create;
begin
  inherited;
  FTradeBox := nil;
end;

destructor TLongTradeLevel.Destroy;
begin

  inherited;
end;

procedure TLongTradeLevel.DoSideLevel(const ATypeTradeLevel: TTypeTradeLevel);
begin
  inherited;
  if Assigned(FTradeBox) then
  begin
    case ATypeTradeLevel of
      tlTop: FTradeBox.DoTrade(TTypeDirection.tdLong,TTypeTrade.ttClose);
      tlBottom: FTradeBox.DoTrade(TTypeDirection.tdLong,TTypeTrade.ttOpen);
    else
      raise Exception.Create('TLongTradeLevel.DoSideLevel: Типа уровня не определен');
    end;
  end;
end;

{ TShortTradeLevel }

constructor TShortTradeLevel.Create;
begin
  inherited;
  FTradeBox := nil;
end;

destructor TShortTradeLevel.Destroy;
begin

  inherited;
end;

procedure TShortTradeLevel.DoSideLevel(const ATypeTradeLevel: TTypeTradeLevel);
begin
  inherited;
  if Assigned(FTradeBox) then
  begin
    case ATypeTradeLevel of
      tlTop: FTradeBox.DoTrade(TTypeDirection.tdShort,TTypeTrade.ttOpen);
      tlBottom: FTradeBox.DoTrade(TTypeDirection.tdShort,TTypeTrade.ttClose);
    else
      raise Exception.Create('TShortTradeLevel.DoSideLevel: Типа уровня не определен');
    end;
  end;
end;

{ TTradeBox }

constructor TTradeBox.Create;
begin
  FDirection := TTypeDirection.tdNull;
  FTypeTrade := TTypeTrade.ttNull;

  FLongTradeLevel := TLongTradeLevel.Create;
  FLongTradeLevel.TradeBox := Self;

  FShortTradeLevel := TShortTradeLevel.Create;
  FShortTradeLevel.TradeBox := Self;
end;

destructor TTradeBox.Destroy;
begin
  FreeAndNil(FShortTradeLevel);
  FreeAndNil(FLongTradeLevel);
  inherited;
end;

procedure TTradeBox.DoTrade(const ADirection: TTypeDirection; const ATypeTrade: TTypeTrade);
begin
  if Assigned(FEventTradeBox) then
    FEventTradeBox(Self,ADirection,ATypeTrade);

  FDirection := ADirection;
  FTypeTrade := ATypeTrade;
end;

procedure TTradeBox.SetUpDateValue(const AValueRSI: Double);
begin
  FLongTradeLevel.SetUpDateValue(AValueRSI);
  FShortTradeLevel.SetUpDateValue(AValueRSI);
end;

function TTradeBox.GetOpenLong: Double;
begin
  Result := FLongTradeLevel.ValueBottom;
end;

function TTradeBox.GetCloseLong: Double;
begin
  Result := FLongTradeLevel.ValueTop;
end;

function TTradeBox.GetOpenShort: Double;
begin
  Result := FShortTradeLevel.ValueTop;
end;

function TTradeBox.GetCloseShort: Double;
begin
  Result := FShortTradeLevel.ValueBottom;
end;

procedure TTradeBox.SetOpenLong(const Value: Double);
begin
  FLongTradeLevel.ValueBottom := Value;
end;

procedure TTradeBox.SetCloseLong(const Value: Double);
begin
  FLongTradeLevel.ValueTop := Value;
end;

procedure TTradeBox.SetOpenShort(const Value: Double);
begin
  FShortTradeLevel.ValueTop := Value;
end;

procedure TTradeBox.SetCloseShort(const Value: Double);
begin
  FShortTradeLevel.ValueBottom := Value;
end;

end.
